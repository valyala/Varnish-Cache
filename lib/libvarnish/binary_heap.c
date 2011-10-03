/*-
 * Copyright (c) 2006 Verdens Gang AS
 * Copyright (c) 2006-2011 Varnish Software AS
 * All rights reserved.
 *
 * Author: Poul-Henning Kamp <phk@phk.freebsd.dk>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * Implementation of a binary heap API
 *
 * See also:
 *	http://portal.acm.org/citation.cfm?doid=1785414.1785434
 *	(or: http://queue.acm.org/detail.cfm?id=1814327)
 */

#include "config.h"

#include <unistd.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>

#include "binary_heap.h"
#include "libvarnish.h"
#include "miniobj.h"

/* Parameters --------------------------------------------------------*/

/*
 * The number of elements in a row has to be a compromise between
 * wasted space and number of memory allocations.
 * With 64k objects per row, there will be at least 5...10 seconds
 * between row additions on a very busy server.
 * At the same time, the worst case amount of wasted memory is kept
 * at a reasonable 1 MB -- two rows on 64bit system.
 * Finally, but without practical significance: 16 bits should be
 * easier for the compiler to optimize.
 */
#define ROW_SHIFT		16u
#define ROW_WIDTH		(1u << ROW_SHIFT)

/*lint -emacro(572, ROW) shift 0 >> by 16 */
/*lint -emacro(835, ROW) 0 left of >> */
/*lint -emacro(778, ROW) const >> evaluates to zero */
#define ROW(bh, n)		((bh)->rows[(n) >> ROW_SHIFT])

/*lint -emacro(835, A) 0 left of & */
#define A(bh, n)		ROW(bh, n)[(n) & (ROW_WIDTH - 1)]

#define R_IDX(page_shift)       ((1u << (page_shift)) - 1)
#define ROOT_IDX(bh)            R_IDX((bh)->page_shift)


struct binheap {
        unsigned                magic;
#define BINHEAP_MAGIC           0x8bd801f0u     /* from /dev/random */
        void                    *priv;
        binheap_cmp_t           *cmp;
        binheap_update_t        *update;
        void                    ***rows;
	void			**rootp;	/* fast access to root */
        unsigned                next;
        unsigned                rows_count;
        unsigned                length;
        unsigned                page_shift;
};

static unsigned
parent(unsigned page_shift, unsigned u)
{
	unsigned v, page_mask, page_size, page_children;

	assert(page_shift > 0);
	assert(page_shift <= ROW_SHIFT);
	page_mask = R_IDX(page_shift);
	AZ(page_mask & (page_mask + 1));
	assert(u >= page_mask);
	if (u == page_mask)
		return u;	/* there is no parent for root */
	if (u <= page_mask + 2)
		return page_mask;
        v = u & page_mask;
	if (v > 1)
		return u - v + v / 2 - 1;
        page_size = page_mask + 1;
        page_children = page_size / 2 + 1;
	v = (u >> page_shift) - 2;
	u = v / page_children + 2;
	return u * page_size + (v % page_children) - page_children;
}

static unsigned
child(unsigned page_shift, unsigned u)
{
	unsigned v, page_mask, page_size, page_children;

	assert(page_shift > 0);
	assert(page_shift <= ROW_SHIFT);
	page_mask = R_IDX(page_shift);
	AZ(page_mask & (page_mask + 1));
	assert(u >= page_mask);
	v = (u & page_mask) + 2;
        page_size = page_mask + 1;
        page_children = page_size / 2 + 1;
	if (v < page_children)
		return u + v;
	v += (u >> page_shift) * page_children - page_size;
	if (v > (UINT_MAX >> page_shift))
		return u;	/* child index is overflown */
	return page_size * v;
}

static void **
alloc_row(unsigned page_shift)
{
        void *p, **row;
	size_t item_size, alignment;
	unsigned u;
	int rv;

	assert(page_shift > 0);
	assert(page_shift <= ROW_SHIFT);
	item_size = sizeof(*row);
        assert((1u << page_shift) < UINT_MAX / item_size);
	alignment = (1u << page_shift) * item_size;
	assert(ROW_WIDTH < SIZE_MAX / item_size);
        p = NULL;
	rv = posix_memalign(&p, alignment, ROW_WIDTH * item_size);
	XXXAZ(rv);
        AN(p);
	row = p;
        AZ(((uintptr_t) row) & (alignment - 1));
	/* null out items */
	for (u = 0; u < ROW_WIDTH; u++)
		row[u] = NULL;
	return row;
}

struct binheap *
binheap_new(void *priv, binheap_cmp_t *cmp_f, binheap_update_t *update_f)
{
	void ***rows;
        struct binheap *bh;
        unsigned page_size, page_shift;

        AN(cmp_f);
        AN(update_f);
        page_size = ((unsigned) getpagesize()) / sizeof(**rows);
	xxxassert(page_size * sizeof(**rows) == getpagesize());
        page_shift = 0u - 1;
        while (page_size) {
                page_size >>= 1;
                ++page_shift;
        }
        xxxassert(page_shift > 0);
	page_size = 1u << page_shift;
        xxxassert(page_size <= ROW_WIDTH);
	XXXAZ(ROW_WIDTH % page_size);

        rows = calloc(sizeof(*rows), 1);
        XXXAN(rows);
        AZ(rows[0]);

        /* allocate the first row and embed binheap structure into it */
	rows[0] = alloc_row(page_shift);
	AN(rows[0]);
        bh = (struct binheap *) rows[0];
	bh->magic = BINHEAP_MAGIC;
	bh->priv = priv;
        bh->cmp = cmp_f;
        bh->update = update_f;
        bh->rows = rows;
	bh->rootp = rows[0] + R_IDX(page_shift);
        bh->next = R_IDX(page_shift);
        bh->rows_count = 1;
	bh->length = ROW_WIDTH;
	bh->page_shift = page_shift;
	/* make sure the row with embedded binheap has free space for root pointer */
        xxxassert(sizeof(*bh) <= sizeof(**rows) * R_IDX(page_shift));
	assert(bh->rootp == &A(bh, R_IDX(page_shift)));
        return (bh);
}

static void
assign(const struct binheap *bh, void *p, unsigned u)
{
	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(bh->update);
	AN(p);
	assert(u != BINHEAP_NOIDX);
	A(bh, u) = p;
        bh->update(bh->priv, p, u);
}

static unsigned
trickleup(struct binheap *bh, void *p1, unsigned u)
{
	void *p2;
        unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
        assert(bh->page_shift > 0);
        assert(bh->page_shift <= ROW_SHIFT);
	assert(u >= ROOT_IDX(bh));
        assert(u < bh->next);
        AN(p1);

        while (u != ROOT_IDX(bh)) {
                v = parent(bh->page_shift, u);
                assert(v < u);
		assert(v >= ROOT_IDX(bh));
		p2 = A(bh, v);
                AN(p2);
                if (bh->cmp(bh->priv, p2, p1))
                        break;	/* parent is smaller than the child */
		assign(bh, p2, u);
                u = v;
        }
        return (u);
}

static unsigned
trickledown(struct binheap *bh, void *p1, unsigned u)
{
	void *p2, *p3;
	unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
        assert(bh->page_shift > 0);
        assert(bh->page_shift <= ROW_SHIFT);
	assert(u >= ROOT_IDX(bh));
        assert(u < bh->next);
	AN(p1);

        while (1) {
                v = child(bh->page_shift, u);
		assert(v > ROOT_IDX(bh));
                assert(v >= u);
		if (v == u)
			break;		/* index overflow */
                if (v >= bh->next)
                        break;		/* reached the end of heap */
		p2 = A(bh, v);
		AN(p2);
                if (v + 1 < bh->next) {
			p3 = A(bh, v + 1);
			AN(p3);
                        if (bh->cmp(bh->priv, p3, p2)) {
                                ++v;
				p2 = p3;
			}
                }
                assert(v < bh->next);
                if (bh->cmp(bh->priv, p1, p2))
                        break;	/* parent is smaller than children */
		assign(bh, p2, u);
                u = v;
        }
	return (u);
}

static void
add_row(struct binheap *bh)
{
        unsigned rows_count;

        CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
        AN(bh->rows);
        assert(bh->rows_count > 0);
	assert(&ROW(bh, bh->length) <= bh->rows + bh->rows_count);
        /* First make sure we have space for another row */
        if (&ROW(bh, bh->length) == bh->rows + bh->rows_count) {
                rows_count = bh->rows_count * 2;
                bh->rows = realloc(bh->rows, sizeof(*bh->rows) * rows_count);
                XXXAN(bh->rows);

                /* NULL out new pointers */
                while (bh->rows_count < rows_count)
                        bh->rows[bh->rows_count++] = NULL;
        }
	AZ(ROW(bh, bh->length));
	ROW(bh, bh->length) = alloc_row(bh->page_shift);
	AN(ROW(bh, bh->length));
	/* prevent from silent heap overflow */
	xxxassert(bh->length <= UINT_MAX - ROW_WIDTH);
        bh->length += ROW_WIDTH;
}

void
binheap_insert(struct binheap *bh, void *p)
{
        unsigned u, v;

	AN(p);
	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(bh->next >= ROOT_IDX(bh));
        assert(bh->length >= bh->next);
        if (bh->length == bh->next)
        	add_row(bh);
        assert(bh->length > bh->next);
	assert(bh->next < UINT_MAX);
        u = bh->next++;
	AZ(A(bh, u));
        v = trickleup(bh, p, u);
	assert(v <= u);
	assert(v >= ROOT_IDX(bh));
	assign(bh, p, v);
}

static unsigned
reorder(struct binheap *bh, void *p, unsigned u)
{
        unsigned v;

        CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
        assert(bh->next >= ROOT_IDX(bh));
        assert(u >= ROOT_IDX(bh));
        assert(u < bh->next);
        AN(p);
        v = trickleup(bh, p, u);
        assert(v >= ROOT_IDX(bh));
        assert(v <= u);
	if (u == v) {
	        v = trickledown(bh, p, u);
	        assert(v >= u);
	}
	return v;
}

void
binheap_reorder(struct binheap *bh, unsigned u)
{
	void *p;
	unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
        assert(bh->next >= ROOT_IDX(bh));
        assert(u != BINHEAP_NOIDX);
        assert(u >= ROOT_IDX(bh));
        assert(u < bh->next);
        p = A(bh, u);
        AN(p);
	v = reorder(bh, p, u);
	if (u != v)
		assign(bh, p, v);
}

static void
remove_row(struct binheap *bh)
{
        unsigned u;

        CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
        assert(bh->length >= 2 * ROW_WIDTH);
        u = bh->length - 1;
	AN(bh->rows);
	assert(bh->rows_count > 0);
	assert(&ROW(bh, u) < bh->rows + bh->rows_count);
        AN(ROW(bh, u));
        free(ROW(bh, u));
        ROW(bh, u) = NULL;
        bh->length -= ROW_WIDTH;
}

void
binheap_delete(struct binheap *bh, unsigned u)
{
	void *p;
	unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(bh->next > ROOT_IDX(bh));
	assert(u != BINHEAP_NOIDX);
	assert(u >= ROOT_IDX(bh));
        assert(u < bh->next);
	p = A(bh, u);
        AN(p);
	A(bh, u) = NULL;
	bh->update(bh->priv, p, BINHEAP_NOIDX);
	assert(bh->next > 0);
        if (u < --bh->next) {
		p = A(bh, bh->next);
		AN(p);
	        A(bh, bh->next) = NULL;
		v = reorder(bh, p, u);
		assign(bh, p, v);
	}

        /*
         * We keep a hysteresis of one full row before we start to
         * return space to the OS to avoid silly behaviour around
         * row boundaries.
         */
        if (bh->next + 2 * ROW_WIDTH <= bh->length)
		remove_row(bh);
}

void *
binheap_root(const struct binheap *bh)
{
        CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(&A(bh, ROOT_IDX(bh)) == bh->rootp);
        assert(*bh->rootp != NULL || bh->next == ROOT_IDX(bh));
        return *bh->rootp;
}

/* binheap2 */
struct binheap2_item {
	void *p;
	unsigned u;
	unsigned key;
};

struct binheap2 {
	struct binheap *bh;
};

static int
cmp2(void *priv, void *p1, void *p2) {
	struct binheap2_item *bi1, *bi2;

	(void)priv;
	bi1 = p1;
	bi2 = p2;
	return bi1->key < bi2->key;
}

static void
update2(void *priv, void *p, unsigned u) {
	struct binheap2_item *bi;

	(void)priv;
	bi = p;
	bi->u = u;
}

struct binheap2 *
binheap2_new(void) {
	struct binheap2 *bh2;

	bh2 = malloc(sizeof(*bh2));
	AN(bh2);
	bh2->bh = binheap_new(NULL, cmp2, update2);
	AN(bh2->bh);
	return bh2;
}

struct binheap2_item *
binheap2_insert(struct binheap2 *bh2, void *p, unsigned key) {
	struct binheap2_item *bi;

	bi = malloc(sizeof(*bi));
	AN(bi);
	bi->p = p;
	bi->u = BINHEAP_NOIDX;
	bi->key = key;
	binheap_insert(bh2->bh, bi);
	return bi;
}

void
binheap2_delete(struct binheap2 *bh2, struct binheap2_item *bi) {
	binheap_delete(bh2->bh, bi->u);
	free(bi);	
}

void
binheap2_reorder(struct binheap2 *bh2, struct binheap2_item *bi, unsigned key) {
	bi->key = key;
	binheap_reorder(bh2->bh, bi->u);
}

void *
binheap2_root(struct binheap2 *bh2) {
	struct binheap2_item *bi;
	bi = binheap_root(bh2->bh);
	if (bi == NULL)
		return NULL;
	AN(bi->p);
	return bi->p;
}

#ifdef TEST_DRIVER

static void
check_invariant(const struct binheap *bh)
{
        unsigned u, v, root_idx, page_shift;
        void *p1, *p2;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
        page_shift = bh->page_shift;
        root_idx = R_IDX(page_shift);
	assert(page_shift > 0);
	assert(page_shift < ROW_SHIFT);
        for (u = root_idx + 1; u < bh->next; u++) {
                v = parent(page_shift, u);
                assert(v < u);
                assert(v >= root_idx);
                p1 = A(bh, u);
                p2 = A(bh, v);
		assert(bh->cmp(bh->priv, p2, p1) || !bh->cmp(bh->priv, p1, p2));
        }
}

#ifdef PARANOIA
#define paranoia_check(bh)	do { \
	check_invariant(bh); \
	check_indexes(bh); \
} while (0)
#else
#define paranoia_check(bh)	((void)0)
#endif

static void
check_parent_child(unsigned page_shift, unsigned n_max)
{
        unsigned n, u, v, root_idx;

	assert(page_shift > 0);
	assert(page_shift <= ROW_SHIFT);
        root_idx = R_IDX(page_shift);
        for (n = root_idx; n < n_max; n++) {
                u = child(page_shift, n);
                assert(u >= n);
                assert(u > root_idx);
                if (u == n)
                        continue;       /* child index is too big */
                v = parent(page_shift, u);
                assert(v == n);
                v = parent(page_shift, u + 1);
                assert(v == n);

                u = parent(page_shift, n);
                assert(u <= n);
                if (u == n) {
                        assert(u == root_idx);
                        continue;
                }
                v = child(page_shift, u);
                assert(v == (n & (0u - 2)));
        }
}

static void
check_parent_child_overflow(unsigned page_shift, unsigned n_max)
{
	unsigned n, u, v, root_idx;

	assert(page_shift > 0);
	assert(page_shift <= ROW_SHIFT);
	root_idx = R_IDX(page_shift);
	for (n = 0; n < n_max; n++) {
		u = parent(page_shift, UINT_MAX - n);
		assert(u < UINT_MAX - n);
		assert(u >= root_idx);
		v = child(page_shift, u);
		assert(v == ((UINT_MAX - n) & (0u - 2)));
	}
}

/* Test driver -------------------------------------------------------*/
#include <stdio.h>

static void
vasfail(const char *func, const char *file, int line,
    const char *cond, int err, int xxx)
{
	fprintf(stderr, "PANIC: %s %s %d %s %d %d\n",
		func, file, line, cond, err, xxx);
	abort();
}

vas_f *VAS_Fail = vasfail;

struct foo {
	unsigned	magic;
#define FOO_MAGIC	0x23239823u
	unsigned	idx;
	unsigned	key;
	unsigned	n;
};

#if 1
#define M 10000000u		/* Number of operations */
#define N 10000000u		/* Number of items */
#else
#define M 3401u			/* Number of operations */
#define N 1131u			/* Number of items */
#endif
#define R ((unsigned) RAND_MAX)	/* Random modulus */

static struct foo *ff[N];
static unsigned comparisons_count, update_calls_count, page_faults_count;

static int
foo_cmp(void *priv, void *a, void *b)
{
	struct foo *fa, *fb;

	CAST_OBJ_NOTNULL(fa, a, FOO_MAGIC);
	CAST_OBJ_NOTNULL(fb, b, FOO_MAGIC);
	++comparisons_count;
	return (fa->key < fb->key);
}

static void
foo_update(void *priv, void *a, unsigned u)
{
	struct binheap *bh;
	struct foo *fp;
	void **p1, **p2;
	unsigned page_mask;

	bh = *((struct binheap **) priv);
	CAST_OBJ_NOTNULL(fp, a, FOO_MAGIC);
	++update_calls_count;
	if (fp->idx != BINHEAP_NOIDX && u != BINHEAP_NOIDX) {
		p1 = &A(bh, fp->idx);
		p2 = &A(bh, u);
		assert(*p2 == fp);
		page_mask = ~((1u << bh->page_shift) * sizeof(void *) - 1);
		if ((((uintptr_t) p1) & page_mask) !=
			(((uintptr_t) p2) & page_mask) &&
			(p2 - bh->rows[0] < 0 ||
				p2 - bh->rows[0] >= 2 * (~page_mask + 1)))
			++page_faults_count;
	}
	fp->idx = u;
}

static void
check_indexes(const struct binheap *bh)
{
	struct foo *fp;
	unsigned n;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	for (n = ROOT_IDX(bh); n < bh->next; n++) {
		fp = A(bh, n);
		assert(fp->idx == n);
	}
}

static void
foo_check(const struct foo *fp)
{
	CHECK_OBJ_NOTNULL(fp, FOO_MAGIC);
	assert(fp->n < N);
	assert(fp == ff[fp->n]);
}

static void
foo_check_existense(const struct foo *fp)
{
        foo_check(fp);
        assert(fp->idx != BINHEAP_NOIDX);
}

static void
foo_insert(struct binheap *bh, unsigned n)
{
        struct foo *fp;
	unsigned key;

	paranoia_check(bh);
        assert(n < N);
        AZ(ff[n]);
        ALLOC_OBJ(fp, FOO_MAGIC);
        XXXAN(fp);
	key = random() % R;
        fp->idx = BINHEAP_NOIDX;
        fp->key = key;
        fp->n = n;
        ff[n] = fp;
	binheap_insert(bh, fp);
	foo_check_existense(fp);
	assert(fp->key == key);
	assert(fp->n == n);
	paranoia_check(bh);
}

static void
foo_delete(struct binheap *bh, struct foo *fp)
{
	unsigned key, n;

	paranoia_check(bh);
	foo_check_existense(fp);
	key = fp->key;
	n = fp->n;
	binheap_delete(bh, fp->idx);
	foo_check(fp);
	assert(fp->idx == BINHEAP_NOIDX);
	assert(fp->key == key);
	assert(fp->n == n);
        ff[fp->n] = NULL;
        FREE_OBJ(fp);
	paranoia_check(bh);
}

static void
foo_reorder(struct binheap *bh, struct foo *fp)
{
        unsigned key, n;

	paranoia_check(bh);
        foo_check_existense(fp);
        key = random() % R;
	n = fp->n;
        fp->key = key;
        binheap_reorder(bh, fp->idx);
	foo_check_existense(fp);
	assert(fp->key == key);
	assert(fp->n == n);
	paranoia_check(bh);
}

static void
print_counters(void)
{
	fprintf(stderr, "%u comparisons, %u update calls, %u page faults\n",
		comparisons_count, update_calls_count, page_faults_count);
	comparisons_count = update_calls_count = page_faults_count = 0;
}

int
main(int argc, char **argv)
{
	struct binheap *bh;
        struct foo *fp;
	unsigned u, n, key, root_idx;
	unsigned delete_count, insert_count, reorder_count;

	if (0) {
//		srandomdev();
		u = random();
		printf("Seed %u\n", u);
		srandom(u);
	}
	for (u = 1; u <= ROW_SHIFT; u++) {
	        check_parent_child(u, M);
	        fprintf(stderr, "%u parent-child tests for row_shift=%u OK\n", M, u);
	        check_parent_child_overflow(u, M);
	        fprintf(stderr, "%u overflow parent-child tests for row_shift=%u OK\n",
			M, u);
	}
	bh = binheap_new(&bh, foo_cmp, foo_update);
	AZ(binheap_root(bh));

	root_idx = ROOT_IDX(bh);
        assert(root_idx != BINHEAP_NOIDX);
	key = 0;
	while (1) {
		/* First insert our N elements */
		for (n = 0; n < N; n++) {
			foo_insert(bh, n);
			key = ff[n]->key;
			fp = binheap_root(bh);
			foo_check(fp);
			assert(fp->idx == root_idx);
			assert(fp->key <= key);
		}
		check_invariant(bh);
		check_indexes(bh);
		fprintf(stderr, "%u inserts OK\n", N);
		print_counters();

		/* For M cycles, pick the root, insert new */
		for (u = 0; u < M; u++) {
			fp = binheap_root(bh);
			foo_check(fp);
			assert(fp->idx == root_idx);
			assert(fp->key <= key);
			n = fp->n;
			foo_delete(bh, fp);
			foo_insert(bh, n);
			key = ff[n]->key;
		}
		check_invariant(bh);
		check_indexes(bh);
		fprintf(stderr, "%u replacements OK\n", M);
		print_counters();

		/* Randomly insert, delete and reorder */
		delete_count = 0;
		insert_count = 0;
		reorder_count = 0;
		for (u = 0; u < M; u++) {
			n = random() % N;
			fp = ff[n];
			if (fp != NULL) {
				if (fp->key & 1) {
					foo_delete(bh, fp);
					++delete_count;
				} else {
					foo_reorder(bh, fp);
					++reorder_count;
				}
			} else {
				foo_insert(bh, n);
				++insert_count;
			}
		}
		assert(delete_count >= insert_count);
		check_invariant(bh);
		check_indexes(bh);
		fprintf(stderr, "%u deletes, %u inserts, %u reorders OK\n",
			delete_count, insert_count, reorder_count);
		print_counters();

                /* Then remove everything */
                key = 0;
		u = 0;
                while (1) {
                        fp = binheap_root(bh);
                        if (fp == NULL) {
                                break;
                        }
                        foo_check(fp);
			assert(fp->idx == root_idx);
                        assert(fp->key >= key);
                        key = fp->key;
                        foo_delete(bh, fp);
                        ++u;
                }
                assert(u == N - (delete_count - insert_count));
		AZ(binheap_root(bh));
		check_invariant(bh);
		check_indexes(bh);
                fprintf(stderr, "%u removes OK\n", u);
		print_counters();
	}
	return (0);
}
#endif
