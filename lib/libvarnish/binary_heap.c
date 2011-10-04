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
 * Update function pass this value as newidx if the given item has been
 * removed from binheap.
 */
#define NOIDX   0

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

struct binheap_item {
        unsigned idx;
        void *p;
};

/*
 * Storing key near p should improve memory locality
 * for hot paths during binheap mutations.
 * Code below expects sizeof(binheap_item) is a power of two.
 */
struct item {
	unsigned key;
	struct binheap_item *bi;
};

struct binheap {
        unsigned                magic;
#define BINHEAP_MAGIC           0x8bd801f0u     /* from /dev/random */
        struct item		**rows;
	struct binheap_item	*free_list;
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

static struct item *
alloc_row(unsigned page_shift)
{
        void *p;
	struct item *row;
	size_t item_size, alignment;
	unsigned u;
	int rv;

	assert(page_shift > 0);
	assert(page_shift <= ROW_SHIFT);
	item_size = sizeof(*row);
	AZ(item_size & (item_size - 1));	/* should be power of 2 */
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
	for (u = 0; u < ROW_WIDTH; u++) {
		row[u].key = 0;
		row[u].bi = NULL;
	}
	return row;
}

struct binheap *
binheap_new(void)
{
	struct item **rows;
        struct binheap *bh;
        unsigned page_size, page_shift;

        page_size = ((unsigned) getpagesize()) / sizeof(**rows);
	xxxassert(page_size > 1);
	xxxassert(page_size * sizeof(**rows) == getpagesize());
        page_shift = 0u - 1;
        while (page_size) {
                page_size >>= 1;
                ++page_shift;
        }
        assert(page_shift > 0);
	page_size = 1u << page_shift;
        xxxassert(page_size <= ROW_WIDTH);
	XXXAZ(ROW_WIDTH % page_size);

        rows = calloc(1, sizeof(*rows));
        XXXAN(rows);
        AZ(rows[0]);

	/*
	 * Since the first memory page in the first row is almost empty
	 * (except the row[R_IDX(page_shift)] at the end of the page),
	 * so let's embed binheap structure into the beginning of the page.
	 * This should also improve locality of reference.
         */
	rows[0] = alloc_row(page_shift);
	AN(rows[0]);
        bh = (struct binheap *) rows[0];
	bh->magic = BINHEAP_MAGIC;
        bh->rows = rows;
	bh->free_list = NULL;
        bh->next = R_IDX(page_shift);
        bh->rows_count = 1;
	bh->length = ROW_WIDTH;
	bh->page_shift = page_shift;

	/*
	 * Make sure the page with embedded binheap don't overlap with
	 * binheap items, which start from R_IDX(page_shift) index.
	 */
        xxxassert(sizeof(*bh) <= sizeof(**rows) * R_IDX(page_shift));
        return bh;
}

static void
assign(const struct binheap *bh, struct binheap_item *bi, unsigned key,
	unsigned idx)
{
	struct item *i;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(bi);
	assert(idx != NOIDX);
	assert(idx >= ROOT_IDX(bh));
	assert(idx < bh->next);
	i = &A(bh, idx);
	i->key = key;
	i->bi = bi;
	bi->idx = idx;
}

static unsigned
trickleup(const struct binheap *bh, unsigned key, unsigned u)
{
	struct item *i;
        unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(u >= ROOT_IDX(bh));
        assert(u < bh->next);

        while (u != ROOT_IDX(bh)) {
                v = parent(bh->page_shift, u);
                assert(v < u);
		assert(v >= ROOT_IDX(bh));
		i = &A(bh, v);
                AN(i->bi);
		AN(i->bi->p);
		assert(i->bi->idx == v);
                if (i->key < key)
                        break;	/* parent is smaller than the child */
		assign(bh, i->bi, i->key, u);
		assert(i->bi->idx == u);
		assert(A(bh, u).bi == i->bi);
		assert(A(bh, u).key == i->key);
                u = v;
        }
        return u;
}

static unsigned
trickledown(const struct binheap *bh, unsigned key, unsigned u)
{
	struct item *i1, *i2;
	unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(u >= ROOT_IDX(bh));
        assert(u < bh->next);

        while (1) {
                v = child(bh->page_shift, u);
		assert(v > ROOT_IDX(bh));
                assert(v >= u);
		if (v == u)
			break;		/* index overflow */
                if (v >= bh->next)
                        break;		/* reached the end of heap */
		i1 = &A(bh, v);
		AN(i1->bi);
		AN(i1->bi->p);
		assert(i1->bi->idx == v);
                if (v + 1 < bh->next) {
			i2 = &A(bh, v + 1);
			AN(i2->bi);
			AN(i2->bi->p);
			assert(i2->bi->idx == v + 1);
                        if (i2->key < i1->key) {
                                ++v;
				i1 = i2;
			}
                }
                assert(v < bh->next);
                if (key < i1->key)
                        break;	/* parent is smaller than children */
		assign(bh, i1->bi, i1->key, u);
		assert(i1->bi->idx == u);
		assert(A(bh, u).bi == i1->bi);
		assert(A(bh, u).key == i1->key);
                u = v;
        }
	return u;
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
		AZ(((uintptr_t) bh->rows) % sizeof(*bh->rows));
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

static struct binheap_item *
alloc_bi_row(void)
{
        struct binheap_item *row;
        unsigned u;

	/* TODO: determine the best row width */
        row = calloc(ROW_WIDTH, sizeof(*row));
        XXXAN(row);
	AZ(((uintptr_t) row) % sizeof(*row));

        /* construct freelist from items in the row */
        for (u = 0; u < ROW_WIDTH; u++) {
                row[u].idx = NOIDX;
                row[u].p = row + u + 1;
        }
        row[ROW_WIDTH - 1].p = NULL;
        return row;
}

static struct binheap_item *
acquire_bi(struct binheap *bh)
{
	struct binheap_item *bi;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	if (bh->free_list == NULL) {
		bh->free_list = alloc_bi_row();
		AN(bh->free_list);
	}
	bi = bh->free_list;
	bh->free_list = bi->p;
	AN(bi);
	assert(bi->idx == NOIDX);
	bi->p = NULL;
	return bi;
}

static void
release_bi(struct binheap *bh, struct binheap_item *bi)
{
	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(bi);
	assert(bi->idx != NOIDX);
	AN(bi->p);
	bi->idx = NOIDX;
	bi->p = bh->free_list;
	bh->free_list = bi;
	/* TODO: free up memory? */
}

struct binheap_item *
binheap_insert(struct binheap *bh, void *p, unsigned key)
{
	struct binheap_item *bi;
        unsigned u, v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(p);
	assert(bh->next >= ROOT_IDX(bh));
        assert(bh->next <= bh->length);
        if (bh->length == bh->next)
        	add_row(bh);
        assert(bh->length > bh->next);
	assert(bh->next < UINT_MAX);
        u = bh->next++;
	AZ(A(bh, u).bi);
	AZ(A(bh, u).key);
        v = trickleup(bh, key, u);
	assert(v <= u);
	assert(v >= ROOT_IDX(bh));
	bi = acquire_bi(bh);
	AN(bi);
	assert(bi->idx == NOIDX);
	AZ(bi->p);
	bi->p = p;
	assign(bh, bi, key, v);
	assert(bi->idx == v);
	assert(A(bh, v).bi == bi);
	assert(A(bh, v).key == key);
	return bi;
}

static unsigned
reorder(const struct binheap *bh, unsigned key, unsigned u)
{
        unsigned v;

        CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
        assert(bh->next >= ROOT_IDX(bh));
        assert(u >= ROOT_IDX(bh));
        assert(u < bh->next);
        v = trickleup(bh, key, u);
        assert(v >= ROOT_IDX(bh));
        assert(v <= u);
	if (u == v) {
	        v = trickledown(bh, key, u);
	        assert(v >= u);
		assert(v < bh->next);
	}
	return v;
}

void
binheap_update(const struct binheap *bh, struct binheap_item *bi,
	unsigned key)
{
	struct item *i;
	unsigned u, v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
        assert(bh->next >= ROOT_IDX(bh));
	AN(bi);
	AN(bi->p);
	u = bi->idx;
        assert(u != NOIDX);
        assert(u >= ROOT_IDX(bh));
        assert(u < bh->next);
	i = &A(bh, u);
	assert(i->bi == bi);
	v = reorder(bh, key, u);
	if (u != v)
		assign(bh, bi, key, v);
	else
		i->key = key;
        assert(bi->idx == v);
        assert(A(bh, v).bi == bi);
	assert(A(bh, v).key == key);
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
binheap_delete(struct binheap *bh, struct binheap_item *bi)
{
	struct item *i;
	unsigned u, v, key;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(bh->next > ROOT_IDX(bh));
	assert(bh->next <= bh->length);
	AN(bi);
	AN(bi->p);
	u = bi->idx;
	assert(u != NOIDX);
	assert(u >= ROOT_IDX(bh));
        assert(u < bh->next);
	i = &A(bh, u);
	assert(i->bi == bi);
	i->key = 0;
	i->bi = NULL;
	release_bi(bh, bi);
	assert(bi->idx == NOIDX);
	assert(bh->next > 0);
        if (u < --bh->next) {
		i = &A(bh, bh->next);
		key = i->key;
		bi = i->bi;
		AN(bi);
		AN(bi->p);
		assert(bi->idx == bh->next);
		i->key = 0;
	        i->bi = NULL;
		v = reorder(bh, key, u);
		assign(bh, bi, key, v);
		assert(bi->idx == v);
		assert(A(bh, v).bi == bi);
		assert(A(bh, v).key == key);
	}

        /*
         * We keep a hysteresis of one full row before we start to
         * return space to the OS to avoid silly behaviour around
         * row boundaries.
         */
        if (bh->next + 2 * ROW_WIDTH <= bh->length) {
		remove_row(bh);
		assert(bh->next + 2 * ROW_WIDTH > bh->length);
	}
}

void *
binheap_root(const struct binheap *bh)
{
	struct binheap_item *bi;

        CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	bi = A(bh, ROOT_IDX(bh)).bi;
	if (bi == NULL) {
		assert(bh->next == ROOT_IDX(bh));
		return NULL;
	}
	assert(bi->idx == ROOT_IDX(bh));
	AN(bi->p);
	return bi->p;
}

#ifdef TEST_DRIVER

static void
check_consistency(const struct binheap *bh)
{
	struct item *i1, *i2;
        unsigned u, v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(ROOT_IDX(bh) != NOIDX);
	assert(bh->next >= ROOT_IDX(bh));
	assert(bh->next <= bh->length);
	assert(bh->length >= ROW_WIDTH);
	assert(bh->rows_count >= 1);
	assert(bh->rows_count <= UINT_MAX / ROW_WIDTH);
	assert(bh->rows_count * ROW_WIDTH >= bh->length);
	assert(bh->page_shift > 0);
	assert(bh->page_shift <= ROW_SHIFT);
	AN(bh->rows);
        for (u = ROOT_IDX(bh) + 1; u < bh->next; u++) {
                v = parent(bh->page_shift, u);
                assert(v < u);
                assert(v >= ROOT_IDX(bh));
		i1 = &A(bh, u);
		i2 = &A(bh, v);
		assert(i2->key <= i1->key);
		AN(i2->bi);
		AN(i2->bi->p);
		assert(i1->bi->idx == u);
		AN(i2->bi);
		AN(i2->bi->p);
		assert(i2->bi->idx == v);
        }
}

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
                assert(v == n || v == n - 1);
        }
}

static void
check_parent_child_overflow(unsigned page_shift, unsigned n_max)
{
	unsigned n, u, v, root_idx;

	assert(page_shift > 0);
	assert(page_shift <= ROW_SHIFT);
	root_idx = R_IDX(page_shift);
	n = UINT_MAX - n_max;
	while (n++ < UINT_MAX) {
		u = parent(page_shift, n);
		assert(u < n);
		assert(u >= root_idx);
		v = child(page_shift, u);
		assert(v == n || v == n - 1);

		u = child(page_shift, n);
		assert(u >= n);
		assert(u >= root_idx);
		if (u == n)
			continue;	/* overflow */
		v = parent(page_shift, u);
		assert(v == n || v == n - 1);
	}
}

/* Test driver -------------------------------------------------------*/
#include <stdio.h>
#include <time.h>

double
TIM_mono(void)
{
        struct timespec ts;

        XXXAZ(clock_gettime(CLOCK_MONOTONIC, &ts));
        return (ts.tv_sec + 1e-9 * ts.tv_nsec);
}

static void
vasfail(const char *func, const char *file, int line,
    const char *cond, int err, int xxx)
{
	fprintf(stderr, "PANIC: %s %s %d %s %d %d\n",
		func, file, line, cond, err, xxx);
	abort();
}

vas_f *VAS_Fail = vasfail;

#define M 1000000u		/* Number of operations */
#define N 500000u		/* Number of items */
#define R ((unsigned) RAND_MAX)	/* Random modulus */

/*
 * Pad foo so its' size is equivalent to the objcore size.
 * Currently size of objcore is 120 bytes on x64 and 72 bytes
 * on x32. This means that the padding should be 96 for x64
 * and 56 for x32.
 */
#define PADDING 96

#ifdef PARANOIA
#define paranoia_check(bh)      check_consistency(bh)
#else
#define paranoia_check(bh)      ((void)0)
#endif

struct foo {
        unsigned                magic;
#define FOO_MAGIC       0x23239823u
        unsigned                key;
        unsigned                n;
        struct binheap_item     *bi;
	char padding[PADDING];
};

static struct foo ff[N];

static void
foo_check(const struct foo *fp)
{
	CHECK_OBJ_NOTNULL(fp, FOO_MAGIC);
	assert(fp->n < N);
	assert(fp == &ff[fp->n]);
}

static void
foo_check_existense(struct binheap *bh, const struct foo *fp)
{
        foo_check(fp);
	AN(fp->bi);
	assert(fp->bi->idx != NOIDX);
	assert(fp->bi->idx >= ROOT_IDX(bh));
	assert(fp->bi->idx < bh->next);
	assert(fp->bi->p == fp);
	assert(fp->bi == A(bh, fp->bi->idx).bi);
	assert(fp->key == A(bh, fp->bi->idx).key);
}

static void
foo_insert(struct binheap *bh, unsigned n)
{
        struct foo *fp;
	unsigned key;

	paranoia_check(bh);
        assert(n < N);
	fp = &ff[n];
	AZ(fp->key);
	AZ(fp->n);
        AZ(fp->bi);
	key = random() % R;
	fp->magic = FOO_MAGIC;
        fp->key = key;
        fp->n = n;
	fp->bi = binheap_insert(bh, fp, key);
	foo_check_existense(bh, fp);
	assert(fp->key == key);
	assert(fp->n == n);
	paranoia_check(bh);
}

static void
foo_delete(struct binheap *bh, struct foo *fp)
{
	unsigned key, n;

	paranoia_check(bh);
	foo_check_existense(bh, fp);
	key = fp->key;
	n = fp->n;
	binheap_delete(bh, fp->bi);
	foo_check(fp);
	AN(fp->bi);
	assert(fp->bi->idx == NOIDX);
	assert(fp->key == key);
	assert(fp->n == n);
	fp->key = 0;
	fp->n = 0;
	fp->bi = NULL;
	paranoia_check(bh);
}

static void
foo_update(struct binheap *bh, struct foo *fp)
{
        unsigned key, n;

	paranoia_check(bh);
        foo_check_existense(bh, fp);
        key = random() % R;
	n = fp->n;
        fp->key = key;
        binheap_update(bh, fp->bi, key);
	foo_check_existense(bh, fp);
	assert(fp->key == key);
	assert(fp->n == n);
	paranoia_check(bh);
}

static void
test(struct binheap *bh)
{
	double start, end;
        struct foo *fp;
	unsigned u, n, key, root_idx;
	unsigned delete_count, insert_count, update_count;

	AZ(binheap_root(bh));
	root_idx = ROOT_IDX(bh);
        assert(root_idx != NOIDX);

	/* First insert our N elements */
	start = TIM_mono();
	for (n = 0; n < N; n++) {
		foo_insert(bh, n);
		key = ff[n].key;
		fp = binheap_root(bh);
		foo_check(fp);
		assert(fp->bi->idx == root_idx);
		assert(fp->key <= key);
	}
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u inserts, %.3lfs OK\n", N, end - start);

	/* For M cycles, pick the root, insert new */
	start = TIM_mono();
	for (u = 0; u < M; u++) {
		fp = binheap_root(bh);
		foo_check(fp);
		assert(fp->bi->idx == root_idx);
		assert(fp->key <= key);
		n = fp->n;
		foo_delete(bh, fp);
		foo_insert(bh, n);
		key = ff[n].key;
	}
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u replacements, %.3lfs OK\n", M, end - start);

	/* Randomly insert, delete and update */
	delete_count = 0;
	insert_count = 0;
	update_count = 0;
	start = TIM_mono();
	for (u = 0; u < M; u++) {
		n = random() % N;
		fp = &ff[n];
		if (fp->bi != NULL) {
			if (fp->key & 1) {
				foo_delete(bh, fp);
				++delete_count;
			} else {
				foo_update(bh, fp);
				++update_count;
			}
		} else {
			foo_insert(bh, n);
			++insert_count;
		}
	}
	assert(delete_count >= insert_count);
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr,
		"%u deletes, %u inserts, %u updates, %.3lfs OK\n",
		delete_count, insert_count, update_count, end - start);

	/* Then remove everything */
	key = 0;
	u = 0;
	start = TIM_mono();
	while (1) {
        	fp = binheap_root(bh);
                if (fp == NULL)
                	break;
		foo_check(fp);
		assert(fp->bi->idx == root_idx);
                assert(fp->key >= key);
                key = fp->key;
                foo_delete(bh, fp);
                ++u;
	}
        assert(u == N - (delete_count - insert_count));
	AZ(binheap_root(bh));
	check_consistency(bh);
	end = TIM_mono();
        fprintf(stderr, "%u deletes, %.3lfs OK\n", u, end - start);
}

int
main(int argc, char **argv)
{
        struct binheap *bh;
        unsigned u, root_idx;

        for (u = 1; u <= ROW_SHIFT; u++) {
                check_parent_child(u, M);
                check_parent_child_overflow(u, M);
        }
        fprintf(stderr, "parent-child test OK\n");

        bh = binheap_new();
        AZ(binheap_root(bh));
        root_idx = ROOT_IDX(bh);
        assert(root_idx != NOIDX);

	test(bh);
	while (1)
		test(bh);
	return 0;
}
#endif
