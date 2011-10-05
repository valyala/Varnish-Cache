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

#include "binary_heap.h"
#include "libvarnish.h"

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
#define ROW_SHIFT		16


#undef PARANOIA

/* Private definitions -----------------------------------------------*/

#define ROOT_IDX		1

#define ROW_WIDTH		(1 << ROW_SHIFT)

/*lint -emacro(572, ROW) shift 0 >> by 16 */
/*lint -emacro(835, ROW) 0 left of >> */
/*lint -emacro(778, ROW) const >> evaluates to zero */
#define ROW(b, n)		((b)->array[(n) >> ROW_SHIFT])

/*lint -emacro(835, A) 0 left of & */
#define A(b, n)			ROW(b, n)[(n) & (ROW_WIDTH - 1)]

struct binheap {
	unsigned		magic;
#define BINHEAP_MAGIC		0xf581581aU	/* from /dev/random */
	void			*priv;
	binheap_cmp_t		*cmp;
	binheap_update_t	*update;
	void			***array;
	unsigned		rows;
	unsigned		length;
	unsigned		next;
	unsigned		page_size;
	unsigned		page_mask;
	unsigned		page_shift;
};

#define VM_AWARE

#ifdef VM_AWARE

static  unsigned
parent(const struct binheap *bh, unsigned u)
{
	unsigned po;
	unsigned v;

	assert(u != UINT_MAX);
	po = u & bh->page_mask;

	if (u < bh->page_size || po > 3) {
		v = (u & ~bh->page_mask) | (po >> 1);
	} else if (po < 2) {
		v = (u - bh->page_size) >> bh->page_shift;
		v += v & ~(bh->page_mask >> 1);
		v |= bh->page_size / 2;
	} else {
		v = u - 2;
	}
	return (v);
}

static void
child(const struct binheap *bh, unsigned u, unsigned *a, unsigned *b)
{
	uintmax_t uu;

	if (u > bh->page_mask && (u & (bh->page_mask - 1)) == 0) {
		/* First two elements are magical except on the first page */
		*a = *b = u + 2;
	} else if (u & (bh->page_size >> 1)) {
		/* The bottom row is even more magical */
		*a = (u & ~bh->page_mask) >> 1;
		*a |= u & (bh->page_mask >> 1);
		*a += 1;
		uu = (uintmax_t)*a << bh->page_shift;
		*a = uu;
		if (*a == uu) {
			*b = *a + 1;
		} else {
			/*
			 * An unsigned is not big enough: clamp instead
			 * of truncating.  We do not support adding
			 * more than UINT_MAX elements anyway, so this
			 * is without consequence.
			 */
			*a = UINT_MAX;
			*b = UINT_MAX;
		}
	} else {
		/* The rest is as usual, only inside the page */
		*a = u + (u & bh->page_mask);
		*b = *a + 1;
	}
#ifdef PARANOIA
	assert(*a > 0);
	assert(*b > 0);
	if (*a != UINT_MAX) {
		assert(parent(bh, *a) == u);
		assert(parent(bh, *b) == u);
	}
#endif
}


#else

static unsigned
parent(const struct binheap *bh, unsigned u)
{

	(void)bh;
	return (u / 2);
}

static void
child(const struct binheap *bh, unsigned u, unsigned *a, unsigned *b)
{

	(void)bh;
	*a = u * 2;
	*b = *a + 1;
}

#endif

/* Implementation ----------------------------------------------------*/

static void
binheap_addrow(struct binheap *bh)
{
	unsigned u;

	/* First make sure we have space for another row */
	if (&ROW(bh, bh->length) >= bh->array + bh->rows) {
		u = bh->rows * 2;
		bh->array = realloc(bh->array, sizeof(*bh->array) * u);
		assert(bh->array != NULL);

		/* NULL out new pointers */
		while (bh->rows < u)
			bh->array[bh->rows++] = NULL;
	}
	assert(ROW(bh, bh->length) == NULL);
	ROW(bh, bh->length) = malloc(sizeof(**bh->array) * ROW_WIDTH);
	assert(ROW(bh, bh->length));
	bh->length += ROW_WIDTH;
}

struct binheap *
binheap_new(void *priv, binheap_cmp_t *cmp_f, binheap_update_t *update_f)
{
	struct binheap *bh;
	unsigned u;

	bh = calloc(sizeof *bh, 1);
	if (bh == NULL)
		return (bh);
	bh->priv = priv;

	bh->page_size = (unsigned)getpagesize() / sizeof (void *);
	bh->page_mask = bh->page_size - 1;
	assert(!(bh->page_size & bh->page_mask));	/* power of two */
	for (u = 1; (1U << u) != bh->page_size; u++)
		;
	bh->page_shift = u;
	assert(bh->page_size <= (sizeof(**bh->array) * ROW_WIDTH));

	bh->cmp = cmp_f;
	bh->update = update_f;
	bh->next = ROOT_IDX;
	bh->rows = 16;		/* A tiny-ish number */
	bh->array = calloc(sizeof *bh->array, bh->rows);
	assert(bh->array != NULL);
	binheap_addrow(bh);
	A(bh, ROOT_IDX) = NULL;
	bh->magic = BINHEAP_MAGIC;
	return (bh);
}

static void
binheap_update(const struct binheap *bh, unsigned u)
{
	assert(bh != NULL);
	assert(bh->magic == BINHEAP_MAGIC);
	assert(u < bh->next);
	assert(A(bh, u) != NULL);
	if (bh->update != NULL)
		bh->update(bh->priv, A(bh, u), u);
}

static void
binhead_swap(const struct binheap *bh, unsigned u, unsigned v)
{
	void *p;

	assert(bh != NULL);
	assert(bh->magic == BINHEAP_MAGIC);
	assert(u < bh->next);
	assert(A(bh, u) != NULL);
	assert(v < bh->next);
	assert(A(bh, v) != NULL);
	p = A(bh, u);
	A(bh, u) = A(bh, v);
	A(bh, v) = p;
	binheap_update(bh, u);
	binheap_update(bh, v);
}

static unsigned
binheap_trickleup(const struct binheap *bh, unsigned u)
{
	unsigned v;

	assert(bh != NULL); assert(bh->magic == BINHEAP_MAGIC);
	assert(u < bh->next);
	assert(A(bh, u) != NULL);

	while (u > ROOT_IDX) {
		assert(u < bh->next);
		assert(A(bh, u) != NULL);
		v = parent(bh, u);
		assert(v < u);
		assert(v < bh->next);
		assert(A(bh, v) != NULL);
		if (!bh->cmp(bh->priv, A(bh, u), A(bh, v)))
			break;
		binhead_swap(bh, u, v);
		u = v;
	}
	return (u);
}

static unsigned
binheap_trickledown(const struct binheap *bh, unsigned u)
{
	unsigned v1, v2;

	assert(bh != NULL);
	assert(bh->magic == BINHEAP_MAGIC);
	assert(u < bh->next);
	assert(A(bh, u) != NULL);

	while (1) {
		assert(u < bh->next);
		assert(A(bh, u) != NULL);
		child(bh, u, &v1, &v2);
		assert(v1 > 0);
		assert(v2 > 0);
		assert(v1 <= v2);

		if (v1 >= bh->next)
			return (u);

		assert(A(bh, v1) != NULL);
		if (v1 != v2 && v2 < bh->next) {
			assert(A(bh, v2) != NULL);
			if (bh->cmp(bh->priv, A(bh, v2), A(bh, v1)))
				v1 = v2;
		}
		assert(v1 < bh->next);
		assert(A(bh, v1) != NULL);
		if (bh->cmp(bh->priv, A(bh, u), A(bh, v1)))
			return (u);
		binhead_swap(bh, u, v1);
		u = v1;
	}
}

void
binheap_insert(struct binheap *bh, void *p)
{
	unsigned u;

	assert(bh != NULL);
	assert(bh->magic == BINHEAP_MAGIC);
	assert(bh->length >= bh->next);
	if (bh->length == bh->next)
		binheap_addrow(bh);
	assert(bh->length > bh->next);
	u = bh->next++;
	A(bh, u) = p;
	binheap_update(bh, u);
	(void)binheap_trickleup(bh, u);
	assert(u < bh->next);
	assert(A(bh, u) != NULL);
}


#ifdef PARANOIA
static void
chk(const struct binheap *bh)
{
	unsigned u, v;

	for (u = 2; u < bh->next; u++) {
		v = parent(bh, u);
		assert(!bh->cmp(bh->priv, A(bh, u), A(bh, v)));
	}
}
#endif

void *
binheap_root(const struct binheap *bh)
{

	assert(bh != NULL);
	assert(bh->magic == BINHEAP_MAGIC);
#ifdef PARANOIA
	chk(bh);
#endif
	return (A(bh, ROOT_IDX));
}

/*
 * It may seem counter-intuitive that we delete by replacement with
 * the tail object. "That's almost certain to not belong there, in
 * particular when we delete the root ?" is the typical reaction.
 *
 * If we tried to trickle up into the empty position, we would,
 * eventually, end up with a hole in the bottom row, at which point
 * we would move the tail object there.
 * But there is no guarantee that the tail object would not need to
 * trickle up from that position, in fact, it might be the new root
 * of this half of the subtree.
 * The total number of operations is guaranteed to be at least
 * N{height} downward selections, because we have to get the hole
 * all the way down, but in addition to that, we may get up to
 * N{height}-1 upward trickles.
 *
 * When we fill the hole with the tail object, the worst case is
 * that it trickles all the way up to of this half-tree, or down
 * to become the tail object again.
 *
 * In other words worst case is N{height} up or downward trickles.
 * But there is a decent chance that it does not make it all the way.
 */

void
binheap_delete(struct binheap *bh, unsigned idx)
{

	assert(bh != NULL);
	assert(bh->magic == BINHEAP_MAGIC);
	assert(bh->next > ROOT_IDX);
	assert(idx < bh->next);
	assert(idx > 0);
	assert(A(bh, idx) != NULL);
	bh->update(bh->priv, A(bh, idx), BINHEAP_NOIDX);
	if (idx == --bh->next) {
		A(bh, bh->next) = NULL;
		return;
	}
	A(bh, idx) = A(bh, bh->next);
	A(bh, bh->next) = NULL;
	binheap_update(bh, idx);
	idx = binheap_trickleup(bh, idx);
	assert(idx < bh->next);
	assert(idx > 0);
	assert(A(bh, idx) != NULL);
	idx = binheap_trickledown(bh, idx);
	assert(idx < bh->next);
	assert(idx > 0);
	assert(A(bh, idx) != NULL);

	/*
	 * We keep a hysteresis of one full row before we start to
	 * return space to the OS to avoid silly behaviour around
	 * row boundaries.
	 */
	if (bh->next + 2 * ROW_WIDTH <= bh->length) {
		free(ROW(bh, bh->length - 1));
		ROW(bh, bh->length - 1) = NULL;
		bh->length -= ROW_WIDTH;
	}
}

/*
 * Move an item up/down after changing its key value
 */

void
binheap_reorder(const struct binheap *bh, unsigned idx)
{

	assert(bh != NULL);
	assert(bh->magic == BINHEAP_MAGIC);
	assert(bh->next > ROOT_IDX);
	assert(idx < bh->next);
	assert(idx > 0);
	assert(A(bh, idx) != NULL);
	idx = binheap_trickleup(bh, idx);
	assert(idx < bh->next);
	assert(idx > 0);
	assert(A(bh, idx) != NULL);
	idx = binheap_trickledown(bh, idx);
	assert(idx < bh->next);
	assert(idx > 0);
	assert(A(bh, idx) != NULL);
}

#ifdef TEST_DRIVER

/* Test driver -------------------------------------------------------*/
#include <stdio.h>
#include <miniobj.h>
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

#define M 1000000u              /* Number of operations */
#define N 500000u               /* Number of items */
#define R ((unsigned) RAND_MAX) /* Random modulus */

/*
 * Pad foo so its' size is equivalent to the objcore size.
 * Currently size of objcore is 120 bytes on x64 and 72 bytes
 * on x32. This means that the padding should be 104 for x64
 * and 56 for x32.
 */
#define PADDING 104

struct foo {
        unsigned        magic;
#define FOO_MAGIC       0x23239823
        unsigned        idx;
        unsigned        key;
        unsigned        n;
	char		padding[PADDING];
};

static struct foo ff[N];

static int
cmp(void *priv, void *a, void *b)
{
	struct foo *fa, *fb;

	CAST_OBJ_NOTNULL(fa, a, FOO_MAGIC);
	CAST_OBJ_NOTNULL(fb, b, FOO_MAGIC);
	return (fa->key < fb->key);
}

void
update(void *priv, void *a, unsigned u)
{
	struct foo *fa;

	CAST_OBJ_NOTNULL(fa, a, FOO_MAGIC);
	fa->idx = u;
}

static void
check_consistency(const struct binheap *bh)
{
        struct foo *fp1, *fp2;
        unsigned u, v;

        CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
        assert(bh->next >= ROOT_IDX);
        assert(bh->next <= bh->length);
        assert(bh->length >= ROW_WIDTH);
        assert(bh->rows >= 1);
        assert(bh->rows <= UINT_MAX / ROW_WIDTH);
        assert(bh->rows * ROW_WIDTH >= bh->length);
        assert(bh->page_shift > 0);
        assert(bh->page_shift <= ROW_SHIFT);
        assert(bh->page_size == (1u << bh->page_shift));
        assert(bh->page_mask == bh->page_size - 1);
        AN(bh->rows);
        for (u = ROOT_IDX + 1; u < bh->next; u++) {
                v = parent(bh, u);
                assert(v < u);
                assert(v >= ROOT_IDX);
                fp1 = A(bh, u);
                fp2 = A(bh, v);
                AN(fp1);
                AN(fp2);
                assert(fp2->key <= fp1->key);
                assert(fp1->idx == u);
                assert(fp2->idx == v);
		assert(fp1->n < N);
		assert(fp2->n < N);
		assert(&ff[fp1->n] == fp1);
		assert(&ff[fp2->n] == fp2);
        }
}

#ifdef PARANOIA
#define paranoia_check(bh)      check_consistency(bh)
#else
#define paranoia_check(bh)      ((void)0)
#endif

static void
check_parent_child(struct binheap *bh, unsigned n_max)
{
        unsigned n, u, v;

        for (n = ROOT_IDX; n < n_max; n++) {
                child(bh, n, &u, &v);
                assert(u >= n);
                assert(u > ROOT_IDX);
                if (u == v) {
			if (u == UINT_MAX)
	                        continue;       /* child index is too big */
			v = parent(bh, u);
			assert(v == n);
		}
		else {
			assert(u + 1 == v);
                	v = parent(bh, u);
	                assert(v == n);
	                v = parent(bh, u + 1);
	                assert(v == n);
		}

		if (n == ROOT_IDX)
			continue;
                u = parent(bh, n);
                assert(u <= n);
		assert(u != BINHEAP_NOIDX);
                child(bh, u, &v, &u);
                assert(v == n || v == n - 1);
        }
}

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
        assert(fp->idx != BINHEAP_NOIDX);
        assert(fp->idx >= ROOT_IDX);
        assert(fp->idx < bh->next);
        assert(fp == A(bh, fp->idx));
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
        assert(fp->idx == BINHEAP_NOIDX);
        key = random() % R;
        fp->magic = FOO_MAGIC;
        fp->key = key;
        fp->n = n;
        binheap_insert(bh, fp);
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
        binheap_delete(bh, fp->idx);
        foo_check(fp);
        assert(fp->idx == BINHEAP_NOIDX);
        assert(fp->key == key);
        assert(fp->n == n);
        fp->key = 0;
        fp->n = 0;
        paranoia_check(bh);
}

static void
foo_reorder(struct binheap *bh, struct foo *fp)
{
        unsigned key, n;

        paranoia_check(bh);
        foo_check_existense(bh, fp);
        key = random() % R;
        n = fp->n;
        fp->key = key;
        binheap_reorder(bh, fp->idx);
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
        unsigned u, n, key;
        unsigned delete_count, insert_count, update_count;

	AZ(binheap_root(bh));
	check_consistency(bh);

	/* First insert our N elements */
        start = TIM_mono();
        for (n = 0; n < N; n++) {
        	foo_insert(bh, n);
                key = ff[n].key;
                fp = binheap_root(bh);
                foo_check(fp);
                assert(fp->idx == ROOT_IDX);
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
                assert(fp->idx == ROOT_IDX);
                assert(fp->key <= key);
                n = fp->n;
                foo_delete(bh, fp);
                foo_insert(bh, n);
                key = ff[n].key;
	}
        check_consistency(bh);
        end = TIM_mono();
        fprintf(stderr, "%u root replacements, %.3lfs OK\n", M, end - start);

	/* Randomly update */
	start = TIM_mono();
	for (u = 0; u < M; u++) {
		n = random() % N;
		fp = &ff[n];
		foo_reorder(bh, fp);
	}
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u random updates, %.3lfs OK\n", M, end - start);

        /* Randomly insert, delete and update */
        delete_count = 0;
        insert_count = 0;
        update_count = 0;
        start = TIM_mono();
        for (u = 0; u < M; u++) {
        	n = random() % N;
                fp = &ff[n];
                if (fp->idx != BINHEAP_NOIDX) {
                	if (fp->key & 1) {
                        	foo_delete(bh, fp);
                                ++delete_count;
			} else {
                        	foo_reorder(bh, fp);
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
                assert(fp->idx == ROOT_IDX);
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

static void
perftest(struct binheap *bh)
{
	double start, end;
	struct foo *fp;
	unsigned u, delete_count;

	AZ(binheap_root(bh));
	check_consistency(bh);
	for (u = 0; u < N; u++)
		ff[u].n = u;

	start = TIM_mono();
	for (u = 0; u < N; u++) {
		ff[u].key = random() % R;
		binheap_insert(bh, &ff[u]);
	}
	end = TIM_mono();
	fprintf(stderr, "perf %d inserts: %.3lfs\n", N, end - start);

	check_consistency(bh);
	start = TIM_mono();
	for (u = 0; u < M; u++) {
		fp = binheap_root(bh);
		binheap_delete(bh, fp->idx);
		ff[fp->n].key = random() % R;
		binheap_insert(bh, &ff[fp->n]);
	}
	end = TIM_mono();
	fprintf(stderr, "perf %d replacements: %.3lfs\n", M, end - start);

	check_consistency(bh);
	start = TIM_mono();
	for (u = 0; u < M; u++) {
		fp = &ff[random() % N];
		fp->key = random() % R;
		binheap_reorder(bh, fp->idx);
	}
	end = TIM_mono();
	fprintf(stderr, "perf %d reorders: %.3lfs\n", M, end - start);

	check_consistency(bh);
	start = TIM_mono();
	delete_count = 0;
	while (1) {
		fp = binheap_root(bh);
		if (fp == NULL)
			break;
		binheap_delete(bh, fp->idx);
		++delete_count;
	}
	end = TIM_mono();
	fprintf(stderr, "perf %d deletions: %.3lfs\n", delete_count, end - start);
	check_consistency(bh);

	for (u = 0; u < N; u++) {
		ff[u].idx = BINHEAP_NOIDX;
		ff[u].key = 0;
		ff[u].n = 0;
	}
}

int
main(int argc, char **argv)
{
        struct binheap *bh;
        unsigned u;

	for (u = 0; u < N; u++)
		ff[u].idx = BINHEAP_NOIDX;

        bh = binheap_new(NULL, cmp, update);
        AZ(binheap_root(bh));
	check_consistency(bh);

        check_parent_child(bh, M);
        fprintf(stderr, "parent-child test OK\n");

	test(bh);
	perftest(bh);
	while (1)
		test(bh);
        return (0);
}
#endif
