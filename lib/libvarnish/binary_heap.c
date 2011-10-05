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

#define R_IDX(page_shift)	((1u << (page_shift)) - 1)
#define ROOT_IDX(bh)		R_IDX((bh)->page_shift)
#define NOIDX			0u


#ifdef TEST_DRIVER

/*
 * The maximum number of resident pages in memory.
 * Other pages are swapped out in lru order.
 */
#define RESIDENT_PAGES_COUNT	1u

/* Memory model, which counts page faults */
struct mem {
	uintptr_t *lru;
	uintptr_t page_mask;
	uint64_t page_faults;
	unsigned is_enabled;
};

static struct mem *
create_mem(void)
{
	struct mem *m;
	uintptr_t page_size;

	page_size = (uintptr_t) getpagesize();
	xxxassert(page_size > 0);
	XXXAZ(page_size & (page_size - 1));

	m = malloc(sizeof(*m));
	XXXAN(m);
	m->lru = calloc(RESIDENT_PAGES_COUNT, sizeof(*m->lru));
	XXXAN(m->lru);
	m->page_mask = ~(page_size - 1);
	m->page_faults = 0;
	m->is_enabled = 0;
	return m;
}

static void
enable_mem(struct mem *m)
{
	m->is_enabled = 1;
}

static void
disable_mem(struct mem *m)
{
	m->is_enabled = 0;
}

static void
clear_mem(struct mem *m)
{
	unsigned u;

	for (u = 0; u < RESIDENT_PAGES_COUNT; u++)
		m->lru[u] = 0;
	m->page_faults = 0;
}

static void
access_mem(struct mem *m, void *p)
{
	uintptr_t addr, *lru;
	unsigned u, v;

	AN(m);
	assert(RESIDENT_PAGES_COUNT > 0);
	if (!m->is_enabled)
		return;

	addr = ((uintptr_t) p) & m->page_mask;
	lru = m->lru;
	for (u = 0; u < RESIDENT_PAGES_COUNT; u++) {
		if (lru[u] == addr) {
			for (v = u; v >= 1; v--)
				lru[v] = lru[v - 1];
			lru[0] = addr;
			return;
		}
	}
	m->page_faults++;
	for (v = RESIDENT_PAGES_COUNT - 1; v >= 1; v--)
		lru[v] = lru[v - 1];
	lru[0] = addr;
}

#define TEST_DRIVER_DECLARE_MEM		struct mem *m;	/* semicolon */
#define TEST_DRIVER_CREATE_MEM(bh)	(bh)->m = create_mem()
#define TEST_DRIVER_ACCESS_MEM(bh, p)	access_mem((bh)->m, (p))
#else
#define TEST_DRIVER_DECLARE_MEM		/* nothing */
#define TEST_DRIVER_CREATE_MEM(bh)	((void)0)
#define TEST_DRIVER_ACCESS_MEM(bh, p)	((void)0)
#endif

#define TEST_DRIVER_ACCESS_KEY(bh, u)	TEST_DRIVER_ACCESS_MEM(bh, &A(bh, u))
#define TEST_DRIVER_ACCESS_IDX(bh, u)	do { \
	TEST_DRIVER_ACCESS_KEY(bh, u); \
	TEST_DRIVER_ACCESS_MEM(bh, A(bh, u).be); \
} while (0)

struct binheap_entry {
	unsigned idx;
	void *p;
};

/*
 * Storing key near p should improve memory locality
 * for hot paths during binheap mutations.
 * Code below expects sizeof(entry) is a power of two.
 */
struct entry {
	unsigned key;
	struct binheap_entry *be;
};

struct binheap {
	unsigned		magic;
#define BINHEAP_MAGIC		0x8bd801f0u	/* from /dev/random */
	struct entry		**rows;
	struct binheap_entry	*free_list;
	unsigned		next;
	unsigned		rows_count;
	unsigned		length;
	unsigned		page_shift;
	TEST_DRIVER_DECLARE_MEM			/* no semicolon */
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

static struct entry *
alloc_row(unsigned page_shift)
{
	struct entry *row;
	size_t entry_size, alignment;
	unsigned u;
	int rv;

	assert(page_shift > 0);
	assert(page_shift <= ROW_SHIFT);
	entry_size = sizeof(*row);
	AZ(entry_size & (entry_size - 1));	/* should be power of 2 */
	assert((1u << page_shift) < UINT_MAX / entry_size);
	alignment = (1u << page_shift) * entry_size;
	assert(ROW_WIDTH <= SIZE_MAX / entry_size);
	row = NULL;
	rv = posix_memalign((void **) &row, alignment, entry_size * ROW_WIDTH);
	XXXAZ(rv);
	AN(row);
	AZ(((uintptr_t) row) & (alignment - 1));
	/* null out entries */
	for (u = 0; u < ROW_WIDTH; u++) {
		row[u].key = 0;
		row[u].be = NULL;
	}
	return row;
}

struct binheap *
binheap_new(void)
{
	struct entry **rows;
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
	TEST_DRIVER_CREATE_MEM(bh);

	/*
	 * Make sure the page with embedded binheap don't overlap with
	 * binheap entries, which start from R_IDX(page_shift) index.
	 */
	xxxassert(sizeof(*bh) <= sizeof(**rows) * R_IDX(page_shift));
	return bh;
}

static void
assign(const struct binheap *bh, struct binheap_entry *be, unsigned key,
	unsigned idx)
{
	struct entry *e;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(be);
	assert(idx != NOIDX);
	assert(idx >= ROOT_IDX(bh));
	assert(idx < bh->next);
	TEST_DRIVER_ACCESS_IDX(bh, idx);
	e = &A(bh, idx);
	e->key = key;
	e->be = be;
	be->idx = idx;
}

static unsigned
trickleup(const struct binheap *bh, unsigned key, unsigned u)
{
	struct entry *e;
	unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(u >= ROOT_IDX(bh));
	assert(u < bh->next);

	while (u != ROOT_IDX(bh)) {
		v = parent(bh->page_shift, u);
		assert(v < u);
		assert(v >= ROOT_IDX(bh));
		TEST_DRIVER_ACCESS_KEY(bh, v);
		e = &A(bh, v);
		AN(e->be);
		AN(e->be->p);
		assert(e->be->idx == v);
		if (e->key < key)
			break;	/* parent is smaller than the child */
		assign(bh, e->be, e->key, u);
		assert(e->be->idx == u);
		assert(A(bh, u).be == e->be);
		assert(A(bh, u).key == e->key);
		u = v;
	}
	return u;
}

static unsigned
trickledown(const struct binheap *bh, unsigned key, unsigned u)
{
	struct entry *e1, *e2;
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
		TEST_DRIVER_ACCESS_KEY(bh, v);
		e1 = &A(bh, v);
		AN(e1->be);
		AN(e1->be->p);
		assert(e1->be->idx == v);
		if (v + 1 < bh->next) {
			TEST_DRIVER_ACCESS_KEY(bh, v + 1);
			e2 = &A(bh, v + 1);
			AN(e2->be);
			AN(e2->be->p);
			assert(e2->be->idx == v + 1);
			if (e2->key < e1->key) {
				++v;
				e1 = e2;
			}
		}
		assert(v < bh->next);
		if (key < e1->key)
			break;	/* parent is smaller than children */
		assign(bh, e1->be, e1->key, u);
		assert(e1->be->idx == u);
		assert(A(bh, u).be == e1->be);
		assert(A(bh, u).key == e1->key);
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

static struct binheap_entry *
alloc_bi_row(void)
{
	struct binheap_entry *row;
	unsigned u;

	/* TODO: determine the best row width */
	row = calloc(ROW_WIDTH, sizeof(*row));
	XXXAN(row);

	/* construct freelist from entries in the row */
	for (u = 0; u < ROW_WIDTH; u++) {
		row[u].idx = NOIDX;
		row[u].p = row + u + 1;
	}
	row[ROW_WIDTH - 1].p = NULL;
	return row;
}

static struct binheap_entry *
acquire_be(struct binheap *bh)
{
	struct binheap_entry *be;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	if (bh->free_list == NULL) {
		bh->free_list = alloc_bi_row();
		AN(bh->free_list);
	}
	be = bh->free_list;
	bh->free_list = be->p;
	AN(be);
	assert(be->idx == NOIDX);
	be->p = NULL;
	return be;
}

static void
release_be(struct binheap *bh, struct binheap_entry *be)
{
	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(be);
	assert(be->idx != NOIDX);
	AN(be->p);
	be->idx = NOIDX;
	be->p = bh->free_list;
	bh->free_list = be;
	/* TODO: free up memory? */
}

struct binheap_entry *
binheap_insert(struct binheap *bh, void *p, unsigned key)
{
	struct binheap_entry *be;
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
	AZ(A(bh, u).be);
	AZ(A(bh, u).key);
	v = trickleup(bh, key, u);
	assert(v <= u);
	assert(v >= ROOT_IDX(bh));
	be = acquire_be(bh);
	AN(be);
	assert(be->idx == NOIDX);
	AZ(be->p);
	be->p = p;
	assign(bh, be, key, v);
	assert(be->idx == v);
	assert(A(bh, v).be == be);
	assert(A(bh, v).key == key);
	return be;
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
binheap_reorder(const struct binheap *bh, struct binheap_entry *be,
	unsigned key)
{
	struct entry *e;
	unsigned u, v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(bh->next >= ROOT_IDX(bh));
	AN(be);
	AN(be->p);
	u = be->idx;
	assert(u != NOIDX);
	assert(u >= ROOT_IDX(bh));
	assert(u < bh->next);
	e = &A(bh, u);
	assert(e->be == be);
	v = reorder(bh, key, u);
	if (u != v)
		assign(bh, be, key, v);
	else
		e->key = key;
	assert(be->idx == v);
	assert(A(bh, v).be == be);
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
binheap_delete(struct binheap *bh, struct binheap_entry *be)
{
	struct entry *e;
	unsigned u, v, key;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(bh->next > ROOT_IDX(bh));
	assert(bh->next <= bh->length);
	AN(be);
	AN(be->p);
	u = be->idx;
	assert(u != NOIDX);
	assert(u >= ROOT_IDX(bh));
	assert(u < bh->next);
	TEST_DRIVER_ACCESS_IDX(bh, u);
	e = &A(bh, u);
	assert(e->be == be);
	e->key = 0;
	e->be = NULL;
	release_be(bh, be);
	assert(be->idx == NOIDX);
	assert(bh->next > 0);
	if (u < --bh->next) {
		TEST_DRIVER_ACCESS_KEY(bh, bh->next);
		e = &A(bh, bh->next);
		key = e->key;
		be = e->be;
		AN(be);
		AN(be->p);
		assert(be->idx == bh->next);
		e->key = 0;
		e->be = NULL;
		v = reorder(bh, key, u);
		assign(bh, be, key, v);
		assert(be->idx == v);
		assert(A(bh, v).be == be);
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
	struct binheap_entry *be;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	TEST_DRIVER_ACCESS_IDX(bh, ROOT_IDX(bh));
	be = A(bh, ROOT_IDX(bh)).be;
	if (be == NULL) {
		assert(bh->next == ROOT_IDX(bh));
		return NULL;
	}
	assert(be->idx == ROOT_IDX(bh));
	AN(be->p);
	return be->p;
}

#ifdef TEST_DRIVER

static void
check_consistency(const struct binheap *bh)
{
	struct entry *e1, *e2;
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
		e1 = &A(bh, u);
		e2 = &A(bh, v);
		assert(e2->key <= e1->key);
		AN(e2->be);
		AN(e2->be->p);
		assert(e1->be->idx == u);
		AN(e2->be);
		AN(e2->be->p);
		assert(e2->be->idx == v);
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
			continue;	/* child index is too big */
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
#define paranoia_check(bh)	check_consistency(bh)
#else
#define paranoia_check(bh)	((void)0)
#endif

struct foo {
	unsigned		magic;
#define FOO_MAGIC	0x23239823u
	unsigned		key;
	unsigned		n;
	struct binheap_entry	*be;
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
	AN(fp->be);
	assert(fp->be->idx != NOIDX);
	assert(fp->be->idx >= ROOT_IDX(bh));
	assert(fp->be->idx < bh->next);
	assert(fp->be->p == fp);
	assert(fp->be == A(bh, fp->be->idx).be);
	assert(fp->key == A(bh, fp->be->idx).key);
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
	AZ(fp->be);
	key = random() % R;
	fp->magic = FOO_MAGIC;
	fp->key = key;
	fp->n = n;
	fp->be = binheap_insert(bh, fp, key);
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
	binheap_delete(bh, fp->be);
	foo_check(fp);
	AN(fp->be);
	assert(fp->be->idx == NOIDX);
	assert(fp->key == key);
	assert(fp->n == n);
	fp->key = 0;
	fp->n = 0;
	fp->be = NULL;
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
	binheap_reorder(bh, fp->be, key);
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
	enable_mem(bh->m);

	/* First insert our N elements */
	start = TIM_mono();
	clear_mem(bh->m);
	for (n = 0; n < N; n++) {
		foo_insert(bh, n);
		key = ff[n].key;
		fp = binheap_root(bh);
		foo_check(fp);
		assert(fp->be->idx == root_idx);
		assert(fp->key <= key);
	}
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u inserts: %.3lfs page_faults=%lu OK\n", N,
		end - start, bh->m->page_faults);

	/* For M cycles, pick the root, insert new */
	start = TIM_mono();
	clear_mem(bh->m);
	for (u = 0; u < M; u++) {
		fp = binheap_root(bh);
		foo_check(fp);
		assert(fp->be->idx == root_idx);
		assert(fp->key <= key);
		n = fp->n;
		foo_delete(bh, fp);
		foo_insert(bh, n);
		key = ff[n].key;
	}
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u root replacements: %.3lfs, page_faults=%lu OK\n", M,
		end - start, bh->m->page_faults);

	/* Randomly update */
	start = TIM_mono();
	clear_mem(bh->m);
	for (u = 0; u < M; u++) {
		n = random() % N;
		fp = &ff[n];
		foo_update(bh, fp);
	}
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u random updates: %.3lfs, page_faults=%lu OK\n", M,
		end - start, bh->m->page_faults);

	/* Randomly insert, delete and update */
	delete_count = 0;
	insert_count = 0;
	update_count = 0;
	start = TIM_mono();
	clear_mem(bh->m);
	for (u = 0; u < M; u++) {
		n = random() % N;
		fp = &ff[n];
		if (fp->be != NULL) {
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
		"%u deletes, %u inserts, %u updates: %.3lfs, "
		"page_faults=%lu OK\n",
		delete_count, insert_count, update_count, end - start,
		bh->m->page_faults);

	/* Then remove everything */
	key = 0;
	u = 0;
	start = TIM_mono();
	clear_mem(bh->m);
	while (1) {
		fp = binheap_root(bh);
		if (fp == NULL)
			break;
		foo_check(fp);
		assert(fp->be->idx == root_idx);
		assert(fp->key >= key);
		key = fp->key;
		foo_delete(bh, fp);
		++u;
	}
	assert(u == N - (delete_count - insert_count));
	AZ(binheap_root(bh));
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u deletes: %.3lfs, page_faults=%lu OK\n", u,
		end - start, bh->m->page_faults);

	disable_mem(bh->m);
}

static void
perftest(struct binheap *bh)
{
	double start, end;
	struct foo *fp;
	unsigned u, delete_count;

	AZ(binheap_root(bh));
	check_consistency(bh);

	start = TIM_mono();
	for (u = 0; u < N; u++) {
		ff[u].be = binheap_insert(bh, &ff[u], random() % R);
	}
	end = TIM_mono();
	fprintf(stderr, "perf %d inserts: %.3lfs\n", N, end - start);

	check_consistency(bh);
	start = TIM_mono();
	for (u = 0; u < M; u++) {
		fp = binheap_root(bh);
		binheap_delete(bh, fp->be);
		fp->be = binheap_insert(bh, fp, random() % R);
	}
	end = TIM_mono();
	fprintf(stderr, "perf %d replacements: %.3lfs\n", M, end - start);

	check_consistency(bh);
	start = TIM_mono();
	for (u = 0; u < M; u++) {
		fp = &ff[random() % N];
		binheap_reorder(bh, fp->be, random() % R);
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
		binheap_delete(bh, fp->be);
		++delete_count;
	}
	end = TIM_mono();
	fprintf(stderr, "perf %d deletions: %.3lfs\n", delete_count,
		end - start);
	check_consistency(bh);

	for (u = 0; u < N; u++) {
		ff[u].be = NULL;
		ff[u].key = 0;
		ff[u].n = 0;
	}
}

int
main(int argc, char **argv)
{
	struct binheap *bh;
	unsigned u;

	for (u = 1; u <= ROW_SHIFT; u++) {
		check_parent_child(u, M);
		check_parent_child_overflow(u, M);
	}
	fprintf(stderr, "parent-child test OK\n");

	bh = binheap_new();
	AZ(binheap_root(bh));
	check_consistency(bh);

	test(bh);
	perftest(bh);
	while (1)
		test(bh);
	return 0;
}
#endif
