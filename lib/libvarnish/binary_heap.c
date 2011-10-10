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
 * The main feature of this binheap is minimizing the number of page faults
 * under memory pressure. Memory shortage is typical for Varnish setups.
 * Canonical binary heap (http://en.wikipedia.org/wiki/Binary_heap) isn't
 * suitable for this workload, since it requires P=log2(2*N/M)/2 average page
 * faults during binheap mutations, where N is the number of items in binheap
 * and M is the number of items per page. For example, P=5.5 on x32 and P=6
 * on x64 for N=2^20.
 * 'Slightly' modified parent-child index calculations used in the implementation
 * reduce average page faults count to P=log2(N)/log2(M/4)/2, i.e. P=1.25 on x32
 * and P=1.43 on x64, which means more than 4x speedup comparing to canonical
 * binary heap under memory pressure. See more details at:
 *	http://portal.acm.org/citation.cfm?doid=1785414.1785434
 *	(or: http://queue.acm.org/detail.cfm?id=1814327)
 *
 * But that's only the half of the story. Though previous implementation
 * already used enhanced parent-child index calculations, it had very serious
 * flaw as seen from minimizing page faults POV - compare() and update()
 * callbacks. Common binheap operations invoke 1.5*log2(N) such callbacks.
 * But what's wrong with them?
 *
 * Test driver can be built and run using the following commands:
 * $ cc -DTEST_DRIVER -I../.. -I../../include -lrt -lm binary_heap.c
 * $ ./a.out
 *
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
#define ROW_SHIFT		16


#undef PARANOIA

/* Private definitions -----------------------------------------------*/

#define ROW_WIDTH		(1 << ROW_SHIFT)

/*lint -emacro(572, ROW) shift 0 >> by 16 */
/*lint -emacro(835, ROW) 0 left of >> */
/*lint -emacro(778, ROW) const >> evaluates to zero */
#define ROW(bh, n)		((bh)->rows[(n) >> ROW_SHIFT])

/*lint -emacro(835, A) 0 left of & */
#define A(bh, n)		ROW(bh, n)[(n) & (ROW_WIDTH - 1)]

#define R_IDX(page_shift)	((1 << (page_shift)) - 1)
#define ROOT_IDX(bh)		R_IDX((bh)->page_shift)
#define NOIDX			0


#ifdef TEST_DRIVER

/* Memory model, which counts page faults */
struct mem {
	unsigned	magic;
#define MEM_MAGIC	0xf07c9610U
	uintptr_t	*lru;
	uintptr_t	page_mask;
	uint64_t	pagefaults_count;
	unsigned	resident_pages_count;
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
	m->magic = MEM_MAGIC;
	m->lru = NULL;
	m->page_mask = ~(page_size - 1);
	m->pagefaults_count = 0;
	m->resident_pages_count = 0;
	return m;
}

static void
init_mem(struct mem *m, unsigned resident_pages_count)
{
	CHECK_OBJ_NOTNULL(m, MEM_MAGIC);
	free(m->lru);
	m->lru = NULL;
	if (resident_pages_count > 0) {
		m->lru = calloc(resident_pages_count, sizeof(*m->lru));
		XXXAN(m->lru);
	}
	m->pagefaults_count = 0;
	m->resident_pages_count = resident_pages_count;
}

static void
access_mem(struct mem *m, void *p)
{
	uintptr_t addr, *lru;
	unsigned u, v;

	CHECK_OBJ_NOTNULL(m, MEM_MAGIC);
	if (m->resident_pages_count == 0)
		return;	/* mem model is disabled */
	if (p == NULL)
		return;	/* access to NULL is forbidden */

	addr = ((uintptr_t) p) & m->page_mask;
	lru = m->lru;
	for (u = 0; u < m->resident_pages_count; u++) {
		if (lru[u] == addr) {
			for (v = u; v >= 1; v--)
				lru[v] = lru[v - 1];
			lru[0] = addr;
			return;
		}
	}
	m->pagefaults_count++;
	for (v = m->resident_pages_count - 1; v >= 1; v--)
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
#define BINHEAP_MAGIC		0xf581581aU	/* from /dev/random */
	struct entry		**rows;
	struct binheap_entry	*free_list;
	struct binheap_entry	*malloc_list;
	unsigned		next;
	unsigned		length;
	unsigned		rows_count;
	unsigned		page_shift;
	TEST_DRIVER_DECLARE_MEM			/* no semicolon */
};

static unsigned
parent(unsigned page_shift, unsigned u)
{
	unsigned v, page_mask, page_size, page_leaves;

	assert(page_shift >= 2);
	assert(page_shift <= ROW_SHIFT);
	page_mask = R_IDX(page_shift);
	AZ(page_mask & (page_mask + 1));
	assert(u > page_mask);
	if (u <= page_mask + 4)
		return page_mask;
	v = u & page_mask;
	if (v >= 4)
		return u - v + v / 4 - 1;
	page_size = page_mask + 1;
	page_leaves = page_size - page_size / 4 + 1;
	assert((u >> page_shift) >= 2);
	v = (u >> page_shift) - 2;
	u = v / page_leaves + 2;
	return u * page_size + (v % page_leaves) - page_leaves;
}

static unsigned
child(unsigned page_shift, unsigned u)
{
	unsigned v, page_mask, page_size, page_leaves;

	assert(page_shift >= 2);
	assert(page_shift <= ROW_SHIFT);
	assert(u < UINT_MAX);
	page_mask = R_IDX(page_shift);
	AZ(page_mask & (page_mask + 1));
	assert(u >= page_mask);
	v = u & page_mask;
	page_size = page_mask + 1;
	if (v + 1 < page_size / 4)
		return u - v + (v + 1) * 4;
	page_leaves = page_size - page_size / 4 + 1;
	v += (u >> page_shift) * page_leaves + 2 - page_size;
	if (v > (UINT_MAX >> page_shift))
		return UINT_MAX;	/* child index is overflown */
	return page_size * v;
}

static struct entry *
alloc_row(unsigned page_shift)
{
	struct entry *row;
	size_t entry_size, alignment;
	unsigned u;
	int rv;

	assert(page_shift >= 2);
	assert(page_shift <= ROW_SHIFT);
	entry_size = sizeof(*row);
	AZ(entry_size & (entry_size - 1));	/* should be power of 2 */
	assert((1 << page_shift) < UINT_MAX / entry_size);
	alignment = (1 << page_shift) * entry_size;
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
	page_shift = 0U - 1;
	while (page_size) {
		page_size >>= 1;
		++page_shift;
	}
	assert(page_shift >= 2);
	page_size = 1 << page_shift;
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
	bh->malloc_list = NULL;
	bh->next = R_IDX(page_shift);
	bh->length = ROW_WIDTH;
	bh->rows_count = 1;
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
	const struct entry *e;
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
	const struct entry *e;
	unsigned v, i, j, last, min_key;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(u >= ROOT_IDX(bh));
	assert(u < bh->next);

	while (1) {
		v = child(bh->page_shift, u);
		assert(v > u);
		if (v >= bh->next)
			break;	/* reached the end of heap */
		assert(bh->next > v);
		last = bh->next - v;
		if (last > 4)
			last = 4;
		j = 4;
		min_key = key;
		for (i = 0; i < last; i++) {
			TEST_DRIVER_ACCESS_KEY(bh, v + i);
			e = &A(bh, v + i);
			AN(e->be);
			assert(e->be->idx == v + i);
			if (e->key < min_key) {
				min_key = e->key;
				j = i;
			}
		}
		if (min_key == key)
			break;
		assert(j < 4);
		e = &A(bh, v + j);
		assign(bh, e->be, e->key, u);
		assert(e->be->idx == u);
		assert(A(bh, u).be == e->be);
		assert(A(bh, u).key == e->key);
		u = v + j;
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
alloc_be_row(struct binheap_entry *prev_malloc_list)
{
	struct binheap_entry *row;
	unsigned u;

	/* TODO: determine the best row width */
	assert(ROW_WIDTH >= 2);
	row = calloc(ROW_WIDTH, sizeof(*row));
	XXXAN(row);

	/*
	 * Construct freelist from entries 1..ROW_WIDTH-1, while using
	 * the first entry for storing pointer to prev_malloc_list.
	 */
	for (u = 1; u < ROW_WIDTH; u++) {
		row[u].idx = NOIDX;
		row[u].p = row + u + 1;
	}
	row[ROW_WIDTH - 1].p = NULL;
	row->p = prev_malloc_list;
	return row;
}

static struct binheap_entry *
acquire_be(struct binheap *bh)
{
	struct binheap_entry *be;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	if (bh->free_list == NULL) {
		bh->malloc_list = alloc_be_row(bh->malloc_list);
		AN(bh->malloc_list);
		bh->free_list = bh->malloc_list + 1;
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
	be->idx = NOIDX;
	be->p = bh->free_list;
	bh->free_list = be;
}

static void
free_be_memory(struct binheap *bh)
{
	struct binheap_entry *be;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	/* memory can be freed up only on empty binheap */
	assert(bh->next == ROOT_IDX(bh));
	while (bh->malloc_list) {
		be = bh->malloc_list;
		bh->malloc_list = be->p;
		free(be);
	}
	bh->free_list = NULL;
}

struct binheap_entry *
binheap_insert(struct binheap *bh, void *p, unsigned key)
{
	struct binheap_entry *be;
	unsigned u, v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
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
	assert(bh->next > 0);
	if (u < --bh->next) {
		TEST_DRIVER_ACCESS_KEY(bh, bh->next);
		e = &A(bh, bh->next);
		key = e->key;
		be = e->be;
		AN(be);
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

	if (bh->next == ROOT_IDX(bh))
		free_be_memory(bh);
}

struct binheap_entry *
binheap_root(const struct binheap *bh)
{
	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	if (bh->next == ROOT_IDX(bh))
		return NULL;
	TEST_DRIVER_ACCESS_KEY(bh, ROOT_IDX(bh));
	return A(bh, ROOT_IDX(bh)).be;
}

void *
binheap_entry_unpack(const struct binheap *bh, const struct binheap_entry *be,
	unsigned *key_ptr)
{
	struct entry *e;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(be);
	assert(be->idx != NOIDX);
	assert(be->idx >= ROOT_IDX(bh));
	assert(be->idx < bh->next);
	AN(key_ptr);

	TEST_DRIVER_ACCESS_IDX(bh, be->idx);
	e = &A(bh, be->idx);
	assert(e->be == be);
	*key_ptr = e->key;
	return be->p;
}

#ifdef TEST_DRIVER

static void
check_time2key(void)
{
	assert(BINHEAP_TIME2KEY(-1e9) == 0);
	assert(BINHEAP_TIME2KEY(-1.0) == 0);
	assert(BINHEAP_TIME2KEY(-0.1) == 0);
	assert(BINHEAP_TIME2KEY(0.499) == 0);
	assert(BINHEAP_TIME2KEY(0.501) == 1);
	assert(BINHEAP_TIME2KEY(1.499) == 1);
	assert(BINHEAP_TIME2KEY(UINT_MAX * 1.0 - 0.6) == UINT_MAX - 1);
	assert(BINHEAP_TIME2KEY(UINT_MAX * 1.0 - 0.4) == UINT_MAX);
	assert(BINHEAP_TIME2KEY(UINT_MAX * 1.0 + 0.4) == UINT_MAX);
	assert(BINHEAP_TIME2KEY(UINT_MAX * 1.0 + 0.6) == UINT_MAX);
	assert(BINHEAP_TIME2KEY(UINT_MAX * 2.0) == UINT_MAX);
	assert(BINHEAP_TIME2KEY(UINT_MAX * 1000.0) == UINT_MAX);
}

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
	assert(bh->page_shift >= 2);
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
		assert(e1->be->idx == u);
		AN(e2->be);
		assert(e2->be->idx == v);
	}
}

static void
check_parent_child_range(unsigned page_shift, unsigned n_min, unsigned n_max)
{
	unsigned n, u, v, i, root_idx;

	assert(page_shift >= 2);
	root_idx = R_IDX(page_shift);
	assert(n_min > root_idx);
	for (n = n_min; n < n_max; n++) {
		u = child(page_shift, n);
		assert(u > n);
		if (u == UINT_MAX)
			continue;	/* child index is too big */
		for (i = 0; i < 4; i++) {
			v = parent(page_shift, u + i);
			assert(v == n);
		}

		u = parent(page_shift, n);
		assert(u < n);
		assert(u >= root_idx);
		v = child(page_shift, u);
		assert(v == (n & ~3U));
	}
}

static void
check_parent_child(unsigned page_shift, unsigned checks_count)
{
	unsigned n_min, n_max;

	assert(page_shift >= 2);
	/* check lower end of index range */
	assert(R_IDX(page_shift) < UINT_MAX - 1);
	n_min = 1 + R_IDX(page_shift);
	assert(checks_count < UINT_MAX - n_min);
	n_max = n_min + checks_count;
	check_parent_child_range(page_shift, n_min, n_max);

	/* check higher end of index range */
	n_min = UINT_MAX - checks_count;
	n_max = n_min + checks_count;
	assert(n_max == UINT_MAX);
	check_parent_child_range(page_shift, n_min, n_max);
}

/* Test driver -------------------------------------------------------*/
#include <math.h>
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

#define ITERATIONS_PER_TEST_COUNT	10000000
#define PARENT_CHILD_TESTS_COUNT	1000000
#define NULL_TESTS_COUNT		1000000
#define MAX_ITEMS_COUNT			1000000
#define MIN_ITEMS_COUNT			1000
#define TEST_STEPS_COUNT		5
#define MAX_RESIDENT_PAGES_COUNT	4096

/*
 * Pad foo so its' size is equivalent to the objcore size.
 * Currently size of objcore is 120 bytes on x64 and 56 bytes
 * on x32. This means that the padding should be 96 for x64
 * and 40 for x32.
 */
#define PADDING 96

#define MQPS(t, q)		((t) ? (q) / (t) / 1e6 : 0)
#define PF(bh)			\
	((double) (bh)->m->pagefaults_count - (bh)->m->resident_pages_count)
#define PF_PER_ITERATION(bh, iterations_count)	\
	(PF(bh) > 0 ? PF(bh) / iterations_count : 0)

#ifdef PARANOIA
#define paranoia_check(bh)	check_consistency(bh)
#else
#define paranoia_check(bh)	((void)0)
#endif

struct foo {
	unsigned		magic;
#define FOO_MAGIC	0x23239823U
	struct binheap_entry	*be;
	unsigned		key;
	unsigned		n;
	char padding[PADDING];
};

static struct foo *ff[MAX_ITEMS_COUNT];

static void
foo_check(const struct foo *fp, unsigned items_count)
{
	CHECK_OBJ_NOTNULL(fp, FOO_MAGIC);
	assert(fp->n < items_count);
	assert(fp == ff[fp->n]);
}

static void
foo_check_existence(struct binheap *bh, const struct foo *fp,
	unsigned items_count)
{
	foo_check(fp, items_count);
	AN(fp->be);
	assert(fp->be->idx != NOIDX);
	assert(fp->be->idx >= ROOT_IDX(bh));
	assert(fp->be->idx < bh->next);
	assert(fp->be->p == fp);
	assert(fp->be == A(bh, fp->be->idx).be);
	assert(fp->key == A(bh, fp->be->idx).key);
}

static void
foo_insert(struct binheap *bh, unsigned n, unsigned items_count)
{
	struct foo *fp;
	unsigned key;

	paranoia_check(bh);
	assert(n < items_count);
	AZ(ff[n]);
	fp = ff[n] = malloc(sizeof(*fp));
	XXXAN(fp);
	key = (unsigned) random();
	fp->magic = FOO_MAGIC;
	fp->key = key;
	fp->n = n;
	fp->be = binheap_insert(bh, fp, key);
	foo_check_existence(bh, fp, items_count);
	assert(fp->key == key);
	assert(fp->n == n);
	paranoia_check(bh);
}

static void
foo_delete(struct binheap *bh, struct foo *fp, unsigned items_count)
{
	unsigned key, n;

	paranoia_check(bh);
	foo_check_existence(bh, fp, items_count);
	key = fp->key;
	n = fp->n;
	binheap_delete(bh, fp->be);
	foo_check(fp, items_count);
	AN(fp->be);
	assert(fp->key == key);
	assert(fp->n == n);
	free(fp);
	ff[n] = NULL;
	paranoia_check(bh);
}

static void
foo_reorder(struct binheap *bh, struct foo *fp, unsigned items_count)
{
	unsigned key, n;

	paranoia_check(bh);
	foo_check_existence(bh, fp, items_count);
	key = (unsigned) random();
	n = fp->n;
	fp->key = key;
	binheap_reorder(bh, fp->be, key);
	foo_check_existence(bh, fp, items_count);
	assert(fp->key == key);
	assert(fp->n == n);
	paranoia_check(bh);
}

static void
test(struct binheap *bh, unsigned items_count, unsigned resident_pages_count)
{
	double start, end;
	struct binheap_entry *be;
	struct foo *fp;
	unsigned u, n, key, deleted_key, root_idx, iterations_count;
	unsigned delete_count, insert_count, reorder_count;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(items_count >= MIN_ITEMS_COUNT);
	assert(items_count <= MAX_ITEMS_COUNT);
	iterations_count = ITERATIONS_PER_TEST_COUNT;
	assert(iterations_count >= items_count);

	fprintf(stderr, "\n+ %u items, %u iterations, %u resident pages\n",
		items_count, iterations_count, resident_pages_count);
	AZ(binheap_root(bh));
	check_consistency(bh);
	root_idx = ROOT_IDX(bh);
	assert(root_idx != NOIDX);

	/* Insert our items */
	key = 0;
	start = TIM_mono();
	init_mem(bh->m, resident_pages_count);
	for (n = 0; n < items_count; n++) {
		foo_insert(bh, n, items_count);
		be = binheap_root(bh);
		AN(be);
		fp = binheap_entry_unpack(bh, be, &key);
		foo_check(fp, items_count);
		assert(fp->key == key);
		assert(fp->be->idx == root_idx);
		assert(key <= ff[n]->key);
	}
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u inserts: %.3lf Mqps, "
		"%.3lf pagefaults per iteration\n",
		items_count, MQPS(end - start, items_count),
		PF_PER_ITERATION(bh, items_count));

	/* For M cycles, pick the root, insert new */
	n = 0;
	start = TIM_mono();
	init_mem(bh->m, resident_pages_count);
	for (u = 0; u < iterations_count; u++) {
		be = binheap_root(bh);
		AN(be);
		fp = binheap_entry_unpack(bh, be, &key);
		foo_check(fp, items_count);
		assert(fp->key == key);
		assert(fp->be->idx == root_idx);
		assert(key <= ff[n]->key);
		n = fp->n;
		foo_delete(bh, fp, items_count);
		foo_insert(bh, n, items_count);
	}
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u root replacements: %.3lf Mqps, "
		"%.3lf pagefaults per iteration\n", iterations_count,
		MQPS(end - start, iterations_count),
		PF_PER_ITERATION(bh, iterations_count));

	/* Randomly reorder */
	start = TIM_mono();
	init_mem(bh->m, resident_pages_count);
	for (u = 0; u < iterations_count; u++) {
		n = random() % items_count;
		fp = ff[n];
		foo_reorder(bh, fp, items_count);
	}
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u random reorders: %.3lf Mqps, "
		"%.3lf pagefaults per iteration\n", iterations_count,
		MQPS(end - start, iterations_count),
		PF_PER_ITERATION(bh, iterations_count));

	/* Randomly insert, delete and reorder */
	delete_count = 0;
	insert_count = 0;
	reorder_count = 0;
	start = TIM_mono();
	init_mem(bh->m, resident_pages_count);
	for (u = 0; u < iterations_count; u++) {
		n = random() % items_count;
		fp = ff[n];
		if (fp != NULL) {
			if (fp->key & 1) {
				foo_delete(bh, fp, items_count);
				++delete_count;
			} else {
				foo_reorder(bh, fp, items_count);
				++reorder_count;
			}
		} else {
			foo_insert(bh, n, items_count);
			++insert_count;
		}
	}
	assert(delete_count >= insert_count);
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr,
		"%u deletes, %u inserts, %u reorders: %.3lf Mqps, "
		"%.3lf pagefaults per iteration\n",
		delete_count, insert_count, reorder_count,
		MQPS(end - start, iterations_count),
		PF_PER_ITERATION(bh, iterations_count));

	/* Then remove everything */
	deleted_key = 0;
	u = 0;
	start = TIM_mono();
	init_mem(bh->m, resident_pages_count);
	while (1) {
		be = binheap_root(bh);
		if (be == NULL)
			break;
		fp = binheap_entry_unpack(bh, be, &key);
		foo_check(fp, items_count);
		assert(fp->key == key);
		assert(fp->be->idx == root_idx);
		assert(key >= deleted_key);
		deleted_key = key;
		foo_delete(bh, fp, items_count);
		++u;
	}
	assert(u == items_count - (delete_count - insert_count));
	AZ(binheap_root(bh));
	check_consistency(bh);
	end = TIM_mono();
	fprintf(stderr, "%u deletes: %.3lf Mqps, "
		"%.3lf pagefaults per iteration\n",
		u, MQPS(end - start, u), PF_PER_ITERATION(bh, u));
}

static void
run_tests(struct binheap *bh, unsigned resident_pages_count)
{
	double k;
	unsigned u, items_count;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(MIN_ITEMS_COUNT > 0);
	assert(MAX_ITEMS_COUNT > MIN_ITEMS_COUNT);
	k = log(((double) MAX_ITEMS_COUNT) / MIN_ITEMS_COUNT);
	assert(TEST_STEPS_COUNT > 1);
	k /= (TEST_STEPS_COUNT - 1);
	test(bh, MIN_ITEMS_COUNT, resident_pages_count);
	for (u = 1; u < TEST_STEPS_COUNT - 1; u++) {
		items_count = (unsigned) (MIN_ITEMS_COUNT * exp(k * u));
		test(bh, items_count, resident_pages_count);
	}
	test(bh, MAX_ITEMS_COUNT, resident_pages_count);
}

static void
null_test(struct binheap *bh, unsigned iterations_count)
{
	struct binheap_entry *be;
	void *p;
	unsigned u, key1, key2;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);

	/* First check is it possible to insert NULLs */
	for (u = 0; u < iterations_count; u++) {
		key1 = (unsigned) random();
		be = binheap_insert(bh, NULL, key1);
		p = binheap_entry_unpack(bh, be, &key2);
		AZ(p);
		assert(key1 == key2);
	}
	check_consistency(bh);
	u = 0;
	while (1) {
		be = binheap_root(bh);
		if (be == NULL)
			break;
		binheap_delete(bh, be);
		++u;
	}
	assert(u == iterations_count);
	AZ(binheap_root(bh));
	check_consistency(bh);
}

int
main(int argc, char **argv)
{
	struct binheap *bh;
	unsigned u;

	srandom(123);	/* generate predictive results */
	check_time2key();
	fprintf(stderr, "time2key test OK\n");

	for (u = 2; u <= ROW_SHIFT; u++) {
		check_parent_child(u, PARENT_CHILD_TESTS_COUNT);
	}
	fprintf(stderr, "%u parent-child tests OK\n", PARENT_CHILD_TESTS_COUNT);

	bh = binheap_new();
	AZ(binheap_root(bh));
	check_consistency(bh);

	null_test(bh, NULL_TESTS_COUNT);
	fprintf(stderr, "%u null tests OK\n", NULL_TESTS_COUNT);

	fprintf(stderr, "\n* Tests with pagefault counter enabled\n");
	for (u = 1; u <= UINT_MAX / 2 && u <= MAX_RESIDENT_PAGES_COUNT; u *= 2)
		run_tests(bh, u);

	fprintf(stderr, "\n* Tests with pagefault counter disabled "
			"(aka 'perftests')\n");
	run_tests(bh, 0);
	return (0);
}
#endif
