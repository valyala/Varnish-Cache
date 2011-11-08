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
 * Test driver can be built and run using the following commands:
 * $ cc -DTEST_DRIVER -I../.. -I../../include -lrt -lm binary_heap.c
 * $ ./a.out
 *
 */

#include "config.h"

#include <errno.h>
#include <limits.h>
#include <math.h>		// for testcase
#include <stdint.h>
#include <stdio.h>		// for testcase
#include <stdlib.h>
#include <time.h>		// for testcase
#include <unistd.h>

#include "binary_heap.h"
#include "miniobj.h"
#include "vas.h"

/* Private definitions -----------------------------------------------*/

#define ROOT_IDX		1

#define A(bh, n)		((bh)->array[n])

#ifdef TEST_DRIVER

/*
 * The memory model is used by test driver in order to count an approximate
 * number of page faults induced by binheap mutations.
 * The model keeps recently accessed pages in lru list
 * of resident_pages_count size.
 * It uses flat array for lru list implementation, since it is simple and
 * it works quite fast for small sizes.
 */
struct mem {
	MAGIC_HERE;
#define MEM_MAGIC	0xf07c9610U

	uintptr_t	*lru;
	uintptr_t	page_mask;
	uint64_t	pagefaults_count;
	unsigned	resident_pages_count;
};

static struct mem m;

static void
init_mem(unsigned resident_pages_count)
{
	uintptr_t page_size;

	page_size = (uintptr_t) getpagesize();
	xxxassert(page_size > 0);
	XXXAZ(page_size & (page_size - 1));

	SET_MAGIC(&m, MEM_MAGIC);
	FREE_ORNULL(m.lru);
	m.lru = NULL;
	if (resident_pages_count > 0)
		CALLOC_NOTNULL(m.lru, resident_pages_count);
	m.page_mask = ~(page_size - 1);
	m.pagefaults_count = 0;
	m.resident_pages_count = resident_pages_count;
}

static void
access_mem(const void *p)
{
	uintptr_t addr, *lru;
	unsigned u, v;

	CHECK_OBJ_NOTNULL(&m, MEM_MAGIC);
	if (m.resident_pages_count == 0)
		return;	/* mem model is disabled */
	if (p == NULL)
		return;	/* access to NULL is forbidden */

	addr = ((uintptr_t) p) & m.page_mask;
	lru = m.lru;
	for (u = 0; u < m.resident_pages_count; u++) {
		if (lru[u] == addr) {
			for (v = u; v >= 1; v--)
				lru[v] = lru[v - 1];
			lru[0] = addr;
			return;
		}
	}
	m.pagefaults_count++;
	for (v = m.resident_pages_count - 1; v >= 1; v--)
		lru[v] = lru[v - 1];
	lru[0] = addr;
}

#define TEST_DRIVER_ACCESS_MEM(p)	access_mem(p)
#else
#define TEST_DRIVER_ACCESS_MEM(p)	((void)0)
#endif

#define TEST_DRIVER_ACCESS_KEY(bh, u)	TEST_DRIVER_ACCESS_MEM(&A(bh, u))

struct binheap {
	MAGIC_HERE;
#define BINHEAP_MAGIC		0xf581581aU	/* from /dev/random */

	binheap_cmp_t		cmp_f;
	binheap_update_t	update_f;
	void			**array;
	unsigned		next;
	unsigned		length;
};

static unsigned
parent(unsigned u)
{
	assert(u > ROOT_IDX);
	return (u / 2);
}

static unsigned
child(unsigned u)
{
	assert(u >= ROOT_IDX);
	assert(u < UINT_MAX);
	if (u > UINT_MAX / 2)
		return (UINT_MAX);
	return (u * 2);
}

struct binheap *
binheap_new(binheap_cmp_t cmp_f, binheap_update_t update_f)
{
	struct binheap *bh;

	AN(cmp_f);
	AN(update_f);
	ALLOC_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	bh->cmp_f = cmp_f;
	bh->update_f = update_f;
	bh->length = 16;
	CALLOC_NOTNULL(bh->array, bh->length);
	bh->next = ROOT_IDX;
	return (bh);
}

static void
assign(const struct binheap *bh, void *p, unsigned u)
{
	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(p);
	assert(u != BINHEAP_NOIDX);
	assert(u >= ROOT_IDX);
	assert(u < bh->next);
	TEST_DRIVER_ACCESS_KEY(bh, u);
	A(bh, u) = p;
	AN(bh->update_f);
	TEST_DRIVER_ACCESS_MEM(p);
	bh->update_f(p, u);
}

static unsigned
trickleup(const struct binheap *bh, void *p1, unsigned u)
{
	void *p2;
	unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(p1);
	assert(u >= ROOT_IDX);
	assert(u < bh->next);
	AN(bh->cmp_f);

	while (u != ROOT_IDX) {
		v = parent(u);
		assert(v < u);
		assert(v >= ROOT_IDX);
		TEST_DRIVER_ACCESS_KEY(bh, v);
		p2 = A(bh, v);
		AN(p2);
		TEST_DRIVER_ACCESS_MEM(p1);
		TEST_DRIVER_ACCESS_MEM(p2);
		if (bh->cmp_f(p2, p1))
			break;	/* parent is smaller than the child */
		assign(bh, p2, u);
		assert(A(bh, u) == p2);
		u = v;
	}
	return (u);
}

static unsigned
trickledown(const struct binheap *bh, void *p1, unsigned u)
{
	void *p2, *p3;
	unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(p1);
	assert(u >= ROOT_IDX);
	assert(u < bh->next);
	AN(bh->cmp_f);

	while (1) {
		v = child(u);
		assert(v > u);
		if (v >= bh->next)
			break;	/* reached the end of heap */
		assert(v < bh->next);
		TEST_DRIVER_ACCESS_KEY(bh, v);
		p2 = A(bh, v);
		if (v + 1 < bh->next) {
			TEST_DRIVER_ACCESS_KEY(bh, v + 1);
			p3 = A(bh, v + 1);
			AN(p3);
			TEST_DRIVER_ACCESS_MEM(p2);
			TEST_DRIVER_ACCESS_MEM(p3);
			if (bh->cmp_f(p3, p2)) {
				p2 = p3;
				v++;
			}
		}
		TEST_DRIVER_ACCESS_MEM(p1);
		TEST_DRIVER_ACCESS_MEM(p2);
		if (!bh->cmp_f(p2, p1))
			break;
		assign(bh, p2, u);
		assert(A(bh, u) == p2);
		u = v;
	}
	return (u);
}

static void
increase_array_size(struct binheap *bh)
{
	unsigned length;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(bh->length > 0);
	assert(bh->next == bh->length);
	xxxassert(bh->length <= UINT_MAX / 2);
	length = bh->length * 2;
	AN(bh->array);
	REALLOC_ARRAY_NOTNULL(bh->array, length);
	while (bh->length < length)
		bh->array[bh->length++] = NULL;
}

void
binheap_insert(struct binheap *bh, void *p)
{
	unsigned u, v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(p);
	assert(bh->next >= ROOT_IDX);
	assert(bh->next <= bh->length);
	if (bh->length == bh->next)
		increase_array_size(bh);
	assert(bh->length > bh->next);
	assert(bh->next < UINT_MAX);
	u = bh->next++;
	AZ(A(bh, u));
	v = trickleup(bh, p, u);
	assert(v <= u);
	assert(v >= ROOT_IDX);
	assign(bh, p, v);
	assert(A(bh, v) == p);
}

static unsigned
reorder(const struct binheap *bh, void *p, unsigned u)
{
	unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	AN(p);
	assert(bh->next >= ROOT_IDX);
	assert(u >= ROOT_IDX);
	assert(u < bh->next);
	v = trickleup(bh, p, u);
	assert(v >= ROOT_IDX);
	assert(v <= u);
	if (v == u) {
		v = trickledown(bh, p, u);
		assert(v >= u);
		assert(v < bh->next);
	}
	return (v);
}

void
binheap_reorder(const struct binheap *bh, unsigned u)
{
	void *p;
	unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(bh->next >= ROOT_IDX);
	assert(u != BINHEAP_NOIDX);
	assert(u >= ROOT_IDX);
	assert(u < bh->next);
	TEST_DRIVER_ACCESS_KEY(bh, u);
	p = A(bh, u);
	AN(p);
	v = reorder(bh, p, u);
	if (u != v)
		assign(bh, p, v);
	assert(A(bh, v) == p);
}

void
binheap_delete(struct binheap *bh, unsigned u)
{
	void *p;
	unsigned v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(bh->next > ROOT_IDX);
	assert(bh->next <= bh->length);
	assert(u != BINHEAP_NOIDX);
	assert(u >= ROOT_IDX);
	assert(u < bh->next);
	TEST_DRIVER_ACCESS_KEY(bh, u);
	p = A(bh, u);
	AN(p);
	AN(bh->update_f);
	TEST_DRIVER_ACCESS_MEM(p);
	bh->update_f(p, BINHEAP_NOIDX);
	assert(bh->next > 0);
	if (u < --bh->next) {
		TEST_DRIVER_ACCESS_KEY(bh, bh->next);
		p = A(bh, bh->next);
		AN(p);
		v = reorder(bh, p, u);
		assign(bh, p, v);
		assert(A(bh, v) == p);
	}
	TEST_DRIVER_ACCESS_KEY(bh, bh->next);
	A(bh, bh->next) = NULL;
}

void *
binheap_root(const struct binheap *bh)
{
	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	if (bh->next == ROOT_IDX)
		return (NULL);
	TEST_DRIVER_ACCESS_KEY(bh, ROOT_IDX);
	return (A(bh, ROOT_IDX));
}

#ifdef TEST_DRIVER
/* Test driver -------------------------------------------------------*/

static void
check_consistency(const struct binheap *bh)
{
	void *p1, *p2;
	unsigned u, v;

	CHECK_OBJ_NOTNULL(bh, BINHEAP_MAGIC);
	assert(ROOT_IDX != BINHEAP_NOIDX);
	assert(bh->next >= ROOT_IDX);
	assert(bh->next <= bh->length);
	AN(bh->cmp_f);
	AN(bh->update_f);
	AN(bh->array);
	for (u = ROOT_IDX + 1; u < bh->next; u++) {
		v = parent(u);
		assert(v < u);
		assert(v >= ROOT_IDX);
		p1 = A(bh, u);
		AN(p1);
		p2 = A(bh, v);
		AN(p2);
		assert(bh->cmp_f(p2, p1) || !bh->cmp_f(p1, p2));
	}
}

static void
check_parent_child_range(unsigned n_min, unsigned n_max)
{
	unsigned n, u, v, i;

	assert(n_min > ROOT_IDX);
	for (n = n_min; n < n_max; n++) {
		u = child(n);
		assert(u > n);
		if (u == UINT_MAX)
			continue;	/* child index is too big */
		v = parent(u);
		assert(v == n);
		v = parent(u + 1);
		assert(v == n);

		u = parent(n);
		assert(u < n);
		assert(u >= ROOT_IDX);
		v = child(u);
		assert(v == (n & ~1U));
	}
}

static void
check_parent_child(unsigned checks_count)
{
	unsigned n_min, n_max;

	/* check lower end of index range */
	assert(ROOT_IDX < UINT_MAX - 1);
	n_min = 1 + ROOT_IDX;
	assert(checks_count < UINT_MAX - n_min);
	n_max = n_min + checks_count;
	check_parent_child_range(n_min, n_max);

	/* check higher end of index range */
	n_min = UINT_MAX - checks_count;
	n_max = n_min + checks_count;
	assert(n_max == UINT_MAX);
	check_parent_child_range(n_min, n_max);
}

static double
get_time(void)
{
	struct timespec ts;
	int rv;

	rv = clock_gettime(CLOCK_MONOTONIC, &ts);
	XXXAZ(rv);
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
#define MAX_ITEMS_COUNT			1000000
#define MIN_ITEMS_COUNT			1000
#define TEST_STEPS_COUNT		5
#define MAX_RESIDENT_PAGES_COUNT	4096

/*
 * Pad foo so its' size is equivalent to the objcore size.
 * Currently size of objcore is 120 bytes on x64 and 56 bytes
 * on x32. This means that the padding should be 92 for x64
 * and 36 for x32.
 */
#define PADDING 92

#define MQPS(t, q)		((t) ? (q) / (t) / 1e6 : 0)
#define PF()			\
	((double) m.pagefaults_count - m.resident_pages_count)
#define PF_PER_ITERATION(iterations_count)	\
	(PF() > 0 ? PF() / iterations_count : 0)

#ifdef PARANOIA
#define paranoia_check(bh)	check_consistency(bh)
#else
#define paranoia_check(bh)	((void)0)
#endif

struct foo {
	MAGIC_HERE;
#define FOO_MAGIC	0x23239823U

	unsigned		idx;
	double			key;
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
	access_mem(fp);
}

static void
foo_check_existence(struct binheap *bh, const struct foo *fp,
	unsigned items_count)
{
	foo_check(fp, items_count);
	assert(fp->idx != BINHEAP_NOIDX);
	assert(fp->idx >= ROOT_IDX);
	assert(fp->idx < bh->next);
	assert(fp == A(bh, fp->idx));
}

static void
foo_insert(struct binheap *bh, unsigned n, unsigned items_count)
{
	struct foo *fp;
	double key;

	paranoia_check(bh);
	assert(n < items_count);
	AZ(ff[n]);
	ALLOC_OBJ_NOTNULL(fp, FOO_MAGIC);
	ff[n] = fp;
	key = random();
	fp->idx = BINHEAP_NOIDX;
	fp->key = key;
	fp->n = n;
	binheap_insert(bh, fp);
	foo_check_existence(bh, fp, items_count);
	assert(fp->key == key);
	assert(fp->n == n);
	paranoia_check(bh);
}

static void
foo_delete(struct binheap *bh, struct foo *fp, unsigned items_count)
{
	double key;
	unsigned n;

	paranoia_check(bh);
	foo_check_existence(bh, fp, items_count);
	key = fp->key;
	n = fp->n;
	binheap_delete(bh, fp->idx);
	foo_check(fp, items_count);
	assert(fp->idx == BINHEAP_NOIDX);
	assert(fp->key == key);
	assert(fp->n == n);
	FREE_OBJ_NOTNULL(fp, FOO_MAGIC);
	ff[n] = NULL;
	paranoia_check(bh);
}

static void
foo_reorder(struct binheap *bh, struct foo *fp, unsigned items_count)
{
	double key;
	unsigned n;

	paranoia_check(bh);
	foo_check_existence(bh, fp, items_count);
	key = random();
	n = fp->n;
	fp->key = key;
	binheap_reorder(bh, fp->idx);
	foo_check_existence(bh, fp, items_count);
	assert(fp->key == key);
	assert(fp->n == n);
	paranoia_check(bh);
}

static void
test(struct binheap *bh, unsigned items_count, unsigned resident_pages_count)
{
	double start, end, dkey;
	struct foo *fp;
	unsigned u, n, iterations_count;
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
	assert(ROOT_IDX != BINHEAP_NOIDX);

	/* Insert our items */
	start = get_time();
	init_mem(resident_pages_count);
	for (n = 0; n < items_count; n++) {
		foo_insert(bh, n, items_count);
		fp = binheap_root(bh);
		foo_check(fp, items_count);
		assert(fp->idx == ROOT_IDX);
		assert(fp->key <= ff[n]->key);
	}
	check_consistency(bh);
	end = get_time();
	fprintf(stderr, "%u inserts: %.3lf Mqps, "
		"%.3lf pagefaults per iteration\n",
		items_count, MQPS(end - start, items_count),
		PF_PER_ITERATION(items_count));

	/* For M cycles, pick the root, insert new */
	n = 0;
	start = get_time();
	init_mem(resident_pages_count);
	for (u = 0; u < iterations_count; u++) {
		fp = binheap_root(bh);
		foo_check(fp, items_count);
		assert(fp->idx == ROOT_IDX);
		assert(fp->key <= ff[n]->key);
		n = fp->n;
		foo_delete(bh, fp, items_count);
		foo_insert(bh, n, items_count);
	}
	check_consistency(bh);
	end = get_time();
	fprintf(stderr, "%u root replacements: %.3lf Mqps, "
		"%.3lf pagefaults per iteration\n", iterations_count,
		MQPS(end - start, iterations_count),
		PF_PER_ITERATION(iterations_count));

	/* Randomly reorder */
	start = get_time();
	init_mem(resident_pages_count);
	for (u = 0; u < iterations_count; u++) {
		n = random() % items_count;
		fp = ff[n];
		foo_reorder(bh, fp, items_count);
	}
	check_consistency(bh);
	end = get_time();
	fprintf(stderr, "%u random reorders: %.3lf Mqps, "
		"%.3lf pagefaults per iteration\n", iterations_count,
		MQPS(end - start, iterations_count),
		PF_PER_ITERATION(iterations_count));

	/* Randomly insert, delete and reorder */
	delete_count = 0;
	insert_count = 0;
	reorder_count = 0;
	start = get_time();
	init_mem(resident_pages_count);
	for (u = 0; u < iterations_count; u++) {
		n = random() % items_count;
		fp = ff[n];
		if (fp != NULL) {
			if (((unsigned) fp->key) & 1) {
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
	end = get_time();
	fprintf(stderr,
		"%u deletes, %u inserts, %u reorders: %.3lf Mqps, "
		"%.3lf pagefaults per iteration\n",
		delete_count, insert_count, reorder_count,
		MQPS(end - start, iterations_count),
		PF_PER_ITERATION(iterations_count));

	/* Then remove everything */
	dkey = 0;
	u = 0;
	start = get_time();
	init_mem(resident_pages_count);
	while (1) {
		fp = binheap_root(bh);
		if (fp == NULL)
			break;
		foo_check(fp, items_count);
		assert(fp->idx == ROOT_IDX);
		assert(fp->key >= dkey);
		dkey = fp->key;
		foo_delete(bh, fp, items_count);
		++u;
	}
	assert(u == items_count - (delete_count - insert_count));
	AZ(binheap_root(bh));
	check_consistency(bh);
	end = get_time();
	fprintf(stderr, "%u deletes: %.3lf Mqps, "
		"%.3lf pagefaults per iteration\n",
		u, MQPS(end - start, u), PF_PER_ITERATION(u));
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

static int
foo_cmp(void *a, void *b)
{
	struct foo *fp1, *fp2;

	CAST_OBJ_NOTNULL(fp1, a, FOO_MAGIC);
	CAST_OBJ_NOTNULL(fp2, b, FOO_MAGIC);

	return (fp1->key < fp2->key);
}

static void
foo_update(void *p, unsigned idx)
{
	struct foo *fp;

	CAST_OBJ_NOTNULL(fp, p, FOO_MAGIC);
	fp->idx = idx;
}

int
main(int argc, char **argv)
{
	struct binheap *bh;
	unsigned u;

	check_parent_child(PARENT_CHILD_TESTS_COUNT);
	fprintf(stderr, "%u parent-child tests OK\n", PARENT_CHILD_TESTS_COUNT);

	init_mem(0);
	bh = binheap_new(foo_cmp, foo_update);
	AZ(binheap_root(bh));
	check_consistency(bh);

	srandom(123);	/* generate predictive results */
	fprintf(stderr, "\n* Tests with pagefault counter enabled\n");
	for (u = 1; u <= UINT_MAX / 2 && u <= MAX_RESIDENT_PAGES_COUNT; u *= 2)
		run_tests(bh, u);

	fprintf(stderr, "\n* Tests with pagefault counter disabled "
			"(aka 'perftests')\n");
	run_tests(bh, 0);
	return (0);
}
#endif
