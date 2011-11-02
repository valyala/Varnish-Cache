/*-
 * Copyright (c) 2006 Verdens Gang AS
 * Copyright (c) 2006-2009 Varnish Software AS
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
 * Binary Heap API (see: http://en.wikipedia.org/wiki/Binary_heap)
 *
 * XXX: doesn't scale back the array of pointers when items are deleted.
 */

/* Public Interface --------------------------------------------------*/

struct binheap;

typedef int (*binheap_cmp_t)(void *a, void *b);
	/*
	 * User-specified callback, which should return non-zero if key value
	 * for a is less than key value for b.
	 */

typedef void (*binheap_update_t)(void *p, unsigned idx);
	/*
	 * User-defined callback, which should store updated index for the given
	 * entry p.
	 * This index can be passed to binheap_reorder() and binheap_delete().
	 */

struct binheap *binheap_new(binheap_cmp_t cmp_f, binheap_update_t update_f);
	/*
	 * Creates binary heap.
	 * cmp_f and update_f cannot be NULL.
	 */

void binheap_insert(struct binheap *bh, void *p);
	/*
	 * Inserts p into binheap.
	 * p cannot be NULL.
	 */

void binheap_reorder(const struct binheap *bh, unsigned idx);
	/*
	 * Reorders binheap after the key for the entry with the given index
	 * has been changed.
	 */

void binheap_delete(struct binheap *bh, unsigned idx);
	/*
	 * Removes the entry with the given index from binheap.
	 */

void *binheap_root(const struct binheap *bh);
	/*
	 * Returns binheap root entry, i.e. the entry with the minimal key.
	 * Returns NULL if binheap is empty.
	 */

#define BINHEAP_NOIDX	0
	/*
	 * This value is passed into update_f() after entry removal
	 * from the binheap.
	 */
