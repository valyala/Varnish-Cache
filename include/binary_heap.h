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
 */

/* Public Interface --------------------------------------------------*/

struct binheap;
struct binheap_item;

struct binheap *binheap_new(void);
	/*
	 * Creates binary heap.
	 */

struct binheap_item *binheap_insert(struct binheap *bh, void *p,
	unsigned key);
	/*
	 * Inserts an item p with the given key into binheap.
	 * Item cannot be NULL.
	 * Returns a pointer to opaque object, which can be passed
	 * to binheap_update() or binheap_delete().
	 */

void binheap_update(const struct binheap *bh, struct binheap_item *bi,
	unsigned key);
        /*
         * Modifies key value for the given item.
         */

void binheap_delete(struct binheap *bh, struct binheap_item *bi);
        /*
         * Removes the item from binheap.
         */

void *binheap_root(const struct binheap *bh);
	/*
	 * Returns the root item.
	 * If the binheap is empty, returns NULL.
	 */
