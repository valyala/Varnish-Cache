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

typedef int binheap_cmp_t(void *priv, void *a, void *b);
	/*
	 * Comparison function.
	 * Should return true if item 'a' should be closer to the root
	 * than item 'b'.
	 */

typedef void binheap_update_t(void *priv, void *a, unsigned newidx);
	/*
	 * Update function.
	 * When items move in the tree, this function gets called to
	 * notify the item of its new index.
	 */

struct binheap *binheap_new(void *priv, binheap_cmp_t, binheap_update_t);
	/*
	 * Create Binary tree.
	 * cmd and update functions are required, priv is optional.
	 * 'priv' is passed to cmp and update functions.
	 */

void binheap_insert(struct binheap *, void *);
	/*
	 * Insert an item.
	 * Item cannot be NULL.
	 */

void binheap_reorder(struct binheap *, unsigned idx);
	/*
	 * Restore binheap invariant after modifying key value for the item
	 * with the given index.
	 */

void binheap_delete(struct binheap *, unsigned idx);
	/*
	 * Remove the item with the given index from binheap.
	 */

void *binheap_root(const struct binheap *);
	/*
	 * Return the root item.
	 * If the binheap is empty, return NULL.
	 */

#define BINHEAP_NOIDX	0
	/*
	 * Update function pass this value as newidx if the given item has been
	 * removed from binheap.
	 */


/* binheap2 */
struct binheap2_item;
struct binheap2;

struct binheap2 *
binheap2_new(void);

struct binheap2_item *
binheap2_insert(struct binheap2 *bh2, void *p, double key);

void
binheap2_delete(struct binheap2 *bh2, struct binheap2_item *bi);

void
binheap2_reorder(struct binheap2 *bh2, struct binheap2_item *bi, double key);

void *
binheap2_root(struct binheap2 *bh2);
