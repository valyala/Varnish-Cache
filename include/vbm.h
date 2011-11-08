/*-
 * Copyright (c) 2006 Verdens Gang AS
 * Copyright (c) 2006-2010 Varnish Software AS
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
 * Self-sizeing bitmap operations
 */

#include "miniobj.h"

/**********************************************************************
 * Generic bitmap functions, may be generalized at some point.
 */

#define VBITMAP_TYPE	unsigned	/* Our preferred wordsize */
#define VBITMAP_LUMP	(1024)		/* How many bits we alloc at a time */
#define VBITMAP_WORD	(sizeof(VBITMAP_TYPE) * 8)
#define VBITMAP_IDX(n)	(n / VBITMAP_WORD)
#define VBITMAP_BIT(n)	(1U << (n % VBITMAP_WORD))

struct vbitmap {
	MAGIC_HERE;
#define VBITMAP_MAGIC	0x5cbe52a4U

	VBITMAP_TYPE	*bits;
	unsigned	nbits;
};

static inline void
vbit_expand(struct vbitmap *vb, unsigned bit)
{
	unsigned char *p;

	CHECK_OBJ_NOTNULL(vb, VBITMAP_MAGIC);
	bit += VBITMAP_LUMP - 1;
	bit -= (bit % VBITMAP_LUMP);
	p = (unsigned char *)vb->bits;
	REALLOC_ARRAY_NOTNULL(p, bit / 8);
	ZERO_ARRAY(p + vb->nbits / 8, (bit - vb->nbits) / 8);
	vb->bits = (void*)p;
	vb->nbits = bit;
}

static inline struct vbitmap *
vbit_init(unsigned initial)
{
	struct vbitmap *vb;

	ALLOC_OBJ_NOTNULL(vb, VBITMAP_MAGIC);
	if (initial == 0)
		initial = VBITMAP_LUMP;
	vbit_expand(vb, initial);
	return (vb);
}

static inline void
vbit_destroy(struct vbitmap *vb)
{
	if (vb == NULL)
		return;
	CHECK_OBJ_NOTNULL(vb, VBITMAP_MAGIC);
	FREE_ORNULL(vb->bits);
	FREE_OBJ_NOTNULL(vb, VBITMAP_MAGIC);
}

static inline void
vbit_set(struct vbitmap *vb, unsigned bit)
{
	CHECK_OBJ_NOTNULL(vb, VBITMAP_MAGIC);
	if (bit >= vb->nbits)
		vbit_expand(vb, bit);
	vb->bits[VBITMAP_IDX(bit)] |= VBITMAP_BIT(bit);
}

static inline void
vbit_clr(const struct vbitmap *vb, unsigned bit)
{
	CHECK_OBJ_NOTNULL(vb, VBITMAP_MAGIC);
	if (bit < vb->nbits)
		vb->bits[VBITMAP_IDX(bit)] &= ~VBITMAP_BIT(bit);
}

static inline int
vbit_test(const struct vbitmap *vb, unsigned bit)
{
	CHECK_OBJ_NOTNULL(vb, VBITMAP_MAGIC);
	if (bit >= vb->nbits)
		return (0);
	return (vb->bits[VBITMAP_IDX(bit)] & VBITMAP_BIT(bit));
}
