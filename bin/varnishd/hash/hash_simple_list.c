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
 * This is the reference hash(/lookup) implementation
 */

#include "config.h"

#include "cache.h"

#include "hash/hash_slinger.h"

/*--------------------------------------------------------------------*/

static VSLIST_HEAD(, objhead)	hsl_head = VSLIST_HEAD_INITIALIZER(hsl_head);
static struct lock hsl_mtx;

/*--------------------------------------------------------------------
 * The ->init method is called during process start and allows
 * initialization to happen before the first lookup.
 */

static void
hsl_start(void)
{
	Lck_New(&hsl_mtx, lck_hsl);
}

/*--------------------------------------------------------------------
 * Lookup and possibly insert element.
 * If the lookup does not find key, nobj is inserted.
 * A reference to the returned object is held.
 */

static struct objhead *
hsl_lookup(const struct sess *sp, struct objhead *noh)
{
	struct objhead *oh;

	CHECK_OBJ_NOTNULL(sp, SESS_MAGIC);
	CHECK_OBJ_NOTNULL(noh, OBJHEAD_MAGIC);
	Lck_Lock(&hsl_mtx);
	VSLIST_FOREACH(oh, &hsl_head, hoh_list) {
		CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
		if (memcmp(oh->digest, noh->digest, sizeof oh->digest))
			continue;

		oh->refcnt++;
		Lck_Unlock(&hsl_mtx);
		return (oh);
	}

	VSLIST_INSERT_HEAD(&hsl_head, noh, hoh_list);
	Lck_Unlock(&hsl_mtx);
	return (noh);
}

/*--------------------------------------------------------------------
 * Dereference and if no references are left, free.
 */

static int
hsl_deref(struct objhead *oh)
{
	int ret;

	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);

	Lck_Lock(&hsl_mtx);
	assert(oh->refcnt > 0);
	if (--oh->refcnt == 0) {
		VSLIST_REMOVE(&hsl_head, oh, objhead, hoh_list);
		ret = 0;
	} else
		ret = 1;
	Lck_Unlock(&hsl_mtx);
	return (ret);
}

/*--------------------------------------------------------------------*/

const struct hash_slinger hsl_slinger = {
	.magic	=	SLINGER_MAGIC,
	.name	=	"simple",
	.start	=	hsl_start,
	.lookup =	hsl_lookup,
	.deref	=	hsl_deref,
};
