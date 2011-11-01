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
 * A classic bucketed hash
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>

#include "cache.h"

#include "hash/hash_slinger.h"

/*--------------------------------------------------------------------*/

struct hcl_hd {
	unsigned		magic;
#define HCL_HEAD_MAGIC		0x0f327016
	VSLIST_HEAD(, objhead)	head;
	struct lock		mtx;
};

static unsigned			hcl_nhash = 16383;
static struct hcl_hd		*hcl_head;

/*--------------------------------------------------------------------
 * The ->init method allows the management process to pass arguments
 */

static void
hcl_init(int ac, char * const *av)
{
	int i;
	unsigned u;

	if (ac == 0)
		return;
	if (ac > 1)
		ARGV_ERR("(-hclassic) too many arguments\n");
	i = sscanf(av[0], "%u", &u);
	if (i <= 0 || u == 0)
		return;
	if (u > 2 && !(u & (u - 1))) {
		fprintf(stderr,
		    "NOTE:\n"
		    "\tA power of two number of hash buckets is "
		    "marginally less efficient\n"
		    "\twith systematic URLs.  Reducing by one"
		    " hash bucket.\n");
		u--;
	}
	hcl_nhash = u;
	fprintf(stderr, "Classic hash: %u buckets\n", hcl_nhash);
	return;
}

/*--------------------------------------------------------------------
 * The ->start method is called during cache process start and allows
 * initialization to happen before the first lookup.
 */

static void
hcl_start(void)
{
	unsigned u;

	hcl_head = calloc(sizeof *hcl_head, hcl_nhash);
	XXXAN(hcl_head);

	for (u = 0; u < hcl_nhash; u++) {
		hcl_head[u].magic = HCL_HEAD_MAGIC;
		VSLIST_INIT(&hcl_head[u].head);
		Lck_New(&hcl_head[u].mtx, lck_hcl);
	}
}

/*--------------------------------------------------------------------
 * Lookup and possibly insert element.
 * If the lookup does not find key, nobj is inserted.
 * A reference to the returned object is held.
 */

static struct objhead *
hcl_lookup(const struct sess *sp, struct objhead *noh)
{
	struct objhead *oh, **ohp;
	struct hcl_hd *hp;
	unsigned u1, digest;

	CHECK_OBJ_NOTNULL(sp, SESS_MAGIC);
	CHECK_OBJ_NOTNULL(noh, OBJHEAD_MAGIC);

	assert(sizeof noh->digest > sizeof digest);
	memcpy(&digest, noh->digest, sizeof digest);
	u1 = digest % hcl_nhash;
	hp = &hcl_head[u1];
	CHECK_OBJ_NOTNULL(hp, HCL_HEAD_MAGIC);

	Lck_Lock(&hp->mtx);
	VSLIST_FOREACH_PREVPTR(oh, ohp, &hp->head, hoh_list) {
		CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
		if (memcmp(oh->digest, noh->digest, sizeof oh->digest))
			continue;

		/*
		 * Move the found object to the head of the list, so recently
		 * found objects will be looked up faster next time.
		 */
		if (ohp != &VSLIST_FIRST(&hp->head)) {
			VSLIST_NEXT(*ohp, hoh_list) = VSLIST_NEXT(oh, hoh_list);
			VSLIST_INSERT_HEAD(&hp->head, oh, hoh_list);
		}
		oh->refcnt++;
		Lck_Unlock(&hp->mtx);
		return (oh);
	}

	VSLIST_INSERT_HEAD(&hp->head, noh, hoh_list);
	noh->hoh_head = hp;

	Lck_Unlock(&hp->mtx);
	return (noh);
}

/*--------------------------------------------------------------------
 * Dereference and if no references are left, free.
 */

static int
hcl_deref(struct objhead *oh)
{
	struct hcl_hd *hp;
	int ret;

	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
	CAST_OBJ_NOTNULL(hp, oh->hoh_head, HCL_HEAD_MAGIC);
	Lck_Lock(&hp->mtx);
	assert(oh->refcnt > 0);
	if (--oh->refcnt == 0) {
		/*
		 * Though removal from singly list list requires O(n) time,
		 * this should be OK here, since hash table buckets must contain
		 * only a few items to be fast. If this isn't the case, just
		 * increase the number of buckets in the table (hcl_nhash).
		 */
		VSLIST_REMOVE(&hp->head, oh, objhead, hoh_list);
		ret = 0;
	} else
		ret = 1;
	Lck_Unlock(&hp->mtx);
	return (ret);
}

/*--------------------------------------------------------------------*/

const struct hash_slinger hcl_slinger = {
	.magic	=	SLINGER_MAGIC,
	.name	=	"classic",
	.init	=	hcl_init,
	.start	=	hcl_start,
	.lookup =	hcl_lookup,
	.deref	=	hcl_deref,
};
