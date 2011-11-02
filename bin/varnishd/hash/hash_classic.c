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

#include <stdlib.h>

#include "cache.h"

#include "hash/hash_slinger.h"

/*--------------------------------------------------------------------*/

struct hcl_head {
	unsigned		magic;
#define HCL_HEAD_MAGIC		0x0f327016
	VSLIST_HEAD(, objhead)	head;
	struct lock		mtx;
};

static struct hcl_head		*hcl_hashtable;

/*--------------------------------------------------------------------
 * The ->init method allows the management process to pass arguments
 */

static void
hcl_init(int ac, char * const *av)
{
	(void)ac;
	(void)av;
}

/*--------------------------------------------------------------------
 * The ->start method is called during cache process start and allows
 * initialization to happen before the first lookup.
 */

static void
hcl_start(void)
{
	unsigned hash_buckets, u;

	hash_buckets = params->hash_buckets;
	assert(hash_buckets > 0);
	hcl_hashtable = calloc(hash_buckets, sizeof(*hcl_hashtable));
	XXXAN(hcl_hashtable);

	for (u = 0; u < hash_buckets; u++) {
		hcl_hashtable[u].magic = HCL_HEAD_MAGIC;
		VSLIST_INIT(&hcl_hashtable[u].head);
		Lck_New(&hcl_hashtable[u].mtx, lck_hcl_head);
	}
}

static struct hcl_head *
get_hcl_head(const struct objhead *oh)
{
	struct hcl_head *hp;
	unsigned digest, u;

	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
	assert(sizeof(oh->digest) > sizeof(digest));
	memcpy(&digest, oh->digest, sizeof(digest));
	assert(params->hash_buckets > 0);
	u = digest % params->hash_buckets;
	hp = &hcl_hashtable[u];
	CHECK_OBJ_NOTNULL(hp, HCL_HEAD_MAGIC);
	return (hp);
}

/*--------------------------------------------------------------------
 * Lookup and possibly insert element.
 * If the lookup does not find key, nobj is inserted.
 * A reference to the returned object is held.
 */

static struct objhead *
hcl_lookup(const struct sess *sp, struct objhead *noh)
{
	struct objhead *oh, *head, **poh;
	struct hcl_head *hp;

	CHECK_OBJ_NOTNULL(sp, SESS_MAGIC);
	CHECK_OBJ_NOTNULL(noh, OBJHEAD_MAGIC);
	hp = get_hcl_head(noh);
	CHECK_OBJ_NOTNULL(hp, HCL_HEAD_MAGIC);

	Lck_Lock(&hp->mtx);
	head = VSLIST_FIRST(&hp->head);
	VSLIST_FOREACH_PREVPTR(oh, poh, &hp->head, hoh_list) {
		CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
		if (memcmp(oh->digest, noh->digest, sizeof oh->digest)) {
			VSC_C_main->n_hcl_lookup_collisions++;
			continue;
		}

		/*
		 * Move the found object to the head of the list, so recently
		 * looked up objects will be grouped close to the list head.
		 * This should result in faster lookups for 'hot' objects.
		 */
		if (oh != head) {
			*poh = VSLIST_NEXT(oh, hoh_list);
			VSLIST_INSERT_HEAD(&hp->head, oh, hoh_list);
		}
		oh->refcnt++;
		Lck_Unlock(&hp->mtx);
		return (oh);
	}

	VSLIST_INSERT_HEAD(&hp->head, noh, hoh_list);

	Lck_Unlock(&hp->mtx);
	return (noh);
}

/*--------------------------------------------------------------------
 * Dereference and if no references are left, free.
 */

static int
hcl_deref(struct objhead *oh)
{
	struct hcl_head *hp;
	int ret;

	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
	hp = get_hcl_head(oh);
	CHECK_OBJ_NOTNULL(hp, HCL_HEAD_MAGIC);

	Lck_Lock(&hp->mtx);
	assert(oh->refcnt > 0);
	if (--oh->refcnt == 0) {
		/*
		 * Though removal from singly list list requires O(n) time,
		 * this should be OK here, since hash table buckets must contain
		 * only a few items to be fast. If this isn't the case, just
		 * increase the number of buckets in the table (hash_buckets).
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
