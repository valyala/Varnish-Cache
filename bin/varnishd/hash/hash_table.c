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

struct bucket {
	unsigned		magic;
#define BUCKET_MAGIC		0x0f327016
	VSLIST_HEAD(, objhead)	head;
	struct lock		mtx;
};

static struct bucket		*buckets;

/*--------------------------------------------------------------------
 * This method is called during cache process start and allows
 * initialization to happen before the first lookup.
 */

void
HTB_Start(void)
{
	unsigned hash_buckets, u;

	hash_buckets = params->hash_buckets;
	assert(hash_buckets > 0);
	buckets = calloc(hash_buckets, sizeof(*buckets));
	XXXAN(buckets);

	for (u = 0; u < hash_buckets; u++) {
		buckets[u].magic = BUCKET_MAGIC;
		VSLIST_INIT(&buckets[u].head);
		Lck_New(&buckets[u].mtx, lck_htb_bucket);
	}
}

static struct bucket *
get_bucket(const struct objhead *oh)
{
	struct bucket *b;
	unsigned digest, u;

	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
	assert(sizeof(oh->digest) > sizeof(digest));
	memcpy(&digest, oh->digest, sizeof(digest));
	assert(params->hash_buckets > 0);
	u = digest % params->hash_buckets;
	AN(buckets);
	b = &buckets[u];
	CHECK_OBJ_NOTNULL(b, BUCKET_MAGIC);
	return (b);
}

/*--------------------------------------------------------------------
 * Lookup and possibly insert element.
 * If the lookup does not find key, nobj is inserted.
 * A reference to the returned object is held.
 */

struct objhead *
HTB_Lookup(struct objhead *noh)
{
	struct objhead *oh, *head, **poh;
	struct bucket *b;

	CHECK_OBJ_NOTNULL(noh, OBJHEAD_MAGIC);
	b = get_bucket(noh);
	CHECK_OBJ_NOTNULL(b, BUCKET_MAGIC);

	Lck_Lock(&b->mtx);
	head = VSLIST_FIRST(&b->head);
	VSLIST_FOREACH_PREVPTR(oh, poh, &b->head, hoh_list) {
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
			VSLIST_INSERT_HEAD(&b->head, oh, hoh_list);
		}
		oh->refcnt++;
		Lck_Unlock(&b->mtx);
		return (oh);
	}

	VSLIST_INSERT_HEAD(&b->head, noh, hoh_list);

	Lck_Unlock(&b->mtx);
	return (noh);
}

/*--------------------------------------------------------------------
 * Dereference and if no references are left, free.
 */

int
HTB_Deref(struct objhead *oh)
{
	struct bucket *b;
	int ret;

	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
	b = get_bucket(oh);
	CHECK_OBJ_NOTNULL(b, BUCKET_MAGIC);

	Lck_Lock(&b->mtx);
	assert(oh->refcnt > 0);
	if (--oh->refcnt == 0) {
		/*
		 * Though removal from singly list list requires O(n) time,
		 * this should be OK here, since hash table buckets must contain
		 * only a few items to be fast. If this isn't the case, just
		 * increase the number of buckets in the table (hash_buckets).
		 */
		VSLIST_REMOVE(&b->head, oh, objhead, hoh_list);
		ret = 0;
	} else
		ret = 1;
	Lck_Unlock(&b->mtx);
	return (ret);
}
