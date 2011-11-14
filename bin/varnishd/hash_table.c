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

#include "cache/cache.h"
#include "hash_table.h"
#include "miniobj.h"

/*--------------------------------------------------------------------*/

struct bucket {
	VSLIST_HEAD(, objhead)	head;
	struct lock		mtx;
};

static struct bucket		*buckets;
static unsigned buckets_count;

/*--------------------------------------------------------------------
 * This method is called during cache process start and allows
 * initialization to happen before the first lookup.
 */

void
HTB_Init(void)
{
	unsigned u;

	buckets_count = cache_param->hashtable_buckets;
	assert(buckets_count > 0);
	CALLOC_NOTNULL(buckets, buckets_count);

	for (u = 0; u < buckets_count; u++) {
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
	assert(sizeof(oh->digest) >= sizeof(digest));
	memcpy(&digest, oh->digest, sizeof(digest));
	assert(buckets_count > 0);
	u = digest % buckets_count;
	AN(buckets);
	b = &buckets[u];
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
	struct objhead *oh, **poh;
	struct bucket *b;

	CHECK_OBJ_NOTNULL(noh, OBJHEAD_MAGIC);
	AZ(VSLIST_NEXT(noh, htb_list));

	b = get_bucket(noh);
	AN(b);

	Lck_Lock(&b->mtx);
	VSLIST_FOREACH_PREVPTR(oh, poh, &b->head, htb_list) {
		CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
		if (memcmp(oh->digest, noh->digest, sizeof oh->digest)) {
			VSC_C_main->n_htb_lookup_collisions++;
			continue;
		}

		/*
		 * Move the found object to the head of the list, so recently
		 * looked up objects will be grouped close to the list head.
		 * This should result in faster lookups for 'hot' objects.
		 */
		if (oh != VSLIST_FIRST(&b->head)) {
			*poh = VSLIST_NEXT(oh, htb_list);
			VSLIST_INSERT_HEAD(&b->head, oh, htb_list);
		}
		oh->refcnt++;
		Lck_Unlock(&b->mtx);
		return (oh);
	}

	VSLIST_INSERT_HEAD(&b->head, noh, htb_list);

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
	AN(b);

	Lck_Lock(&b->mtx);
	assert(oh->refcnt > 0);
	if (--oh->refcnt == 0) {
		/*
		 * Though removal from singly list list requires O(n) time,
		 * this should be OK here, since hash table buckets must contain
		 * only a few items to be fast. If this isn't the case, just
		 * increase the number of hashtable buckets (hashtable_buckets).
		 */
		VSLIST_REMOVE(&b->head, oh, objhead, htb_list);
		VSLIST_NEXT(oh, htb_list) = NULL;
		ret = 0;
	} else
		ret = 1;
	Lck_Unlock(&b->mtx);
	return (ret);
}
