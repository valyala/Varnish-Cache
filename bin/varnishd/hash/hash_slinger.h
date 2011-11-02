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
 */

struct object;
struct objcore;
struct objhead;
struct sess;
struct worker;

/* hash_table.c */
void HTB_Start(void);
struct objhead *HTB_Lookup(const struct sess *sp, struct objhead *oh);
int HTB_Deref(struct objhead *oh);

/* cache_hash.c */
void HSH_Prealloc(const struct sess *sp);
void HSH_Cleanup(struct worker *w);
struct objcore *HSH_Lookup(struct sess *sp, struct objhead **poh);
void HSH_Unbusy(const struct sess *sp);
void HSH_Ref(struct objcore *o);
void HSH_Drop(struct sess *sp);
void HSH_Init(void);
void HSH_AddString(const struct sess *sp, const char *str);
struct objcore *HSH_Insert(const struct sess *sp);
void HSH_Purge(const struct sess *, struct objhead *, double ttl, double grace);

#ifdef VARNISH_CACHE_CHILD

struct waitinglist {
	unsigned		magic;
#define WAITINGLIST_MAGIC	0x063a477a
	VTAILQ_HEAD(, sess)	list;
};

struct objhead {
	unsigned		magic;
#define OBJHEAD_MAGIC		0x1b96615d

	int			refcnt;
	struct lock		mtx;
	VTAILQ_HEAD(,objcore)	objcs;
	unsigned char		digest[DIGEST_LEN];
	struct waitinglist	*waitinglist;

	/*----------------------------------------------------
	 * The fields below are for the sole private use of
	 * the hash implementation(s).
	 */
	VSLIST_ENTRY(objhead)	hoh_list;
};

void HSH_DeleteObjHead(struct worker *w, struct objhead *oh);
int HSH_Deref(struct worker *w, struct objcore *oc, struct object **o);
#endif /* VARNISH_CACHE_CHILD */
