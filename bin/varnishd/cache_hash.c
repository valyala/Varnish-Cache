/*-
 * Copyright (c) 2006 Verdens Gang AS
 * Copyright (c) 2006-2011 Varnish Software AS
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
 * This is the central hash-table code, it relies on a chosen hash
 * implementation only for the actual hashing, all the housekeeping
 * happens here.
 *
 * We have two kinds of structures, objecthead and object.  An objecthead
 * corresponds to a given (Host:, URL) tupple, and the objects hung from
 * the objecthead may represent various variations (ie: Vary: header,
 * different TTL etc) instances of that web-entity.
 *
 * Each objecthead has a mutex which locks both its own fields, the
 * list of objects and fields in the objects.
 *
 * The hash implementation must supply a reference count facility on
 * the objecthead, and return with a reference held after a lookup.
 *
 * Lookups in the hash implementation returns with a ref held and each
 * object hung from the objhead holds a ref as well.
 *
 * Objects have refcounts which are locked by the objecthead mutex.
 *
 * New objects are always marked busy, and they can go from busy to
 * not busy only once.
 */

#include "config.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "cache.h"
#include "cache_hash.h"
#include "hash_table.h"
#include "vsha256.h"

/*---------------------------------------------------------------------*/
/* Precreate an objhead and object for later use */
void
HSH_Prealloc(const struct sess *sp)
{
	struct worker *w;
	struct objhead *oh;
	struct objcore *oc;

	CHECK_OBJ_NOTNULL(sp, SESS_MAGIC);
	CHECK_OBJ_NOTNULL(sp->wrk, WORKER_MAGIC);
	w = sp->wrk;

	if (w->nobjcore == NULL) {
		ALLOC_OBJ(oc, OBJCORE_MAGIC);
		XXXAN(oc);
		w->nobjcore = oc;
		w->stats.n_objectcore++;
		oc->flags |= OC_F_BUSY;
	}
	CHECK_OBJ_NOTNULL(w->nobjcore, OBJCORE_MAGIC);

	if (w->nobjhead == NULL) {
		ALLOC_OBJ(oh, OBJHEAD_MAGIC);
		XXXAN(oh);
		oh->refcnt = 1;
		VSLIST_INIT(&oh->objcore_head);
		Lck_New(&oh->mtx, lck_objhdr);
		w->nobjhead = oh;
		w->stats.n_objecthead++;
	}
	CHECK_OBJ_NOTNULL(w->nobjhead, OBJHEAD_MAGIC);

	if (w->nbusyobj == NULL) {
		ALLOC_OBJ(w->nbusyobj, BUSYOBJ_MAGIC);
		XXXAN(w->nbusyobj);
	}
}

void
HSH_Cleanup(struct worker *w)
{

	if (w->nobjcore != NULL) {
		FREE_OBJ(w->nobjcore);
		w->stats.n_objectcore--;
		w->nobjcore = NULL;
	}
	if (w->nobjhead != NULL) {
		Lck_Delete(&w->nobjhead->mtx);
		FREE_OBJ(w->nobjhead);
		w->nobjhead = NULL;
		w->stats.n_objecthead--;
	}
	if (w->nbusyobj != NULL) {
		FREE_OBJ(w->nbusyobj);
		w->nbusyobj = NULL;
	}
}

void
HSH_DeleteObjHead(struct worker *w, struct objhead *oh)
{

	AZ(oh->refcnt);
	assert(VSLIST_EMPTY(&oh->objcore_head));
	AZ(oh->waitinglist);
	AZ(VSLIST_NEXT(oh, htb_list));
	Lck_Delete(&oh->mtx);
	w->stats.n_objecthead--;
	FREE_OBJ(oh);
}

void
HSH_AddString(const struct sess *sp, const char *str)
{
	int l;

	if (str == NULL)
		str = "";
	l = strlen(str);

	SHA256_Update(sp->wrk->sha256ctx, str, l);
	SHA256_Update(sp->wrk->sha256ctx, "#", 1);

	if (params->log_hash)
		WSP(sp, SLT_Hash, "%s", str);
}

/*---------------------------------------------------------------------
 * Insert an object which magically appears out of nowhere or, more likely,
 * comes off some persistent storage device.
 * Return it with a reference held.
 */

struct objcore *
HSH_Insert(const struct sess *sp)
{
	struct worker *w;
	struct objhead *oh;
	struct objcore *oc;

	CHECK_OBJ_NOTNULL(sp, SESS_MAGIC);
	CHECK_OBJ_NOTNULL(sp->wrk, WORKER_MAGIC);
	w = sp->wrk;

	HSH_Prealloc(sp);

	AZ(sp->hash_objhead);
	AN(w->nobjhead);
	oh = HTB_Lookup(w->nobjhead);
	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
	if (oh == w->nobjhead)
		w->nobjhead = NULL;
	Lck_Lock(&oh->mtx);
	assert(oh->refcnt > 0);

	/* Insert (precreated) objcore in objecthead */
	oc = w->nobjcore;
	w->nobjcore = NULL;
	oc->refcnt = 1;
	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
	AZ(oc->flags & OC_F_BUSY);

	VSLIST_INSERT_HEAD(&oh->objcore_head, oc, hsh_list);
	oc->objhead = oh;
	/* NB: do not deref objhead the new object inherits our reference */
	Lck_Unlock(&oh->mtx);
	w->stats.n_vampireobject++;
	return (oc);
}

/*---------------------------------------------------------------------
 */

struct objcore *
HSH_Lookup(struct sess *sp, struct objhead **poh)
{
	struct worker *w;
	struct objhead *oh;
	struct objcore *oc, **poc;
	struct objcore *busy_oc, *grace_oc;
	struct object *o;
	double grace_ttl;
	int i;

	CHECK_OBJ_NOTNULL(sp, SESS_MAGIC);
	CHECK_OBJ_NOTNULL(sp->wrk, WORKER_MAGIC);
	CHECK_OBJ_NOTNULL(sp->http, HTTP_MAGIC);
	AN(sp->director);
	w = sp->wrk;

	HSH_Prealloc(sp);
	AN(w->nobjhead);
	memcpy(w->nobjhead->digest, sp->digest, sizeof sp->digest);

	if (sp->hash_objhead != NULL) {
		/*
		 * This sess came off the waiting list, and brings a
		 * oh refcnt with it.
		 */
		CHECK_OBJ_NOTNULL(sp->hash_objhead, OBJHEAD_MAGIC);
		oh = sp->hash_objhead;
		sp->hash_objhead = NULL;
	} else {
		AN(w->nobjhead);
		oh = HTB_Lookup(w->nobjhead);
		if (oh == w->nobjhead)
			w->nobjhead = NULL;
	}

	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
	Lck_Lock(&oh->mtx);
	assert(oh->refcnt > 0);
	busy_oc = NULL;
	grace_oc = NULL;
	grace_ttl = NAN;
	VSLIST_FOREACH_PREVPTR(oc, poc, &oh->objcore_head, hsh_list) {
		/* Must be at least our own ref + the objcore we examine */
		assert(oh->refcnt > 1);
		CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
		assert(oc->objhead == oh);

		if (oc->flags & OC_F_BUSY) {
			CHECK_OBJ_NOTNULL(oc->busyobj, BUSYOBJ_MAGIC);
			if (sp->hash_ignore_busy)
				continue;

			if (oc->busyobj->vary != NULL &&
			    !VRY_Match(sp, oc->busyobj->vary))
				continue;

			busy_oc = oc;
			continue;
		}

		o = oc_getobj(w, oc);
		CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);

		if (o->exp.ttl <= 0.)
			continue;
		if (BAN_CheckObject(o, sp))
			continue;
		if (o->vary != NULL && !VRY_Match(sp, o->vary))
			continue;

		/* If still valid, use it */
		if (EXP_Ttl(sp, o) >= sp->t_req) {
		        /*
		         * Move found object to the objcore_head head, because
			 * it is likely it will be requested again soon.
		         */
		        if (oc != VSLIST_FIRST(&oh->objcore_head)) {
				*poc = VSLIST_NEXT(oc, hsh_list);
		                VSLIST_INSERT_HEAD(&oh->objcore_head, oc,
						   hsh_list);
		        }
			break;
		}

		/*
		 * Remember any matching objects inside their grace period
		 * and if there are several, use the least expired one.
		 */
		if (EXP_Grace(sp, o) >= sp->t_req) {
			if (grace_oc == NULL ||
			    grace_ttl < o->exp.entered + o->exp.ttl) {
				grace_oc = oc;
				grace_ttl = o->exp.entered + o->exp.ttl;
			}
		}
	}

	/*
	 * If we have seen a busy object or the backend is unhealthy, and
	 * we have an object in grace, use it, if req.grace is also
	 * satisified.
	 * XXX: Interesting footnote:  The busy object might be for a
	 * XXX: different "Vary:" than we sought.  We have no way of knowing
	 * XXX: this until the object is unbusy'ed, so in practice we
	 * XXX: serialize fetch of all Vary's if grace is possible.
	 */

	AZ(sp->objcore);
	sp->objcore = grace_oc;		/* XXX: Hack-ish */
	if (oc == NULL			/* We found no live object */
	    && grace_oc != NULL		/* There is a grace candidate */
	    && (busy_oc != NULL		/* Somebody else is already busy */
	    || !VDI_Healthy(sp->director, sp))) {
					/* Or it is impossible to fetch */
		o = oc_getobj(w, grace_oc);
		CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);
		oc = grace_oc;
	}
	sp->objcore = NULL;

	if (oc != NULL && !sp->hash_always_miss) {
		o = oc_getobj(w, oc);
		CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);
		assert(o->objcore == oc);

		/* We found an object we like */
		oc->refcnt++;
		if (o->hits < INT_MAX)
			o->hits++;
		assert(oh->refcnt > 1);
		Lck_Unlock(&oh->mtx);
		i = HTB_Deref(oh);
		AN(i);
		*poh = oh;
		return (oc);
	}

	if (busy_oc != NULL) {
		/* There are one or more busy objects, wait for them */
		if (sp->esi_level == 0) {
			VTAILQ_NEXT(sp, list) = oh->waitinglist;
			oh->waitinglist = sp;
		}
		if (params->diag_bitmap & 0x20)
			WSP(sp, SLT_Debug,
				"on waiting list <%p>", oh);
		SES_Charge(sp);
		/*
		 * The objhead reference transfers to the sess, we get it
		 * back when the sess comes off the waiting list and
		 * calls us again
		 */
		sp->hash_objhead = oh;
		sp->wrk = NULL;
		Lck_Unlock(&oh->mtx);
		return (NULL);
	}

	/* Insert (precreated) objcore in objecthead */
	oc = w->nobjcore;
	w->nobjcore = NULL;
	AN(oc->flags & OC_F_BUSY);
	oc->refcnt = 1;

	/* XXX: clear w->nbusyobj before use */
	VRY_Validate(sp->vary_b);
	if (sp->vary_l != NULL)
		w->nbusyobj->vary = sp->vary_b;
	else
		w->nbusyobj->vary = NULL;
	oc->busyobj = w->nbusyobj;
	w->nbusyobj = NULL;

	VSLIST_INSERT_HEAD(&oh->objcore_head, oc, hsh_list);
	oc->objhead = oh;
	/* NB: do not deref objhead the new object inherits our reference */
	Lck_Unlock(&oh->mtx);
	*poh = oh;
	return (oc);
}

/*---------------------------------------------------------------------
 */

static void
hsh_rush(struct objhead *oh)
{
	unsigned u;
	struct sess *sp;

	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
	Lck_AssertHeld(&oh->mtx);
	for (u = 0; u < params->rush_exponent; u++) {
		sp = oh->waitinglist;
		if (sp == NULL)
			break;
		CHECK_OBJ_NOTNULL(sp, SESS_MAGIC);
		AZ(sp->wrk);
		oh->waitinglist = VTAILQ_NEXT(sp, list);
		DSL(0x20, SLT_Debug, sp->vsl_id, "off waiting list");
		if (SES_Schedule(sp)) {
			/*
			 * We could not schedule the session, leave the
			 * rest on the busy list.
			 */
			break;
		}
	}
}

/*---------------------------------------------------------------------
 * Purge an entire objhead
 */

void
HSH_Purge(const struct sess *sp, struct objhead *oh, double ttl, double grace)
{
	struct objcore *oc, **poc;
	unsigned spc, nobj, n;
	struct object *o;

	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
	spc = WS_Reserve(sp->wrk->ws, 0);
	poc = (void*)sp->wrk->ws->f;
	Lck_Lock(&oh->mtx);
	assert(oh->refcnt > 0);
	nobj = 0;
	VSLIST_FOREACH(oc, &oh->objcore_head, hsh_list) {
		/* Must be at least our own ref + the objcore we examine */
		assert(oh->refcnt > 1);
		CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
		assert(oc->objhead == oh);

		if (oc->flags & OC_F_BUSY) {
			/*
			 * We cannot purge busy objects here, because their
			 * owners have special rights to them, and may nuke
			 * them without concern for the refcount, which by
			 * definition always must be one, so they don't check.
			 */
			continue;
		}

		(void)oc_getobj(sp->wrk, oc); /* XXX: still needed ? */

		xxxassert(spc >= sizeof *poc);
		oc->refcnt++;
		spc -= sizeof *poc;
		poc[nobj++] = oc;
	}
	Lck_Unlock(&oh->mtx);

	/* NB: inverse test to catch NAN also */
	if (!(ttl > 0.))
		ttl = -1.;
	if (!(grace > 0.))
		grace = -1.;
	for (n = 0; n < nobj; n++) {
		oc = poc[n];
		CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
		o = oc_getobj(sp->wrk, oc);
		if (o == NULL)
			continue;
		CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);
		o->exp.ttl = ttl;
		o->exp.grace = grace;
		EXP_Rearm(o);
		(void)HSH_Deref(sp->wrk, NULL, &o);
	}
	WS_Release(sp->wrk->ws, 0);
}


/*---------------------------------------------------------------------
 * Kill a busy object we don't need anyway.
 * There may be sessions on the waiting list, so we cannot just blow
 * it out of the water.
 */

void
HSH_Drop(struct sess *sp)
{
	struct object *o;

	CHECK_OBJ_NOTNULL(sp, SESS_MAGIC);
	o = sp->obj;
	CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);
	AssertObjCorePassOrBusy(o->objcore);
	o->exp.ttl = -1.;
	if (o->objcore != NULL)		/* Pass has no objcore */
		HSH_Unbusy(sp);
	(void)HSH_Deref(sp->wrk, NULL, &sp->obj);
}

void
HSH_Unbusy(const struct sess *sp)
{
	struct object *o;
	struct objhead *oh;
	struct objcore *oc;

	CHECK_OBJ_NOTNULL(sp, SESS_MAGIC);
	o = sp->obj;
	CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);
	oc = o->objcore;
	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
	oh = oc->objhead;
	CHECK_OBJ(oh, OBJHEAD_MAGIC);

	AssertObjBusy(o);
	AN(oc->ban);
	assert(oc->refcnt > 0);
	assert(oh->refcnt > 0);
	if (o->ws_o->overflow)
		sp->wrk->stats.n_objoverflow++;
	if (params->diag_bitmap & 0x40)
		WSP(sp, SLT_Debug,
		    "Object %u workspace free %u", o->xid, WS_Free(o->ws_o));

	Lck_Lock(&oh->mtx);
	assert(oh->refcnt > 0);
	oc->flags &= ~OC_F_BUSY;
	AZ(sp->wrk->nbusyobj);
	sp->wrk->nbusyobj = oc->busyobj;
	oc->busyobj = NULL;
	hsh_rush(oh);
	AN(oc->ban);
	Lck_Unlock(&oh->mtx);
	assert(oc_getobj(sp->wrk, oc) == o);
}

void
HSH_Ref(struct objcore *oc)
{
	struct objhead *oh;

	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
	oh = oc->objhead;
	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);
	Lck_Lock(&oh->mtx);
	assert(oc->refcnt > 0);
	oc->refcnt++;
	Lck_Unlock(&oh->mtx);
}

/*--------------------------------------------------------------------
 * Dereference objcore and or object
 *
 * Can deal with:
 *	bare objcore (incomplete fetch)
 *	bare object (pass)
 *	object with objcore
 *	XXX later:  objcore with object (?)
 *
 * But you can only supply one of the two arguments at a time.
 *
 * Returns zero if target was destroyed.
 */

int
HSH_Deref(struct worker *w, struct objcore *oc, struct object **oo)
{
	struct object *o = NULL;
	struct objhead *oh;
	unsigned r;

	/* Only one arg at a time */
	assert(oc == NULL || oo == NULL);

	if (oo != NULL) {
		o = *oo;
		*oo = NULL;
		CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);
		oc = o->objcore;
	}

	if (o != NULL && oc == NULL) {
		/*
		 * A pass object with neither objcore nor objhdr reference.
		 * -> simply free the (Transient) storage
		 */
		STV_Freestore(o);
		STV_free(o->objstore);
		w->stats.n_object--;
		return (0);
	}

	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);

	oh = oc->objhead;
	CHECK_OBJ_NOTNULL(oh, OBJHEAD_MAGIC);

	Lck_Lock(&oh->mtx);
	assert(oh->refcnt > 0);
	assert(oc->refcnt > 0);
	r = --oc->refcnt;
	if (!r)
		VSLIST_REMOVE(&oh->objcore_head, oc, objcore, hsh_list);
	else {
		/* Must have an object */
		AN(oc->methods);
	}
	hsh_rush(oh);
	Lck_Unlock(&oh->mtx);
	if (r != 0)
		return (r);

	BAN_DestroyObj(oc);
	AZ(oc->ban);

	if (oc->flags & OC_F_BUSY) {
		CHECK_OBJ_NOTNULL(oc->busyobj, BUSYOBJ_MAGIC);
		if (w->nbusyobj == NULL)
			w->nbusyobj = oc->busyobj;
		else
			FREE_OBJ(oc->busyobj);
		oc->busyobj = NULL;
	}
	AZ(oc->busyobj);

	if (oc->methods != NULL) {
		oc_freeobj(oc);
		w->stats.n_object--;
	}
	FREE_OBJ(oc);

	w->stats.n_objectcore--;
	/* Drop our ref on the objhead */
	assert(oh->refcnt > 0);
	if (HTB_Deref(oh))
		return (0);
	HSH_DeleteObjHead(w, oh);
	return (0);
}

void
HSH_Init(void)
{
	assert(DIGEST_LEN == SHA256_LEN);	/* avoid #include pollution */
	HTB_Init();
}
