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
 * LRU and timer handling.
 *
 * LRU list is used for killing old objects in the case of cache overflow.
 * Each object also has expiration timeout, but objects aren't deleted
 * immediately after they expired. Instead, they remain live until the next
 * call to EXP_IsExpired() or until LRU killer kills them.
 * This works faster than using sophisitcated datastructures like priority
 * queues for immediate objects' epxiration.
 *
 * We hold a single object reference for LRU list.
 *
 * An attempted overview:
 *
 *	                        EXP_Ttl()      EXP_Grace()   EXP_Keep()
 *				   |                |            |
 *      entered                    v                v            |
 *         |                       +--------------->+            |
 *         v                       |      grace                  |
 *         +---------------------->+                             |
 *                  ttl            |                             v
 *                                 +---------------------------->+
 *                                     keep
 *
 */

#include "config.h"

#include <math.h>

#include "cache.h"

#include "hash/hash_slinger.h"
#include "vtim.h"

static pthread_t exp_thread;
static struct objcore *exp_list;
static struct lock exp_list_mtx;

/*--------------------------------------------------------------------
 * struct exp manipulations
 *
 * The Get/Set functions encapsulate the mutual magic between the
 * fields in one single place.
 */

void
EXP_Clr(struct exp *e)
{

	e->ttl = -1;
	e->grace = -1;
	e->keep = -1;
	e->age = 0;
	e->entered = 0;
}

#define EXP_ACCESS(fld, low_val, extra)				\
	double							\
	EXP_Get_##fld(const struct exp *e)			\
	{							\
		return (e->fld > 0. ? e->fld : low_val);	\
	}							\
								\
	void							\
	EXP_Set_##fld(struct exp *e, double v)			\
	{							\
		if (v > 0.)					\
			e->fld = v;				\
		else {						\
			e->fld = -1.;				\
			extra;					\
		}						\
	}							\

EXP_ACCESS(ttl, -1., (e->grace = e->keep = -1.))
EXP_ACCESS(grace, 0., )
EXP_ACCESS(keep, 0.,)

/*--------------------------------------------------------------------
 * Calculate an objects effective keep, grace or ttl time, suitably
 * adjusted for defaults and by per-session limits.
 */

static double
EXP_Keep(const struct sess *sp, const struct object *o)
{
	double r;

	r = (double)params->default_keep;
	if (o->exp.keep > 0.)
		r = o->exp.keep;
	if (sp != NULL && sp->exp.keep > 0. && sp->exp.keep < r)
		r = sp->exp.keep;
	return (EXP_Ttl(sp, o) + r);
}

double
EXP_Grace(const struct sess *sp, const struct object *o)
{
	double r;

	r = (double)params->default_grace;
	if (o->exp.grace >= 0.)
		r = o->exp.grace;
	if (sp != NULL && sp->exp.grace > 0. && sp->exp.grace < r)
		r = sp->exp.grace;
	return (EXP_Ttl(sp, o) + r);
}

double
EXP_Ttl(const struct sess *sp, const struct object *o)
{
	double r;

	r = o->exp.ttl;
	if (sp != NULL && sp->exp.ttl > 0. && sp->exp.ttl < r)
		r = sp->exp.ttl;
	return (o->exp.entered + r);
}

/*--------------------------------------------------------------------
 * When & why does the timer fire for this object ?
 */

static double
get_object_when(const struct object *o)
{
	double when, w2;

	CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);

	when = EXP_Keep(NULL, o);
	w2 = EXP_Grace(NULL, o);
	if (w2 > when)
		when = w2;
	assert(!isnan(when));
	return when;
}

/*--------------------------------------------------------------------*/

static void
lru_insert(struct objcore *oc, struct lru *lru)
{
	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
	CHECK_OBJ_NOTNULL(lru, LRU_MAGIC);
	AZ(oc->on_lru);

	Lck_AssertHeld(&lru->mtx);
	VTAILQ_INSERT_TAIL(&lru->lru_head, oc, lru_list);
	oc->on_lru = 1;
}

static void
lru_remove(struct objcore *oc, struct lru *lru)
{
	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
	CHECK_OBJ_NOTNULL(lru, LRU_MAGIC);
	AN(oc->on_lru);

	Lck_AssertHeld(&lru->mtx);
	VTAILQ_REMOVE(&lru->lru_head, oc, lru_list);
	oc->on_lru = 0;
}

/*--------------------------------------------------------------------
 * Object has been added to cache, record in lru.
 *
 * The objcore comes with a reference, which we inherit.
 */

void
EXP_Inject(struct objcore *oc, struct lru *lru, double when)
{

	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
	CHECK_OBJ_NOTNULL(lru, LRU_MAGIC);

	Lck_Lock(&lru->mtx);
	/* XXX: use per-object mutex for timer_when locking? */
	oc->timer_when = when;
	lru_insert(oc, lru);
	Lck_Unlock(&lru->mtx);
}

/*--------------------------------------------------------------------
 * Object has been added to cache, record in lru.
 *
 * We grab a reference to the object, which will keep it around until
 * we decide its time to let it go.
 */

void
EXP_Insert(struct object *o)
{
	struct objcore *oc;
	struct lru *lru;

	CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);
	oc = o->objcore;
	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
	AssertObjBusy(o);
	HSH_Ref(oc);

	assert(o->exp.entered != 0 && !isnan(o->exp.entered));
	o->last_lru = o->exp.entered;

	lru = oc_getlru(oc);
	CHECK_OBJ_NOTNULL(lru, LRU_MAGIC);
	Lck_Lock(&lru->mtx);
	/* XXX: use per-object mutex for timer_when locking? */
	oc->timer_when = get_object_when(o);
	lru_insert(oc, lru);
	Lck_Unlock(&lru->mtx);
	oc_updatemeta(oc);
}

/*--------------------------------------------------------------------
 * Object was used, move to tail of LRU list.
 *
 * EXP_Touch() MUST be called only after EXP_Insert() or EXP_Inject()
 * were called. These functions properly insert the object into LRU list.
 * It is OK if EXP_Touch() is called after the object has been removed
 * from LRU list due to expiration.
 */

int
EXP_Touch(struct objcore *oc)
{
	struct lru *lru;

	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);

	/*
	 * For -spersistent we don't move objects on the lru list.  Each
	 * segment has its own LRU list, and the order on it is not material
	 * for anything.  The code below would move the objects to the
	 * LRU list of the currently open segment, which would prevent
	 * the cleaner from doing its job.
	 */
	if (oc->flags & OC_F_LRUDONTMOVE)
		return (0);

	lru = oc_getlru(oc);
	CHECK_OBJ_NOTNULL(lru, LRU_MAGIC);
	if (Lck_Trylock(&lru->mtx))
		return (0);

	/*
	 * If the object isn't in LRU, then it is expired.
	 * Don't ressurect expired objects, because they will be killed
	 * soon anyway.
	 */
	if (oc->on_lru) {
		lru_remove(oc, lru);
		lru_insert(oc, lru);
		VSC_C_main->n_lru_moved++;
	}
	Lck_Unlock(&lru->mtx);
	return (1);
}

/*--------------------------------------------------------------------
 * We have changed one or more of the object timers, update
 * oc->timer_when accordingly.
 *
 * The VCL code can send us here on a non-cached object, just return.
 *
 * XXX: special case check for ttl = 0 ?
 */

void
EXP_Rearm(const struct object *o)
{
	struct objcore *oc;
	struct lru *lru;
	double when;

	CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);
	oc = o->objcore;
	if (oc == NULL)
		return;
	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
	lru = oc_getlru(oc);
	CHECK_OBJ_NOTNULL(lru, LRU_MAGIC);
        when = get_object_when(o);
	Lck_Lock(&lru->mtx);
	/* XXX: use per-object mutex for timer_when locking? */
	oc->timer_when = when;
	Lck_Unlock(&lru->mtx);
	oc_updatemeta(oc);
}


/*--------------------------------------------------------------------
 * This thread kills all objects from exp_list.
 */

static void * __match_proto__(void *start_routine(void *))
exp_timer(struct sess *sp, void *priv)
{
	struct objcore *oc;
	struct object *o;

	(void)priv;
	while (1) {
		Lck_Lock(&exp_list_mtx);
		if (exp_list == NULL) {
			Lck_Unlock(&exp_list_mtx);
			WSL_Flush(sp->wrk, 0);
			WRK_SumStat(sp->wrk);
			VTIM_sleep(params->expiry_sleep);
			continue;
		}
		oc = exp_list;
		CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
		exp_list = VTAILQ_NEXT(oc, lru_list);
		Lck_Unlock(&exp_list_mtx);

		VSC_C_main->n_expired++;

		o = oc_getobj(sp->wrk, oc);
		CHECK_OBJ_NOTNULL(o, OBJECT_MAGIC);
		WSL(sp->wrk, SLT_ExpKill, 0, "%u %.0f",
		    o->xid, EXP_Ttl(NULL, o) - VTIM_real());
		(void)HSH_Deref(sp->wrk, oc, NULL);
	}
	NEEDLESS_RETURN(NULL);
}


/*--------------------------------------------------------------------
 * Checks whether the given object is expired. If it is expired, then
 * schedule its' deletion.
 * Returns: 1: expired, 0: not expired.
 */

int
EXP_IsExpired(struct objcore *oc, double t_req)
{
	struct lru *lru;

	CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
	AZ(oc->flags & OC_F_BUSY);

	/*
	 * Remove the object from LRU list, since this is quite fast operation.
	 * Delegate object deletion to exp_thread, since it can be time
	 * consuming operation, so can negatively impact response time if
	 * the object had been swapped out from RAM.
	 */
	lru = oc_getlru(oc);
	CHECK_OBJ_NOTNULL(lru, LRU_MAGIC);
	if (Lck_Trylock(&lru->mtx))
		return (0);	/* No luck now - will be lucky next time. */

	if (oc->timer_when > t_req) {
		Lck_Unlock(&lru->mtx);
		return (0);
	}
	if (!oc->on_lru) {
		Lck_Unlock(&lru->mtx);
		return (1);
	}
	lru_remove(oc, lru);
	AZ(oc->on_lru);
	Lck_Unlock(&lru->mtx);

	Lck_Lock(&exp_list_mtx);
	/*
	 * Smart hack: since oc->lru_list is unused after removing
	 * the object from LRU list, let's use it for building exp_list :)
	 * There is no need in acquiring lru->mtx during oc->lru_list
	 * modifications, because the object cannot be re-used by cache again.
	 * So exp_list_mtx protection is enough.
	 */
	VTAILQ_NEXT(oc, lru_list) = exp_list;
	exp_list = oc;
	Lck_Unlock(&exp_list_mtx);
	return (1);
}

/*--------------------------------------------------------------------
 * Attempt to make space by nuking the oldest object on the LRU list
 * which isn't in use.
 * Returns: 1: did, 0: didn't, -1: can't
 */

int
EXP_NukeOne(struct worker *w, struct lru *lru)
{
	struct objcore *oc;
	struct object *o;

	/* Find the first currently unused object on the LRU. */
	Lck_Lock(&lru->mtx);
	VTAILQ_FOREACH(oc, &lru->lru_head, lru_list) {
		CHECK_OBJ_NOTNULL(oc, OBJCORE_MAGIC);
		/*
		 * It wont release any space if we cannot release the last
		 * reference, besides, if somebody else has a reference,
		 * it's a bad idea to nuke this object anyway.
		 */
		if (oc->refcnt == 1)
			break;
	}
	if (oc != NULL) {
		lru_remove(oc, lru);
		VSC_C_main->n_lru_nuked++;
	}
	Lck_Unlock(&lru->mtx);

	if (oc == NULL)
		return (-1);

	/* XXX: bad idea for -spersistent */
	o = oc_getobj(w, oc);
	WSL(w, SLT_ExpKill, 0, "%u LRU", o->xid);
	(void)HSH_Deref(w, NULL, &o);
	return (1);
}

/*--------------------------------------------------------------------*/

void
EXP_Init(void)
{
	Lck_New(&exp_list_mtx, lck_exp);
	AZ(exp_list);
	WRK_BgThread(&exp_thread, "cache-timeout", exp_timer, NULL);
}
