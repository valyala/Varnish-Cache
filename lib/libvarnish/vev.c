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
 */

#include "config.h"

#include <errno.h>
#include <limits.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "binary_heap.h"
#include "miniobj.h"
#include "vas.h"
#include "vqueue.h"
#include "vev.h"
#include "vtim.h"

#undef DEBUG_EVENTS

/* INFTIM indicates an infinite timeout for poll(2) */
#ifndef INFTIM
#define INFTIM -1
#endif

struct vevsig {
	struct vev_base		*vevb;
	struct vev		*vev;
	struct sigaction	sigact;
	unsigned char		happened;
};

static struct vevsig		*vev_sigs;
static int			vev_nsig;

struct vev_base {
	MAGIC_HERE;
#define VEV_BASE_MAGIC		0x477bcf3dU

	VTAILQ_HEAD(,vev)	events;
	struct pollfd		*pfd;
	unsigned		npfd;
	unsigned		lpfd;
	struct binheap		*binheap;
	unsigned char		compact_pfd;
	unsigned char		disturbed;
	unsigned		psig;
	pthread_t		thread;
#ifdef DEBUG_EVENTS
	FILE			*debug;
#endif
};

/*--------------------------------------------------------------------*/

#ifdef DEBUG_EVENTS
#define DBG(evb, ...) do {				\
	if ((evb)->debug != NULL)			\
		fprintf((evb)->debug, __VA_ARGS__);	\
	} while (0);
#else
#define DBG(evb, ...)	/* ... */
#endif

/*--------------------------------------------------------------------*/

static void
vev_bh_update(void *a, unsigned idx)
{
	struct vev *e;

	CAST_OBJ_NOTNULL(e, a, VEV_MAGIC);
	e->__binheap_idx = idx;
}

static int
vev_bh_cmp(void *a, void *b)
{
	struct vev *ea, *eb;

	CAST_OBJ_NOTNULL(ea, a, VEV_MAGIC);
	CAST_OBJ_NOTNULL(eb, b, VEV_MAGIC);
	return (ea->__when < eb->__when);
}

/*--------------------------------------------------------------------*/

static void
vev_get_pfd(struct vev_base *evb)
{
	unsigned u;

	if (evb->lpfd + 1 < evb->npfd)
		return;

	if (evb->npfd < 8)
		u = 8;
	else if (evb->npfd > 256)
		u = evb->npfd + 256;
	else
		u = evb->npfd * 2;
	REALLOC_ARRAY_NOTNULL(evb->pfd, u);
	evb->npfd = u;
}

/*--------------------------------------------------------------------*/

static void
vev_get_sig(int sig)
{
	struct vevsig *os;

	if (sig < vev_nsig)
		return;

	CALLOC_NOTNULL(os, sig + 1L);
	memcpy(os, vev_sigs, vev_nsig * sizeof *os);

	FREE_ORNULL(vev_sigs);
	vev_sigs = os;
	vev_nsig = sig + 1;
}

/*--------------------------------------------------------------------*/

static void
vev_sighandler(int sig)
{
	struct vevsig *es;

	assert(sig < vev_nsig);
	assert(vev_sigs != NULL);
	es = &vev_sigs[sig];
	if (!es->happened)
		es->vevb->psig++;
	es->happened = 1;
}

/*--------------------------------------------------------------------*/

struct vev_base *
vev_new_base(void)
{
	struct vev_base *evb;

	ALLOC_OBJ_NOTNULL(evb, VEV_BASE_MAGIC);
	vev_get_pfd(evb);
	VTAILQ_INIT(&evb->events);
	evb->binheap = binheap_new(vev_bh_cmp, vev_bh_update);
	AN(evb->binheap);
	evb->thread = pthread_self();
#ifdef DEBUG_EVENTS
	evb->debug = fopen("/tmp/_.events", "w");
	AN(evb->debug);
	setbuf(evb->debug, NULL);
	DBG(evb, "\n\nStart debugging\n");
#endif
	return (evb);
}

/*--------------------------------------------------------------------*/

void
vev_destroy_base(struct vev_base *evb)
{
	CHECK_OBJ_NOTNULL(evb, VEV_BASE_MAGIC);
	assert(evb->thread == pthread_self());
	FREE_OBJ_NOTNULL(evb, VEV_BASE_MAGIC);
}

/*--------------------------------------------------------------------*/

struct vev *
vev_new(void)
{
	struct vev *e;

	ALLOC_OBJ_NOTNULL(e, VEV_MAGIC);
	e->fd = -1;
	e->__binheap_idx = BINHEAP_NOIDX;
	return (e);
}

/*--------------------------------------------------------------------*/

void
vev_add(struct vev_base *evb, struct vev *e)
{
	struct vevsig *es;

	CHECK_OBJ_NOTNULL(evb, VEV_BASE_MAGIC);
	CHECK_OBJ_NOTNULL(e, VEV_MAGIC);
	assert(e->callback != NULL);
	assert(e->sig >= 0);
	assert(e->timeout >= 0.0);
	assert(e->fd < 0 || e->fd_flags);
	assert(evb->thread == pthread_self());
	DBG(evb, "ev_add(%p) fd = %d\n", e, e->fd);

	if (e->sig > 0)
		vev_get_sig(e->sig);

	if (e->fd >= 0)
		vev_get_pfd(evb);

	if (e->sig > 0) {
		es = &vev_sigs[e->sig];
		XXXAZ(es->vev);
		assert(es->happened == 0);
		es->vev = e;
		es->vevb = evb;
		es->sigact.sa_flags = e->sig_flags;
		es->sigact.sa_handler = vev_sighandler;
	} else {
		es = NULL;
	}

	if (e->fd >= 0) {
		assert(evb->lpfd < evb->npfd);
		evb->pfd[evb->lpfd].fd = e->fd;
		evb->pfd[evb->lpfd].events =
		    e->fd_flags & (EV_RD|EV_WR|EV_ERR|EV_HUP);
		e->__poll_idx = evb->lpfd;
		evb->lpfd++;
		DBG(evb, "... pidx = %d lpfd = %d\n",
		    e->__poll_idx, evb->lpfd);
	} else
		e->__poll_idx = -1;

	assert(e->__binheap_idx == BINHEAP_NOIDX);
	if (e->timeout != 0.0) {
		e->__when = VTIM_mono() + e->timeout;
		binheap_insert(evb->binheap, e);
		assert(e->__binheap_idx != BINHEAP_NOIDX);
	}

	e->__vevb = evb;
	e->__privflags = 0;
	if (e->fd < 0)
		VTAILQ_INSERT_TAIL(&evb->events, e, __list);
	else
		VTAILQ_INSERT_HEAD(&evb->events, e, __list);

	if (e->sig > 0) {
		assert(es != NULL);
		xxxassert(sigaction(e->sig, &es->sigact, NULL) == 0);
	}
}

/*--------------------------------------------------------------------*/

void
vev_del(struct vev_base *evb, struct vev *e)
{
	struct vevsig *es;

	CHECK_OBJ_NOTNULL(evb, VEV_BASE_MAGIC);
	CHECK_OBJ_NOTNULL(e, VEV_MAGIC);
	DBG(evb, "ev_del(%p) fd = %d\n", e, e->fd);
	assert(evb == e->__vevb);
	assert(evb->thread == pthread_self());

	if (e->__binheap_idx != BINHEAP_NOIDX) {
		binheap_delete(evb->binheap, e->__binheap_idx);
		assert(e->__binheap_idx == BINHEAP_NOIDX);
	}

	if (e->fd >= 0) {
		DBG(evb, "... pidx = %d\n", e->__poll_idx);
		evb->pfd[e->__poll_idx].fd = -1;
		if (e->__poll_idx == evb->lpfd - 1)
			evb->lpfd--;
		else
			evb->compact_pfd++;
		e->fd = -1;
		DBG(evb, "... lpfd = %d\n", evb->lpfd);
	}

	if (e->sig > 0) {
		assert(e->sig < vev_nsig);
		es = &vev_sigs[e->sig];
		assert(es->vev == e);
		es->vev = NULL;
		es->vevb = NULL;
		es->sigact.sa_flags = e->sig_flags;
		es->sigact.sa_handler = SIG_DFL;
		xxxassert(sigaction(e->sig, &es->sigact, NULL) == 0);
		es->happened = 0;
	}

	VTAILQ_REMOVE(&evb->events, e, __list);

	e->__vevb = NULL;
	FREE_OBJ_NOTNULL(e, VEV_MAGIC);
	evb->disturbed = 1;
}

/*--------------------------------------------------------------------*/

int
vev_schedule(struct vev_base *evb)
{
	int i;

	CHECK_OBJ_NOTNULL(evb, VEV_BASE_MAGIC);
	assert(evb->thread == pthread_self());
	do
		i = vev_schedule_one(evb);
	while (i == 1);
	return (i);
}

/*--------------------------------------------------------------------*/

static void
vev_compact_pfd(struct vev_base *evb)
{
	unsigned u;
	struct pollfd *p;
	struct vev *ep;
	int lfd;

	DBG(evb, "compact_pfd() lpfd = %d\n", evb->lpfd);
	p = evb->pfd;
	for (u = 0; u < evb->lpfd; u++, p++) {
		DBG(evb, "...[%d] fd = %d\n", u, p->fd);
		if (p->fd >= 0)
			continue;
		if (u == evb->lpfd - 1)
			break;
		lfd = evb->pfd[evb->lpfd - 1].fd;
		VTAILQ_FOREACH(ep, &evb->events, __list)
			if (ep->fd == lfd)
				break;
		AN(ep);
		DBG(evb, "...[%d] move %p pidx %d\n", u, ep, ep->__poll_idx);
		*p = evb->pfd[--evb->lpfd];
		ep->__poll_idx = u;
	}
	evb->lpfd = u;
	evb->compact_pfd = 0;
	DBG(evb, "... lpfd = %d\n", evb->lpfd);
}

/*--------------------------------------------------------------------*/

static int
vev_sched_timeout(struct vev_base *evb, struct vev *e, double t)
{
	int i;

	i = e->callback(e, 0);
	if (i)
		vev_del(evb, e);
	else {
		e->__when = t + e->timeout;
		assert(e->__binheap_idx != BINHEAP_NOIDX);
		binheap_reorder(evb->binheap, e->__binheap_idx);
		assert(e->__binheap_idx != BINHEAP_NOIDX);
	}
	return (1);
}

static int
vev_sched_signal(struct vev_base *evb)
{
	int i, j;
	struct vevsig *es;
	struct vev *e;

	es = vev_sigs;
	for (j = 0; j < vev_nsig; j++, es++) {
		if (!es->happened || es->vevb != evb)
			continue;
		evb->psig--;
		es->happened = 0;
		e = es->vev;
		assert(e != NULL);
		i = e->callback(e, EV_SIG);
		if (i)
			vev_del(evb, e);
	}
	return (1);
}

int
vev_schedule_one(struct vev_base *evb)
{
	double t;
	struct vev *e, *e2, *e3;
	int i, j, tmo;
	struct pollfd *pfd;

	CHECK_OBJ_NOTNULL(evb, VEV_BASE_MAGIC);
	assert(evb->thread == pthread_self());
	e = binheap_root(evb->binheap);
	if (e != NULL) {
		CHECK_OBJ_NOTNULL(e, VEV_MAGIC);
		assert(e->__binheap_idx != BINHEAP_NOIDX);
		t = VTIM_mono();
		if (e->__when <= t)
			return (vev_sched_timeout(evb, e, t));
		if (e->__when - t > INT_MAX / 1e3)
			tmo = INT_MAX;
		else
			tmo = (int) ((e->__when - t) * 1e3);
		if (tmo == 0)
			tmo = 1;
	} else
		tmo = INFTIM;

	if (evb->compact_pfd)
		vev_compact_pfd(evb);

	if (tmo == INFTIM && evb->lpfd == 0)
		return (0);

	if (evb->psig)
		return (vev_sched_signal(evb));
	assert(evb->lpfd < evb->npfd);
	i = poll(evb->pfd, evb->lpfd, tmo);
	if (i == -1 && errno == EINTR)
		return (vev_sched_signal(evb));
	if (i == 0) {
		assert(e != NULL);
		t = VTIM_mono();
		if (e->__when <= t)
			return (vev_sched_timeout(evb, e, t));
	}
	evb->disturbed = 0;
	VTAILQ_FOREACH_SAFE(e, &evb->events, __list, e2) {
		if (i == 0)
			break;
		if (e->fd < 0)
			continue;
		assert(e->__poll_idx < evb->lpfd);
		pfd = &evb->pfd[e->__poll_idx];
		assert(pfd->fd == e->fd);
		if (!pfd->revents)
			continue;
		DBG(evb, "callback(%p) fd = %d what = 0x%x pidx = %d\n",
		    e, e->fd, pfd->revents, e->__poll_idx);
		j = e->callback(e, pfd->revents);
		i--;
		if (evb->disturbed) {
			VTAILQ_FOREACH(e3, &evb->events, __list) {
				if (e3 == e) {
					e3 = VTAILQ_NEXT(e, __list);
					break;
				} else if (e3 == e2)
					break;
			}
			e2 = e3;
			evb->disturbed = 0;
		}
		if (j) {
			vev_del(evb, e);
			evb->disturbed = 0;
		}
	}
	assert(i == 0);
	return (1);
}
