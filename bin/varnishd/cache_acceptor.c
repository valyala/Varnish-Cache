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
 */

#include "config.h"

#include "cache.h"

#include "vcli.h"
#include "vcli_priv.h"
#include "vtcp.h"
#include "vtim.h"

static pthread_t	VCA_thread;
static struct timeval	tv_sndtimeo;
static struct timeval	tv_rcvtimeo;

/*--------------------------------------------------------------------
 * We want to get out of any kind of trouble-hit TCP connections as fast
 * as absolutely possible, so we set them LINGER enabled with zero timeout,
 * so that even if there are outstanding write data on the socket, a close(2)
 * will return immediately.
 */
static const struct linger linger = {
	.l_onoff	=	0,
};

static unsigned char	need_sndtimeo, need_rcvtimeo, need_linger, need_test;

static void
sock_test(int fd)
{
	struct linger lin;
	struct timeval tv;
	socklen_t l;
	int i;

	l = sizeof lin;
	i = getsockopt(fd, SOL_SOCKET, SO_LINGER, &lin, &l);
	if (i) {
		VTCP_Assert(i);
		return;
	}
	assert(l == sizeof lin);
	if (memcmp(&lin, &linger, l))
		need_linger = 1;

#ifdef SO_SNDTIMEO_WORKS
	l = sizeof tv;
	i = getsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, &l);
	if (i) {
		VTCP_Assert(i);
		return;
	}
	assert(l == sizeof tv);
	if (memcmp(&tv, &tv_sndtimeo, l))
		need_sndtimeo = 1;
#else
	(void)tv;
	(void)tv_sndtimeo;
	(void)need_sndtimeo;
#endif

#ifdef SO_RCVTIMEO_WORKS
	l = sizeof tv;
	i = getsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, &l);
	if (i) {
		VTCP_Assert(i);
		return;
	}
	assert(l == sizeof tv);
	if (memcmp(&tv, &tv_rcvtimeo, l))
		need_rcvtimeo = 1;
#else
	(void)tv;
	(void)tv_rcvtimeo;
	(void)need_rcvtimeo;
#endif

	need_test = 0;
}

/*--------------------------------------------------------------------
 * Called once the workerthread gets hold of the session, to do setup
 * setup overhead, we don't want to bother the acceptor thread with.
 */

void
VCA_Prep(struct sess *sp)
{
	char addr[VTCP_ADDRBUFSIZE];
	char port[VTCP_PORTBUFSIZE];

	VTCP_name(&sp->sockaddr, sp->sockaddrlen,
	    addr, sizeof addr, port, sizeof port);
	sp->addr = WS_Dup(sp->ws, addr);
	sp->port = WS_Dup(sp->ws, port);
	if (cache_param->log_local_addr) {
		AZ(getsockname(sp->fd, (void*)&sp->mysockaddr, &sp->mysockaddrlen));
		VTCP_name(&sp->mysockaddr, sp->mysockaddrlen,
		    addr, sizeof addr, port, sizeof port);
		WSP(sp, SLT_SessionOpen, "%s %s %s %s",
		    sp->addr, sp->port, addr, port);
	} else {
		WSP(sp, SLT_SessionOpen, "%s %s %s",
		    sp->addr, sp->port, sp->mylsock->name);
	}
	sp->acct_ses.first = sp->t_open;
	if (need_test)
		sock_test(sp->fd);
	if (need_linger)
		VTCP_Assert(setsockopt(sp->fd, SOL_SOCKET, SO_LINGER,
		    &linger, sizeof linger));
#ifdef SO_SNDTIMEO_WORKS
	if (need_sndtimeo)
		VTCP_Assert(setsockopt(sp->fd, SOL_SOCKET, SO_SNDTIMEO,
		    &tv_sndtimeo, sizeof tv_sndtimeo));
#endif
#ifdef SO_RCVTIMEO_WORKS
	if (need_rcvtimeo)
		VTCP_Assert(setsockopt(sp->fd, SOL_SOCKET, SO_RCVTIMEO,
		    &tv_rcvtimeo, sizeof tv_rcvtimeo));
#endif
}

/*--------------------------------------------------------------------
 * If accept(2)'ing fails, we pace ourselves to relive any resource
 * shortage if possible.
 */

static double vca_pace = 0.0;
static struct lock pace_mtx;

static void
vca_pace_check(void)
{
	double p;

	if (vca_pace == 0.0)
		return;
	Lck_Lock(&pace_mtx);
	p = vca_pace;
	Lck_Unlock(&pace_mtx);
	if (p > 0.0)
		VTIM_sleep(p);
}

static void
vca_pace_bad(void)
{

	Lck_Lock(&pace_mtx);
	vca_pace += cache_param->acceptor_sleep_incr;
	if (vca_pace > cache_param->acceptor_sleep_max)
		vca_pace = cache_param->acceptor_sleep_max;
	Lck_Unlock(&pace_mtx);
}

static void
vca_pace_good(void)
{

	if (vca_pace == 0.0)
		return;
	Lck_Lock(&pace_mtx);
	vca_pace *= cache_param->acceptor_sleep_decay;
	if (vca_pace < cache_param->acceptor_sleep_incr)
		vca_pace = 0.0;
	Lck_Unlock(&pace_mtx);
}

/*--------------------------------------------------------------------
 * Accept on a listen socket, and handle error returns.
 */

static int hack_ready;

int
VCA_Accept(struct listen_sock *ls, struct wrk_accept *wa)
{
	int i;

	CHECK_OBJ_NOTNULL(ls, LISTEN_SOCK_MAGIC);
	vca_pace_check();

	while(!hack_ready)
		(void)usleep(100*1000);

	wa->acceptaddrlen = sizeof wa->acceptaddr;
	i = accept(ls->sock, (void*)&wa->acceptaddr, &wa->acceptaddrlen);

	if (i < 0) {
		switch (errno) {
		case ECONNABORTED:
			break;
		case EMFILE:
			VSL(SLT_Debug, ls->sock, "Too many open files");
			vca_pace_bad();
			break;
		default:
			VSL(SLT_Debug, ls->sock, "Accept failed: %s",
			    strerror(errno));
			vca_pace_bad();
			break;
		}
	}
	wa->acceptlsock = ls;
	wa->acceptsock = i;
	return (i);
}

/*--------------------------------------------------------------------
 * Fail a session
 *
 * This happens if we accept the socket, but cannot get a session
 * structure.
 *
 * We consider this a DoS situation (false positive:  Extremely popular
 * busy objects) and silently close the connection with minimum effort
 * and fuzz, rather than try to send an intelligent message back.
 */

void
VCA_FailSess(struct worker *w)
{
	struct wrk_accept *wa;

	CHECK_OBJ_NOTNULL(w, WORKER_MAGIC);
	CAST_OBJ_NOTNULL(wa, (void*)w->ws->f, WRK_ACCEPT_MAGIC);
	AZ(w->sp);
	AZ(close(wa->acceptsock));
	w->stats.sess_drop++;
	vca_pace_bad();
}

/*--------------------------------------------------------------------*/

void
VCA_SetupSess(struct worker *w)
{
	struct sess *sp;
	struct wrk_accept *wa;

	CHECK_OBJ_NOTNULL(w, WORKER_MAGIC);
	CAST_OBJ_NOTNULL(wa, (void*)w->ws->f, WRK_ACCEPT_MAGIC);
	sp = w->sp;
	CHECK_OBJ_NOTNULL(sp, SESS_MAGIC);
	sp->fd = wa->acceptsock;
	sp->vsl_id = wa->acceptsock | VSL_CLIENTMARKER ;
	wa->acceptsock = -1;
	sp->t_open = VTIM_real();
	sp->t_end = sp->t_open;
	sp->mylsock = wa->acceptlsock;
	CHECK_OBJ_NOTNULL(sp->mylsock, LISTEN_SOCK_MAGIC);
	assert(wa->acceptaddrlen <= sp->sockaddrlen);
	memcpy(&sp->sockaddr, &wa->acceptaddr, wa->acceptaddrlen);
	sp->sockaddrlen = wa->acceptaddrlen;
	sp->step = STP_FIRST;
	vca_pace_good();
	w->stats.sess_conn++;
}

/*--------------------------------------------------------------------*/

static void *
vca_acct(void *arg)
{
#ifdef SO_RCVTIMEO_WORKS
	double sess_timeout = 0;
#endif
#ifdef SO_SNDTIMEO_WORKS
	double send_timeout = 0;
#endif
	struct listen_sock *ls;
	double t0, now;

	THR_SetName("cache-acceptor");
	(void)arg;

	VTAILQ_FOREACH(ls, &heritage.socks, list) {
		if (ls->sock < 0)
			continue;
		AZ(listen(ls->sock, cache_param->listen_depth));
		AZ(setsockopt(ls->sock, SOL_SOCKET, SO_LINGER,
		    &linger, sizeof linger));
	}

	hack_ready = 1;

	need_test = 1;
	t0 = VTIM_real();
	while (1) {
		(void)sleep(1);
#ifdef SO_SNDTIMEO_WORKS
		if (cache_param->idle_send_timeout != send_timeout) {
			need_test = 1;
			send_timeout = cache_param->idle_send_timeout;
			tv_sndtimeo = VTIM_timeval(send_timeout);
			VTAILQ_FOREACH(ls, &heritage.socks, list) {
				if (ls->sock < 0)
					continue;
				AZ(setsockopt(ls->sock, SOL_SOCKET,
				    SO_SNDTIMEO,
				    &tv_sndtimeo, sizeof tv_sndtimeo));
			}
		}
#endif
#ifdef SO_RCVTIMEO_WORKS
		if (cache_param->sess_timeout != sess_timeout) {
			need_test = 1;
			sess_timeout = cache_param->sess_timeout;
			tv_rcvtimeo = VTIM_timeval(sess_timeout);
			VTAILQ_FOREACH(ls, &heritage.socks, list) {
				if (ls->sock < 0)
					continue;
				AZ(setsockopt(ls->sock, SOL_SOCKET,
				    SO_RCVTIMEO,
				    &tv_rcvtimeo, sizeof tv_rcvtimeo));
			}
		}
#endif
		now = VTIM_real();
		VSC_C_main->uptime = (uint64_t)(now - t0);
	}
	NEEDLESS_RETURN(NULL);
}


/*--------------------------------------------------------------------*/

static void
ccf_start(struct cli *cli, const char * const *av, void *priv)
{

	(void)cli;
	(void)av;
	(void)priv;

	AZ(pthread_create(&VCA_thread, NULL, vca_acct, NULL));
}

/*--------------------------------------------------------------------*/

static void
ccf_listen_address(struct cli *cli, const char * const *av, void *priv)
{
	struct listen_sock *ls;
	char h[32], p[32];

	(void)cli;
	(void)av;
	(void)priv;

	/*
	 * This CLI command is primarily used by varnishtest.  Don't
	 * respond until liste(2) has been called, in order to avoid
	 * a race where varnishtest::client would attempt to connect(2)
	 * before listen(2) has been called.
	 */
	while(!hack_ready)
		(void)usleep(100*1000);

	VTAILQ_FOREACH(ls, &heritage.socks, list) {
		if (ls->sock < 0)
			continue;
		VTCP_myname(ls->sock, h, sizeof h, p, sizeof p);
		VCLI_Out(cli, "%s %s\n", h, p);
	}
}

/*--------------------------------------------------------------------*/

static struct cli_proto vca_cmds[] = {
	{ CLI_SERVER_START,	"i", ccf_start },
	{ "debug.listen_address",
	    "debug.listen_address",
	    "Report the actual listen address\n", 0, 0,
	    "d", ccf_listen_address, NULL },
	{ NULL }
};

void
VCA_Init(void)
{

	CLI_AddFuncs(vca_cmds);
	Lck_New(&pace_mtx, lck_vcapace);
}

void
VCA_Shutdown(void)
{
	struct listen_sock *ls;
	int i;

	VTAILQ_FOREACH(ls, &heritage.socks, list) {
		if (ls->sock < 0)
			continue;
		i = ls->sock;
		ls->sock = -1;
		(void)close(i);
	}
}
