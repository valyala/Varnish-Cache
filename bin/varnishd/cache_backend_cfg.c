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
 * Handle configuration of backends from VCL programs.
 *
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>

#include "cache.h"

#include "cache_backend.h"
#include "miniobj.h"
#include "vcli.h"
#include "vcli_priv.h"
#include "vrt.h"

struct lock VBE_mtx;

/*
 * The list of backends is not locked, it is only ever accessed from
 * the CLI thread, so there is no need.
 */
static VTAILQ_HEAD(, backend) backends = VTAILQ_HEAD_INITIALIZER(backends);

/*--------------------------------------------------------------------
 */

static void
VBE_Nuke(struct backend *b)
{

	ASSERT_CLI();
	CHECK_OBJ_NOTNULL(b, BACKEND_MAGIC);
	VTAILQ_REMOVE(&backends, b, list);
	FREE_ORNULL(b->ipv4);
	FREE_ORNULL(b->ipv4_addr);
	FREE_ORNULL(b->ipv6);
	FREE_ORNULL(b->ipv6_addr);
	FREE_ORNULL(b->port);
	VSM_Free(b->vsc);
	FREE_OBJ_NOTNULL(b, BACKEND_MAGIC);
	VSC_C_main->n_backend--;
}

/*--------------------------------------------------------------------
 */

void
VBE_Poll(void)
{
	struct backend *b, *b2;

	ASSERT_CLI();
	VTAILQ_FOREACH_SAFE(b, &backends, list, b2) {
		if (b->refcount == 0 && b->probe == NULL)
			VBE_Nuke(b);
	}
}

/*--------------------------------------------------------------------
 * Drop a reference to a backend.
 * The last reference must come from the watcher in the CLI thread,
 * as only that thread is allowed to clean up the backend list.
 */

void
VBE_DropRefLocked(struct backend *b)
{
	int i;
	struct vbc *vbe, *vbe2;

	CHECK_OBJ_NOTNULL(b, BACKEND_MAGIC);
	assert(b->refcount > 0);

	i = --b->refcount;
	Lck_Unlock(&b->mtx);
	if (i > 0)
		return;

	ASSERT_CLI();
	VTAILQ_FOREACH_SAFE(vbe, &b->connlist, list, vbe2) {
		VTAILQ_REMOVE(&b->connlist, vbe, list);
		if (vbe->fd >= 0) {
			AZ(close(vbe->fd));
			vbe->fd = -1;
		}
		vbe->backend = NULL;
		VBE_ReleaseConn(vbe);
	}
	VBE_Nuke(b);
}

void
VBE_DropRefVcl(struct backend *b)
{

	CHECK_OBJ_NOTNULL(b, BACKEND_MAGIC);

	Lck_Lock(&b->mtx);
	b->vsc->vcls--;
	VBE_DropRefLocked(b);
}

void
VBE_DropRefConn(struct backend *b)
{

	CHECK_OBJ_NOTNULL(b, BACKEND_MAGIC);

	Lck_Lock(&b->mtx);
	assert(b->n_conn > 0);
	b->n_conn--;
	VBE_DropRefLocked(b);
}

/*--------------------------------------------------------------------
 * See lib/libvcl/vcc_backend.c::emit_sockaddr()
 */

static void
copy_sockaddr(struct sockaddr_storage **sa, socklen_t *len,
    const unsigned char *src)
{

	assert(*src > 0);
	CALLOC_NOTNULL(*sa, 1);
	memcpy(*sa, src + 1, *src);
	*len = *src;
}

/*--------------------------------------------------------------------
 * Add a backend/director instance when loading a VCL.
 * If an existing backend is matched, grab a refcount and return.
 * Else create a new backend structure with reference initialized to one.
 */

struct backend *
VBE_AddBackend(struct cli *cli, const struct vrt_backend *vb)
{
	struct backend *b;
	char buf[128];

	AN(vb->vcl_name);
	assert(vb->ipv4_sockaddr != NULL || vb->ipv6_sockaddr != NULL);
	(void)cli;
	ASSERT_CLI();

	/* Run through the list and see if we already have this backend */
	VTAILQ_FOREACH(b, &backends, list) {
		CHECK_OBJ_NOTNULL(b, BACKEND_MAGIC);
		if (strcmp(b->vcl_name, vb->vcl_name))
			continue;
		if (vb->ipv4_sockaddr != NULL && (
		    b->ipv4len != vb->ipv4_sockaddr[0] ||
		    memcmp(b->ipv4, vb->ipv4_sockaddr + 1, b->ipv4len)))
			continue;
		if (vb->ipv6_sockaddr != NULL && (
		    b->ipv6len != vb->ipv6_sockaddr[0] ||
		    memcmp(b->ipv6, vb->ipv6_sockaddr + 1, b->ipv6len)))
			continue;
		b->refcount++;
		b->vsc->vcls++;
		return (b);
	}

	/* Create new backend */
	ALLOC_OBJ_NOTNULL(b, BACKEND_MAGIC);
	Lck_New(&b->mtx, lck_backend);
	b->refcount = 1;

	bprintf(buf, "%s(%s,%s,%s)",
	    vb->vcl_name,
	    vb->ipv4_addr == NULL ? "" : vb->ipv4_addr,
	    vb->ipv6_addr == NULL ? "" : vb->ipv6_addr, vb->port);

	b->vsc = VSM_Alloc(sizeof *b->vsc, VSC_CLASS, VSC_TYPE_VBE, buf);
	b->vsc->vcls++;

	VTAILQ_INIT(&b->connlist);

	VTAILQ_INIT(&b->troublelist);

	/*
	 * This backend may live longer than the VCL that instantiated it
	 * so we cannot simply reference the VCL's copy of things.
	 */
	REPLACE(b->vcl_name, vb->vcl_name);
	REPLACE(b->display_name, buf);
	REPLACE(b->ipv4_addr, vb->ipv4_addr);
	REPLACE(b->ipv6_addr, vb->ipv6_addr);
	REPLACE(b->port, vb->port);

	/*
	 * Copy over the sockaddrs
	 */
	if (vb->ipv4_sockaddr != NULL)
		copy_sockaddr(&b->ipv4, &b->ipv4len, vb->ipv4_sockaddr);
	if (vb->ipv6_sockaddr != NULL)
		copy_sockaddr(&b->ipv6, &b->ipv6len, vb->ipv6_sockaddr);

	assert(b->ipv4 != NULL || b->ipv6 != NULL);

	b->healthy = 1;
	b->admin_health = from_probe;

	VTAILQ_INSERT_TAIL(&backends, b, list);
	VSC_C_main->n_backend++;
	return (b);
}

/*--------------------------------------------------------------------*/

void
VRT_init_dir(struct cli *cli, struct director **dir, const char *name,
    int idx, const void *priv)
{

	ASSERT_CLI();
	if (!strcmp(name, "simple"))
		VRT_init_dir_simple(cli, dir, idx, priv);
	else if (!strcmp(name, "hash"))
		VRT_init_dir_hash(cli, dir, idx, priv);
	else if (!strcmp(name, "random"))
		VRT_init_dir_random(cli, dir, idx, priv);
	else if (!strcmp(name, "dns"))
		VRT_init_dir_dns(cli, dir, idx, priv);
	else if (!strcmp(name, "round-robin"))
		VRT_init_dir_round_robin(cli, dir, idx, priv);
	else if (!strcmp(name, "fallback"))
		VRT_init_dir_fallback(cli, dir, idx, priv);
	else if (!strcmp(name, "client"))
		VRT_init_dir_client(cli, dir, idx, priv);
	else
		INCOMPL();
}

void
VRT_fini_dir(struct cli *cli, struct director *b)
{

	(void)cli;
	ASSERT_CLI();
	CHECK_OBJ_NOTNULL(b, DIRECTOR_MAGIC);
	b->fini(b);
	b->priv = NULL;
}

/*--------------------------------------------------------------------*/

static int
backend_find(const char *matcher, struct backend **r, int n)
{
	struct backend *b;
	char *vcl_name;
	char *s;
	char *match_ip = NULL;
	char *match_port = NULL;
	int found = 0;

	s = strchr(matcher, '(');

	if (s == NULL) {
		/* Simple match, max one hit */
		VTAILQ_FOREACH(b, &backends, list) {
			CHECK_OBJ_NOTNULL(b, BACKEND_MAGIC);
			if (strcmp(b->vcl_name, matcher) == 0) {
				if (r && found < n)
					r[found] = b;
				found++;
			}
		}
		return found;
	}

	vcl_name = strndup(matcher, s - matcher);
	AN(vcl_name);
	s++;
	while (*s != ')') {
		if (*s == ':') {
			/* Port */
			s++;
			match_port = s;
			if (!(s = strchr(match_port, ','))) {
				s = strchr(match_port, ')');
			}
			XXXAN(s);
			match_port = strndup(match_port, s - match_port);
			AN(match_port);
			if (*s == ',')
				s++;
		} else {
			/* IP */
			match_ip = s;
			if (!(s = strchr(match_ip, ','))) {
				s = strchr(match_ip, ')');
			}
			XXXAN(s);
			match_ip = strndup(match_ip, s - match_ip);
			AN(match_ip);
			if (*s == ',')
				s++;
		}
	}
	VTAILQ_FOREACH(b, &backends, list) {
		CHECK_OBJ_NOTNULL(b, BACKEND_MAGIC);
		if (match_port && strcmp(b->port, match_port) != 0)
			continue;
		if (match_ip &&
		    (strcmp(b->ipv4_addr, match_ip) != 0) &&
		    (strcmp(b->ipv6_addr, match_ip) != 0))
			continue;
		if (strcmp(b->vcl_name, vcl_name) == 0) {
			if (r && found < n)
				r[found] = b;
			found++;
		}
	}
	return found;
}

static void
cli_backend_list(struct cli *cli, const char * const *av, void *priv)
{
	struct backend *b;
	const char *ah;

	(void)av;
	(void)priv;
	ASSERT_CLI();
	VCLI_Out(cli, "%-30s %10s %15s %15s", "Backend name",
		 "Conns", "Probed healthy", "Admin health");
	VTAILQ_FOREACH(b, &backends, list) {
		CHECK_OBJ_NOTNULL(b, BACKEND_MAGIC);
		if (b->admin_health == from_probe) {
			ah = "Auto";
		} else if (b->admin_health == sick) {
			ah = "Sick";
		} else {
			ah = "Healthy";
		}
		VCLI_Out(cli, "\n%-30s %10d %15s %15s",
			 b->display_name,
			 b->refcount,
			 (b ->healthy ? "Yes" : "No"),
			 ah);
	}
}

static void
cli_backend_set_health(struct cli *cli, const char * const *av, void *priv)
{
	struct backend **b;
	enum health_status state;
	int n;
	const char *wstate;

	(void)av;
	(void)priv;
	ASSERT_CLI();
	wstate = av[3];
	if (strcmp(wstate, "healthy") == 0) {
		state = healthy;
	} else if (strcmp(wstate, "sick") == 0) {
		state = sick;
	} else if (strcmp(wstate, "auto") == 0) {
		state = from_probe;
	} else {
		VCLI_Out(cli, "Invalid state %s", wstate);
		VCLI_SetResult(cli, CLIS_CANT);
		return;
	}
	n = backend_find(av[2], NULL, 0);
	if (n == 0) {
		VCLI_Out(cli, "No matching backends");
		VCLI_SetResult(cli, CLIS_CANT);
		return;
	}

	b = calloc(n, sizeof(struct backend *));
	AN(b);
	n = backend_find(av[2], b, n);

	VCLI_Out(cli, "Set state to %s for the following backends:", wstate);
	for (int i = 0; i < n; i++) {
		b[i]->admin_health = state;
		VCLI_Out(cli, "\n\t%s", b[i]->display_name);
	}
}

static struct cli_proto backend_cmds[] = {
	{ "backend.list", "backend.list",
	    "\tList all backends\n", 0, 0, "d", cli_backend_list },
	{ "backend.set_health", "backend.set_health matcher state",
	    "\tShow a backend\n", 2, 2, "d", cli_backend_set_health },
	{ NULL }
};

/*--------------------------------------------------------------------*/

void
VBE_Init(void)
{

	Lck_New(&VBE_mtx, lck_vbe);
	CLI_AddFuncs(backend_cmds);
}
