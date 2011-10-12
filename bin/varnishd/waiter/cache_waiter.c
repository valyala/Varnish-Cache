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

#include "waiter/cache_waiter.h"
#include "vcli.h"
#include "vcli_priv.h"

static const struct waiter * const vca_waiters[] = {
    #if defined(HAVE_KQUEUE)
	&waiter_kqueue,
    #endif
    #if defined(HAVE_EPOLL_CTL)
	&waiter_epoll,
    #endif
    #if defined(HAVE_PORT_CREATE)
	&waiter_ports,
    #endif
	&waiter_poll,
	NULL,
};

struct waiter const * waiter;

const char *
WAIT_GetName(void)
{

	if (waiter != NULL)
		return (waiter->name);
	else
		return ("no_waiter");
}

void
WAIT_tweak_waiter(struct cli *cli, const char *arg)
{
	int i;

	ASSERT_MGT();

	if (arg == NULL) {
		if (waiter == NULL)
			VCLI_Out(cli, "default");
		else
			VCLI_Out(cli, "%s", waiter->name);

		VCLI_Out(cli, " (");
		for (i = 0; vca_waiters[i] != NULL; i++)
			VCLI_Out(cli, "%s%s", i == 0 ? "" : ", ",
			    vca_waiters[i]->name);
		VCLI_Out(cli, ")");
		return;
	}
	if (!strcmp(arg, "default")) {
		waiter = NULL;
		return;
	}
	for (i = 0; vca_waiters[i]; i++) {
		if (!strcmp(arg, vca_waiters[i]->name)) {
			waiter = vca_waiters[i];
			return;
		}
	}
	VCLI_Out(cli, "Unknown waiter");
	VCLI_SetResult(cli, CLIS_PARAM);
}

void
WAIT_Init(void)
{

	if (waiter == NULL)
		waiter = vca_waiters[0];

	AN(waiter);
	AN(waiter->name);
	AN(waiter->init);
	AN(waiter->pass);
}
