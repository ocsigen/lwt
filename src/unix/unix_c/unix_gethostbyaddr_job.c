/* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2009-2010 Jérémie Dimino
 *               2009 Mauricio Fernandez
 *               2010 Pierre Chambart
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#define _GNU_SOURCE

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>

#include "lwt_unix.h"

#include "unix_get_network_information_utils.h"

struct job_gethostbyaddr {
    struct lwt_unix_job job;
    struct in_addr addr;
    struct hostent entry;
    struct hostent *ptr;
#ifndef NON_R_GETHOSTBYADDR
    char buffer[NETDB_BUFFER_SIZE];
#endif
};

static void worker_gethostbyaddr(struct job_gethostbyaddr *job)
{
#if HAS_GETHOSTBYADDR_R == 7
    int h_errno;
    job->ptr = gethostbyaddr_r(&job->addr, 4, AF_INET, &job->entry, job->buffer,
                               NETDB_BUFFER_SIZE, &h_errno);
#elif HAS_GETHOSTBYADDR_R == 8
    int h_errno;
    if (gethostbyaddr_r(&job->addr, 4, AF_INET, &job->entry, job->buffer,
                        NETDB_BUFFER_SIZE, &job->ptr, &h_errno) != 0)
        job->ptr = NULL;
#else
    job->ptr = gethostbyaddr(&job->addr, 4, AF_INET);
    if (job->ptr) {
        job->ptr = hostent_dup(job->ptr);
        if (job->ptr) {
            job->entry = *job->ptr;
        }
    }
#endif
}

static value result_gethostbyaddr(struct job_gethostbyaddr *job)
{
    if (job->ptr == NULL) {
        lwt_unix_free_job(&job->job);
        caml_raise_not_found();
    } else {
        value entry = alloc_host_entry(&job->entry);
#ifdef NON_R_GETHOSTBYADDR
        hostent_free(job->ptr);
#endif
        lwt_unix_free_job(&job->job);
        return entry;
    }
}

CAMLprim value lwt_unix_gethostbyaddr_job(value val_addr)
{
    LWT_UNIX_INIT_JOB(job, gethostbyaddr, 0);
    job->addr = GET_INET_ADDR(val_addr);
    return lwt_unix_alloc_job(&job->job);
}
#endif
