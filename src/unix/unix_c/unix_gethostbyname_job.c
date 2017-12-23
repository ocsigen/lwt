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

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <caml/socketaddr.h>
#include <netdb.h>
#include <stdlib.h>
#include <string.h>

#include "lwt_unix.h"

#include "unix_get_network_information_utils.h"

struct job_gethostbyname {
    struct lwt_unix_job job;
    struct hostent entry;
    struct hostent *ptr;
#ifndef NON_R_GETHOSTBYNAME
    char buffer[NETDB_BUFFER_SIZE];
#endif
    char *name;
    char data[];
};

static void worker_gethostbyname(struct job_gethostbyname *job)
{
#if HAS_GETHOSTBYNAME_R == 5
    int h_errno;
    job->ptr = gethostbyname_r(job->name, &job->entry, job->buffer,
                               NETDB_BUFFER_SIZE, &h_errno);
#elif HAS_GETHOSTBYNAME_R == 6
    int h_errno;
    if (gethostbyname_r(job->name, &job->entry, job->buffer, NETDB_BUFFER_SIZE,
                        &(job->ptr), &h_errno) != 0)
        job->ptr = NULL;
#else
    job->ptr = gethostbyname(job->name);
    if (job->ptr) {
        job->ptr = hostent_dup(job->ptr);
        if (job->ptr) {
            job->entry = *job->ptr;
        }
    }
#endif
}

static value result_gethostbyname(struct job_gethostbyname *job)
{
    if (job->ptr == NULL) {
        lwt_unix_free_job(&job->job);
        caml_raise_not_found();
    } else {
        value entry = alloc_host_entry(&job->entry);
#ifdef NON_R_GETHOSTBYNAME
        hostent_free(job->ptr);
#endif
        lwt_unix_free_job(&job->job);
        return entry;
    }
}

CAMLprim value lwt_unix_gethostbyname_job(value name)
{
    LWT_UNIX_INIT_JOB_STRING(job, gethostbyname, 0, name);
    return lwt_unix_alloc_job(&(job->job));
}
#endif
