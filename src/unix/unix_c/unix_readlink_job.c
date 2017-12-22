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

#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>

#include "lwt_unix.h"

struct job_readlink {
    struct lwt_unix_job job;
    char *buffer;
    ssize_t result;
    int error_code;
    char *name;
    char data[];
};

static void worker_readlink(struct job_readlink *job)
{
    ssize_t buffer_size = 1024;
    ssize_t link_length;

    for (;;) {
        job->buffer = lwt_unix_malloc(buffer_size + 1);
        link_length = readlink(job->name, job->buffer, buffer_size);

        if (link_length < 0) {
            free(job->buffer);
            job->result = -1;
            job->error_code = errno;
            return;
        }
        if (link_length < buffer_size) {
            job->buffer[link_length] = 0;
            job->result = link_length;
            return;
        } else {
            free(job->buffer);
            buffer_size *= 2;
        }
    }
}

static value result_readlink(struct job_readlink *job)
{
    LWT_UNIX_CHECK_JOB_ARG(job, job->result < 0, "readlink", job->name);
    value result = caml_copy_string(job->buffer);
    free(job->buffer);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_readlink_job(value name)
{
    LWT_UNIX_INIT_JOB_STRING(job, readlink, 0, name);
    return lwt_unix_alloc_job(&(job->job));
}
#endif
