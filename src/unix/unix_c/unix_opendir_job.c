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
#include <dirent.h>
#include <sys/types.h>

#include "lwt_unix.h"

struct job_opendir {
    struct lwt_unix_job job;
    DIR *result;
    int error_code;
    char *path;
    char data[];
};

static void worker_opendir(struct job_opendir *job)
{
    job->result = opendir(job->path);
    job->error_code = errno;
}

static value result_opendir(struct job_opendir *job)
{
    LWT_UNIX_CHECK_JOB_ARG(job, job->result == NULL, "opendir", job->path);
    value result = caml_alloc_small(1, Abstract_tag);
    DIR_Val(result) = job->result;
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_opendir_job(value path)
{
    LWT_UNIX_INIT_JOB_STRING(job, opendir, 0, path);
    return lwt_unix_alloc_job(&job->job);
}
#endif
