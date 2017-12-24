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

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <dirent.h>
#include <sys/types.h>

#include "lwt_unix.h"

struct job_closedir {
    struct lwt_unix_job job;
    int result;
    int error_code;
    DIR *dir;
};

static void worker_closedir(struct job_closedir *job)
{
    job->result = closedir(job->dir);
    job->error_code = errno;
}

static value result_closedir(struct job_closedir *job)
{
    LWT_UNIX_CHECK_JOB(job, job->dir < 0, "closedir");
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_closedir_job(value dir)
{
    LWT_UNIX_INIT_JOB(job, closedir, 0);
    job->dir = DIR_Val(dir);
    return lwt_unix_alloc_job(&job->job);
}
#endif
