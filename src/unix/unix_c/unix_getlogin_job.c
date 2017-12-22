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
#include <unistd.h>

#include "lwt_unix.h"

#if !defined(__ANDROID__)

struct job_getlogin {
    struct lwt_unix_job job;
    char buffer[1024];
    int result;
};

static void worker_getlogin(struct job_getlogin *job)
{
    job->result = getlogin_r(job->buffer, 1024);
}

static value result_getlogin(struct job_getlogin *job)
{
    int result = job->result;
    if (result) {
        lwt_unix_free_job(&job->job);
        unix_error(result, "getlogin", Nothing);
    } else {
        value v = caml_copy_string(job->buffer);
        lwt_unix_free_job(&job->job);
        return v;
    }
}

CAMLprim value lwt_unix_getlogin_job(value Unit)
{
    LWT_UNIX_INIT_JOB(job, getlogin, 0);
    return lwt_unix_alloc_job(&job->job);
}

#else

LWT_NOT_AVAILABLE1(unix_getlogin_job)

#endif
#endif
