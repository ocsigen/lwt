/* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2010 Jérémie Dimino
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

#if defined(LWT_ON_WINDOWS)

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include "lwt_unix.h"

struct job_fsync {
    struct lwt_unix_job job;
    HANDLE handle;
    DWORD error_code;
};

static void worker_fsync(struct job_fsync *job)
{
    if (!FlushFileBuffers(job->handle)) job->error_code = GetLastError();
}

static value result_fsync(struct job_fsync *job)
{
    DWORD error = job->error_code;
    if (error) {
        lwt_unix_free_job(&job->job);
        win32_maperr(error);
        uerror("fsync", Nothing);
    }
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_fsync_job(value val_fd)
{
    struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
    if (fd->kind != KIND_HANDLE) {
        caml_invalid_argument("Lwt_unix.fsync");
    } else {
        LWT_UNIX_INIT_JOB(job, fsync, 0);
        job->handle = fd->fd.handle;
        job->error_code = 0;
        return lwt_unix_alloc_job(&(job->job));
    }
}
#endif
