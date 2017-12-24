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

#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#include "lwt_unix.h"

struct job_bytes_write {
    struct lwt_unix_job job;
    union {
        HANDLE handle;
        SOCKET socket;
    } fd;
    int kind;
    char *buffer;
    DWORD length;
    DWORD result;
    DWORD error_code;
};

static void worker_bytes_write(struct job_bytes_write *job)
{
    if (job->kind == KIND_SOCKET) {
        int ret;
        ret = send(job->fd.socket, job->buffer, job->length, 0);
        if (ret == SOCKET_ERROR) job->error_code = WSAGetLastError();
        job->result = ret;
    } else {
        if (!WriteFile(job->fd.handle, job->buffer, job->length, &(job->result),
                       NULL))
            job->error_code = GetLastError();
    }
}

CAMLprim value result_bytes_write(struct job_bytes_write *job)
{
    value result;
    DWORD error = job->error_code;
    if (error) {
        lwt_unix_free_job(&job->job);
        win32_maperr(error);
        uerror("bytes_write", Nothing);
    }
    result = Val_long(job->result);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_bytes_write_job(value val_fd, value val_buffer,
                                        value val_offset, value val_length)
{
    struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
    LWT_UNIX_INIT_JOB(job, bytes_write, 0);
    job->job.worker = (lwt_unix_job_worker)worker_bytes_write;
    job->kind = fd->kind;
    if (fd->kind == KIND_HANDLE)
        job->fd.handle = fd->fd.handle;
    else
        job->fd.socket = fd->fd.socket;
    job->buffer = (char *)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
    job->length = Long_val(val_length);
    job->error_code = 0;
    return lwt_unix_alloc_job(&(job->job));
}
#endif
