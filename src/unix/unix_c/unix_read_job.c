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
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <string.h>

#include "lwt_unix.h"

struct job_read {
    struct lwt_unix_job job;
    /* The file descriptor. */
    int fd;
    /* The amount of data to read. */
    long length;
    /* The OCaml string. */
    value string;
    /* The offset in the string. */
    long offset;
    /* The result of the read syscall. */
    long result;
    /* The value of errno. */
    int error_code;
    /* The temporary buffer. */
    char buffer[];
};

static void worker_read(struct job_read *job)
{
    job->result = read(job->fd, job->buffer, job->length);
    job->error_code = errno;
}

static value result_read(struct job_read *job)
{
    long result = job->result;
    if (result < 0) {
        int error_code = job->error_code;
        caml_remove_generational_global_root(&(job->string));
        lwt_unix_free_job(&job->job);
        unix_error(error_code, "read", Nothing);
    } else {
        memcpy(String_val(job->string) + job->offset, job->buffer, result);
        caml_remove_generational_global_root(&(job->string));
        lwt_unix_free_job(&job->job);
        return Val_long(result);
    }
}

CAMLprim value lwt_unix_read_job(value val_fd, value val_buffer,
                                 value val_offset, value val_length)
{
    long length = Long_val(val_length);
    LWT_UNIX_INIT_JOB(job, read, length);
    job->fd = Int_val(val_fd);
    job->length = length;
    job->string = val_buffer;
    job->offset = Long_val(val_offset);
    caml_register_generational_global_root(&(job->string));
    return lwt_unix_alloc_job(&(job->job));
}
#endif
