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

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>

#include "lwt_unix.h"
#include "unix_readv_writev_utils.h"

/* Job and writev primitives for blocking file descriptors. */
struct job_writev {
    struct lwt_unix_job job;
    int fd;
    int error_code;
    ssize_t result;
    size_t count;
    /* Heap-allocated iovec structures. */
    struct iovec *iovecs;
    /* Heap-allocated array of pointers to heap-allocated copies of bytes buffer
       slices. This array is NULL-terminated. */
    char **buffer_copies;
};

static void worker_writev(struct job_writev *job)
{
    job->result = writev(job->fd, job->iovecs, job->count);
    job->error_code = errno;
}

static value result_writev(struct job_writev *job)
{
    char **buffer_copy;
    for (buffer_copy = job->buffer_copies; *buffer_copy != NULL;
         ++buffer_copy) {
        free(*buffer_copy);
    }
    free(job->buffer_copies);
    free(job->iovecs);

    ssize_t result = job->result;
    LWT_UNIX_CHECK_JOB(job, result < 0, "writev");
    lwt_unix_free_job(&job->job);
    return Val_long(result);
}

CAMLprim value lwt_unix_writev_job(value fd, value io_vectors, value val_count)
{
    CAMLparam3(fd, io_vectors, val_count);

    LWT_UNIX_INIT_JOB(job, writev, 0);
    job->fd = Int_val(fd);
    job->count = Long_val(val_count);

    /* Assemble iovec structures on the heap and copy bytes buffer slices. */
    job->iovecs = lwt_unix_malloc(job->count * sizeof(struct iovec));
    /* The extra (+ 1) pointer is for the NULL terminator, in case all buffer
       slices are in bytes buffers. */
    job->buffer_copies = lwt_unix_malloc((job->count + 1) * sizeof(char *));
    flatten_io_vectors(job->iovecs, io_vectors, job->count, job->buffer_copies,
                       NULL);

    CAMLreturn(lwt_unix_alloc_job(&job->job));
}
#endif
