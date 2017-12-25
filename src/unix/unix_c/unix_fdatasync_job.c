/* OCaml promise library
 * http://www.ocsigen.org/lwt
 *
 * Copyright (C) 2012 Jérémie Dimino
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

/* Informations:

   - this is the expected prototype of the C function [fdatasync]:

       int fdatasync(int fd)

   - these are the expected ocaml externals for this job:

       external fdatasync_job : Unix.file_descr -> unit Lwt_unix.job = "lwt_unix_fdatasync_job"
       external fdatasync_sync : Unix.file_descr -> unit = "lwt_unix_fdatasync_sync"
*/

/* Caml headers. */
#define CAML_NAME_SPACE
#include <lwt_unix.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>

#if !defined(LWT_ON_WINDOWS) && defined(HAVE_FDATASYNC)

/* Specific headers. */
#include <errno.h>
#include <string.h>
#include <unistd.h>

/* +-----------------------------------------------------------------+
   | Asynchronous job                                                |
   +-----------------------------------------------------------------+ */

/* Structure holding informations for calling [fdatasync]. */
struct job_fdatasync {
  /* Informations used by lwt. It must be the first field of the structure. */
  struct lwt_unix_job job;
  /* This field store the result of the call. */
  int result;
  /* This field store the value of [errno] after the call. */
  int errno_copy;
  /* in parameter. */
  int fd;
};

/* The function calling [fdatasync]. */
static void worker_fdatasync(struct job_fdatasync* job)
{
  /* Perform the blocking call. */
  job->result = fdatasync(job->fd);
  /* Save the value of errno. */
  job->errno_copy = errno;
}

/* The function building the caml result. */
static value result_fdatasync(struct job_fdatasync* job)
{
  /* Check for errors. */
  if (job->result < 0) {
    /* Save the value of errno so we can use it once the job has been freed. */
    int error = job->errno_copy;
    /* Free the job structure. */
    lwt_unix_free_job(&job->job);
    /* Raise the error. */
    unix_error(error, "fdatasync", Nothing);
  }
  /* Free the job structure. */
  lwt_unix_free_job(&job->job);
  /* Return the result. */
  return Val_unit;
}

/* The stub creating the job structure. */
CAMLprim value lwt_unix_fdatasync_job(value fd)
{
  /* Allocate a new job. */
  struct job_fdatasync* job = lwt_unix_new(struct job_fdatasync);
  /* Initializes function fields. */
  job->job.worker = (lwt_unix_job_worker)worker_fdatasync;
  job->job.result = (lwt_unix_job_result)result_fdatasync;
  /* Copy the fd parameter. */
  job->fd = Int_val(fd);
  /* Wrap the structure into a caml value. */
  return lwt_unix_alloc_job(&job->job);
}

#else /* !defined(LWT_ON_WINDOWS) && defined(HAVE_FDATASYNC) */

CAMLprim value lwt_unix_fdatasync_job(value Unit)
{
  lwt_unix_not_available("fdatasync");
  return Val_unit;
}

#endif /* !defined(LWT_ON_WINDOWS) && defined(HAVE_FDATASYNC) */
