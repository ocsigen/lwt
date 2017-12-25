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

   - this is the expected prototype of the C function [rename]:

       int rename(char* oldpath, char* newpath)

   - these are the expected ocaml externals for this job:

       external rename_job : string -> string -> unit Lwt_unix.job = "lwt_unix_rename_job"
       external rename_sync : string -> string -> unit = "lwt_unix_rename_sync"
*/

/* Caml headers. */
#define CAML_NAME_SPACE
#include <lwt_unix.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>

#if !defined(LWT_ON_WINDOWS)

/* Specific headers. */
#include <errno.h>
#include <string.h>
#include <stdio.h>

/* +-----------------------------------------------------------------+
   | Asynchronous job                                                |
   +-----------------------------------------------------------------+ */

/* Structure holding informations for calling [rename]. */
struct job_rename {
  /* Informations used by lwt. It must be the first field of the structure. */
  struct lwt_unix_job job;
  /* This field store the result of the call. */
  int result;
  /* This field store the value of [errno] after the call. */
  int errno_copy;
  /* in parameter. */
  char* oldpath;
  /* in parameter. */
  char* newpath;
  /* Buffer for string parameters. */
  char data[];
};

/* The function calling [rename]. */
static void worker_rename(struct job_rename* job)
{
  /* Perform the blocking call. */
  job->result = rename(job->oldpath, job->newpath);
  /* Save the value of errno. */
  job->errno_copy = errno;
}

/* The function building the caml result. */
static value result_rename(struct job_rename* job)
{
  /* Check for errors. */
  if (job->result < 0) {
    /* Save the value of errno so we can use it once the job has been freed. */
    int error = job->errno_copy;
    /* Copy the contents of job->oldpath into a caml string. */
    value string_argument = caml_copy_string(job->oldpath);
    /* Free the job structure. */
    lwt_unix_free_job(&job->job);
    /* Raise the error. */
    unix_error(error, "rename", string_argument);
  }
  /* Free the job structure. */
  lwt_unix_free_job(&job->job);
  /* Return the result. */
  return Val_unit;
}

/* The stub creating the job structure. */
CAMLprim value lwt_unix_rename_job(value oldpath, value newpath)
{
  /* Get the length of the oldpath parameter. */
  mlsize_t len_oldpath = caml_string_length(oldpath) + 1;
  /* Get the length of the newpath parameter. */
  mlsize_t len_newpath = caml_string_length(newpath) + 1;
  /* Allocate a new job. */
  struct job_rename* job = lwt_unix_new_plus(struct job_rename, len_oldpath + len_newpath);
  /* Set the offset of the oldpath parameter inside the job structure. */
  job->oldpath = job->data;
  /* Set the offset of the newpath parameter inside the job structure. */
  job->newpath = job->data + len_oldpath;
  /* Copy the oldpath parameter inside the job structure. */
  memcpy(job->oldpath, String_val(oldpath), len_oldpath);
  /* Copy the newpath parameter inside the job structure. */
  memcpy(job->newpath, String_val(newpath), len_newpath);
  /* Initializes function fields. */
  job->job.worker = (lwt_unix_job_worker)worker_rename;
  job->job.result = (lwt_unix_job_result)result_rename;
  /* Wrap the structure into a caml value. */
  return lwt_unix_alloc_job(&job->job);
}

#else /* !defined(LWT_ON_WINDOWS) */

CAMLprim value lwt_unix_rename_job(value Unit)
{
  lwt_unix_not_available("rename");
  return Val_unit;
}

#endif /* !defined(LWT_ON_WINDOWS) */
