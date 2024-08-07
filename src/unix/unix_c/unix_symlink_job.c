/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



/* Informations:

   - this is the expected prototype of the C function [symlink]:

       int symlink(char* oldpath, char* newpath)

   - these are the expected ocaml externals for this job:

       external symlink_job : string -> string -> unit Lwt_unix.job = "lwt_unix_symlink_job"
       external symlink_sync : string -> string -> unit = "lwt_unix_symlink_sync"
*/

/* Caml headers. */
#include "lwt_config.h"
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include "lwt_unix.h"

#if !defined(LWT_ON_WINDOWS)

/* Specific headers. */
#include <errno.h>
#include <string.h>
#include <unistd.h>

/* +-----------------------------------------------------------------+
   | Asynchronous job                                                |
   +-----------------------------------------------------------------+ */

/* Structure holding informations for calling [symlink]. */
struct job_symlink {
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

/* The function calling [symlink]. */
static void worker_symlink(struct job_symlink* job)
{
  /* Perform the blocking call. */
  job->result = symlink(job->oldpath, job->newpath);
  /* Save the value of errno. */
  job->errno_copy = errno;
}

/* The function building the caml result. */
static value result_symlink(struct job_symlink* job)
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
    unix_error(error, "symlink", string_argument);
  }
  /* Free the job structure. */
  lwt_unix_free_job(&job->job);
  /* Return the result. */
  return Val_unit;
}

/* The stub creating the job structure. */
CAMLprim value lwt_unix_symlink_job(value oldpath, value newpath)
{
  /* Get the length of the oldpath parameter. */
  mlsize_t len_oldpath = caml_string_length(oldpath) + 1;
  /* Get the length of the newpath parameter. */
  mlsize_t len_newpath = caml_string_length(newpath) + 1;
  /* Allocate a new job. */
  struct job_symlink* job = lwt_unix_new_plus(struct job_symlink, len_oldpath + len_newpath);
  /* Set the offset of the oldpath parameter inside the job structure. */
  job->oldpath = job->data;
  /* Set the offset of the newpath parameter inside the job structure. */
  job->newpath = job->data + len_oldpath;
  /* Copy the oldpath parameter inside the job structure. */
  memcpy(job->oldpath, String_val(oldpath), len_oldpath);
  /* Copy the newpath parameter inside the job structure. */
  memcpy(job->newpath, String_val(newpath), len_newpath);
  /* Initializes function fields. */
  job->job.worker = (lwt_unix_job_worker)worker_symlink;
  job->job.result = (lwt_unix_job_result)result_symlink;
  /* Wrap the structure into a caml value. */
  return lwt_unix_alloc_job(&job->job);
}

#else /* !defined(LWT_ON_WINDOWS) */

CAMLprim value lwt_unix_symlink_job(value Unit)
{
  lwt_unix_not_available("symlink");
  return Val_unit;
}

#endif /* !defined(LWT_ON_WINDOWS) */
