/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



/* Informations:

   - this is the expected prototype of the C function [rmdir]:

       int rmdir(char* path)

   - these are the expected ocaml externals for this job:

       external rmdir_job : string -> unit Lwt_unix.job = "lwt_unix_rmdir_job"
       external rmdir_sync : string -> unit = "lwt_unix_rmdir_sync"
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

/* Structure holding informations for calling [rmdir]. */
struct job_rmdir {
  /* Informations used by lwt. It must be the first field of the structure. */
  struct lwt_unix_job job;
  /* This field store the result of the call. */
  int result;
  /* This field store the value of [errno] after the call. */
  int errno_copy;
  /* in parameter. */
  char* path;
  /* Buffer for string parameters. */
  char data[];
};

/* The function calling [rmdir]. */
static void worker_rmdir(struct job_rmdir* job)
{
  /* Perform the blocking call. */
  job->result = rmdir(job->path);
  /* Save the value of errno. */
  job->errno_copy = errno;
}

/* The function building the caml result. */
static value result_rmdir(struct job_rmdir* job)
{
  /* Check for errors. */
  if (job->result < 0) {
    /* Save the value of errno so we can use it once the job has been freed. */
    int error = job->errno_copy;
    /* Copy the contents of job->path into a caml string. */
    value string_argument = caml_copy_string(job->path);
    /* Free the job structure. */
    lwt_unix_free_job(&job->job);
    /* Raise the error. */
    unix_error(error, "rmdir", string_argument);
  }
  /* Free the job structure. */
  lwt_unix_free_job(&job->job);
  /* Return the result. */
  return Val_unit;
}

/* The stub creating the job structure. */
CAMLprim value lwt_unix_rmdir_job(value path)
{
  /* Get the length of the path parameter. */
  mlsize_t len_path = caml_string_length(path) + 1;
  /* Allocate a new job. */
  struct job_rmdir* job = lwt_unix_new_plus(struct job_rmdir, len_path);
  /* Set the offset of the path parameter inside the job structure. */
  job->path = job->data;
  /* Copy the path parameter inside the job structure. */
  memcpy(job->path, String_val(path), len_path);
  /* Initializes function fields. */
  job->job.worker = (lwt_unix_job_worker)worker_rmdir;
  job->job.result = (lwt_unix_job_result)result_rmdir;
  /* Wrap the structure into a caml value. */
  return lwt_unix_alloc_job(&job->job);
}

#else /* !defined(LWT_ON_WINDOWS) */

CAMLprim value lwt_unix_rmdir_job(value Unit)
{
  lwt_unix_not_available("rmdir");
  return Val_unit;
}

#endif /* !defined(LWT_ON_WINDOWS) */
