/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_close
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

#include "lwt_unix.h"

#if defined(LWT_ON_WINDOWS)

LWT_UNIX_JOB_NOT_IMPLEMENTED(close)

#else /* defined(LWT_ON_WINDOWS) */

struct job {
  LWT_UNIX_JOB_FIELDS;
  int fd;
  int result;
  int error_code;
};

#define Job_val(v) *(struct job**)Data_custom_val(v)

static void worker(struct job *job)
{
  job->result = close(job->fd);
  job->error_code = errno;
}

CAMLprim value lwt_unix_close_job(value val_fd)
{
  struct job *job = lwt_unix_new(struct job);
  job->fd = Int_val(val_fd);
  job->worker = (lwt_unix_job_worker)worker;
  return lwt_unix_alloc_job((lwt_unix_job)job);
}

CAMLprim value lwt_unix_close_result(value val_job)
{
  struct job *job = Job_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "close", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_close_free(value val_job)
{
  free(Job_val(val_job));
  return Val_unit;
}

#endif
