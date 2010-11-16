/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_open
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

CAMLprim value lwt_unix_open_job()
{
  invalid_argument("not implemented");
}

CAMLprim value lwt_unix_open_result()
{
  invalid_argument("not implemented");
}

CAMLprim value lwt_unix_open_free()
{
  invalid_argument("not implemented");
}

#else /* defined(LWT_ON_WINDOWS) */

#include <caml/alloc.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif
#ifndef O_DSYNC
#define O_DSYNC 0
#endif
#ifndef O_SYNC
#define O_SYNC 0
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#endif

static int open_flag_table[] = {
  O_RDONLY,
  O_WRONLY,
  O_RDWR,
  O_NONBLOCK,
  O_APPEND,
  O_CREAT,
  O_TRUNC,
  O_EXCL,
  O_NOCTTY,
  O_DSYNC,
  O_SYNC,
  O_RSYNC
};

struct job {
  LWT_UNIX_JOB_FIELDS;
  char *path;
  int flags;
  int perms;
  int fd;
  int blocking;
  int error_code;
};

#define Job_val(v) *(struct job**)Data_custom_val(v)

static void worker(struct job *job)
{
  int fd;
  fd = open(job->path, job->flags, job->perms);
  job->fd = fd;
  job->error_code = errno;
  if (fd >= 0) {
    struct stat stat;
    if (fstat(fd, &stat) < 0)
      job->blocking = 1;
    else
      job->blocking = S_ISFIFO(stat.st_mode) || S_ISSOCK(stat.st_mode);
  }
}

CAMLprim value lwt_unix_open_job(value val_path, value val_flags, value val_perms)
{
  struct job *job = lwt_unix_new(struct job);
  job->path = lwt_unix_strdup(String_val(val_path));
  job->flags = convert_flag_list(val_flags, open_flag_table);
  job->perms = Int_val(val_perms);
  job->worker = (lwt_unix_job_worker)worker;
  return lwt_unix_alloc_job((lwt_unix_job)job);
}

CAMLprim value lwt_unix_open_result(value val_job)
{
  struct job *job = Job_val(val_job);
  int fd = job->fd;
  if (fd < 0) unix_error(job->error_code, "open", Nothing);
  value result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(fd);
  Field(result, 1) = Val_bool(job->blocking);
  return result;
}

CAMLprim value lwt_unix_open_free(value val_job)
{
  struct job *job = Job_val(val_job);
  free(job->path);
  free(job);
  return Val_unit;
}

#endif
