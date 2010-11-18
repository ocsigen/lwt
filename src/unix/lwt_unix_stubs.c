/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_stubs
 * Copyright (C) 2009-2010 Jérémie Dimino
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

#define _GNU_SOURCE

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/config.h>
#include <caml/custom.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <dirent.h>
#include <pwd.h>
#include <grp.h>
#include <netdb.h>
#include <termios.h>

#include "lwt_unix.h"

#if defined(LWT_ON_WINDOWS)
#  include <windows.h>
#else
#  include <sys/socket.h>
#  include <pthread.h>
#endif

/* +-----------------------------------------------------------------+
   | OS-dependent functions                                          |
   +-----------------------------------------------------------------+ */

#if defined(LWT_ON_WINDOWS)
#  include "lwt_unix_windows.c"
#else
#  include "lwt_unix_unix.c"
#endif

/* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ */

void *lwt_unix_malloc(size_t size)
{
  void *ptr = malloc(size);
  if (ptr == NULL) {
    perror("cannot allocate memory");
    abort();
  }
  return ptr;
}

char *lwt_unix_strdup(char *str)
{
  char *new_str = strdup(str);
  if (new_str == NULL) {
    perror("cannot allocate memory");
    abort();
  }
  return new_str;
}

/* +-----------------------------------------------------------------+
   | Byte order                                                      |
   +-----------------------------------------------------------------+ */

value lwt_unix_system_byte_order()
{
#ifdef ARCH_BIG_ENDIAN
  return Val_int(1);
#else
  return Val_int(0);
#endif
}

/* +-----------------------------------------------------------------+
   | Notifications                                                   |
   +-----------------------------------------------------------------+ */

int notification_fd_writer = -1;

lwt_unix_mutex notification_pipe_mutex;

value lwt_unix_init_notification(value fd)
{
  notification_fd_writer = FD_val(fd);
  lwt_unix_initialize_mutex(notification_pipe_mutex);
  return Val_unit;
}

CAMLprim value lwt_unix_send_notification_stub(value val_id)
{
  char buf[4];
  int id = Val_int(val_id);

  buf[0] = id;
  buf[1] = id >> 8;
  buf[2] = id >> 16;
  buf[3] = id >> 24;

  caml_enter_blocking_section();

  lwt_unix_acquire_mutex(notification_pipe_mutex);

  int offset = 0;
  while (offset < 4) {
    int n = write(notification_fd_writer, &(buf[offset]), 4 - offset);

    if (n <= 0) {
      lwt_unix_release_mutex(notification_pipe_mutex);
      caml_leave_blocking_section();
      uerror("send_notification", Nothing);
    }

    offset += n;
  }

  lwt_unix_release_mutex(notification_pipe_mutex);

  caml_leave_blocking_section();

  return Val_unit;
}

void lwt_unix_send_notification(int id)
{
  char buf[4];

  buf[0] = id;
  buf[1] = id >> 8;
  buf[2] = id >> 16;
  buf[3] = id >> 24;

  lwt_unix_acquire_mutex(notification_pipe_mutex);

  int offset = 0;
  while (offset < 4) {
    int n = write(notification_fd_writer, &(buf[offset]), 4 - offset);

    if (n <= 0) {
      perror("failed to send notification");
      break;
    }

    offset += n;
  }

  lwt_unix_release_mutex(notification_pipe_mutex);
}

/* +-----------------------------------------------------------------+
   | Jobs                                                            |
   +-----------------------------------------------------------------+ */

/* Description of jobs. */
struct custom_operations job_ops = {
  "lwt.unix.job",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

/* Get the job structure contained in a custom value. */
#define Job_val(v) *(lwt_unix_job*)Data_custom_val(v)

value lwt_unix_alloc_job(lwt_unix_job job)
{
  value val_job = caml_alloc_custom(&job_ops, sizeof(lwt_unix_job), 0, 1);
  Job_val(val_job) = job;
  return val_job;
}

/* The function executed by worker threads. */
static void *worker(void *data)
{
  lwt_unix_job job = (lwt_unix_job)data;

  /* Execute the job. */
  job->worker(job);

  switch (job->async_method) {

  case LWT_UNIX_ASYNC_METHOD_NONE:
    job->done = 1;
    break;

  case LWT_UNIX_ASYNC_METHOD_DETACH:
    lwt_unix_acquire_mutex(job->mutex);

    /* Job is done. If the main thread stopped until now, asynchronous
       notification is not necessary. */
    job->done = 1;

    /* Send a notification if the main thread continued its execution
       before the job terminated. */
    if (job->fast == 0) {
      lwt_unix_release_mutex(job->mutex);
      lwt_unix_send_notification(job->notification_id);
    } else
      lwt_unix_release_mutex(job->mutex);

    break;

  case LWT_UNIX_ASYNC_METHOD_SWITCH:
    break;
  }

  return NULL;
}

CAMLprim value lwt_unix_start_job(value val_job, value val_notification_id, value val_async_method)
{
  lwt_unix_job job = Job_val(val_job);

  /* Initialises job parameters. */
  job->done = 0;
  job->fast = 1;
  job->async_method = Int_val(val_async_method);
  job->notification_id = Int_val(val_notification_id);

  switch (job->async_method) {

  case LWT_UNIX_ASYNC_METHOD_NONE:
    /* Execute the job synchronously. */
    caml_enter_blocking_section();
    job->worker(job);
    caml_leave_blocking_section();
    break;

  case LWT_UNIX_ASYNC_METHOD_DETACH:
    lwt_unix_initialize_mutex(job->mutex);
    /* Launch the job on another thread. */
    lwt_unix_launch_thread(worker, (void*)job);
    break;

  case LWT_UNIX_ASYNC_METHOD_SWITCH:
    caml_failwith("the switch async method is not yet implemented");
    break;
  }

  return Val_unit;
}

CAMLprim value lwt_unix_check_job(value val_job)
{
  lwt_unix_job job = Job_val(val_job);

  switch (job->async_method) {

  case LWT_UNIX_ASYNC_METHOD_NONE:
    return Val_int(1);

  case LWT_UNIX_ASYNC_METHOD_DETACH:
  case LWT_UNIX_ASYNC_METHOD_SWITCH:
    lwt_unix_acquire_mutex(job->mutex);
    /* We are not waiting anymore. */
    job->fast = 0;
    value result = Val_bool(job->done);
    lwt_unix_release_mutex(job->mutex);

    return result;
  }

  return Val_int(0);
}

CAMLprim value lwt_unix_cancel_job(value val_job)
{
  struct lwt_unix_job *job = Job_val(val_job);

  switch (job->async_method) {

  case LWT_UNIX_ASYNC_METHOD_NONE:
    break;

  case LWT_UNIX_ASYNC_METHOD_DETACH:
  case LWT_UNIX_ASYNC_METHOD_SWITCH:
    lwt_unix_acquire_mutex(job->mutex);
    if (job->done == 0) {
#if defined(LWT_ON_WINDOWS)
      /* TODO: do something here. */
#else
      pthread_cancel(job->thread);
#endif
    }
    lwt_unix_release_mutex(job->mutex);
    break;
  }

  return Val_unit;
}
