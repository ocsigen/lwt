/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Header lwt_unix
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

#ifndef __LWT_UNIX_H
#define __LWT_UNIX_H

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <ev.h>

/* Detect the target OS */
#if defined(_WIN32) || defined(_WIN64)
#  define LWT_ON_WINDOWS
#endif

/* Specific OS includes. */
#if defined(LWT_ON_WINDOWS)
#  include <windows.h>
#else
#  include <pthread.h>
#  include <unistd.h>
#  include <string.h>
#  include <errno.h>
#endif

/* The macro to get the file-descriptor from a value. */
#if defined(LWT_ON_WINDOWS)
#  define FD_val(value) win_CRT_fd_of_filedescr(value)
#else
#  define FD_val(value) Int_val(value)
#endif

/* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ */

/* Allocate the given amount of memory and abort the program if there
   is no free memory left. */
void *lwt_unix_malloc(size_t size);

/* Same as [strdup] and abort hte program if there is not memory
   left. */
char *lwt_unix_strdup(char *string);

/* Helper for allocating structures. */
#define lwt_unix_new(type) (type*)lwt_unix_malloc(sizeof(type))

/* +-----------------------------------------------------------------+
   | Notifications                                                   |
   +-----------------------------------------------------------------+ */

/* Sends a notification for the given id. */
void lwt_unix_send_notification(int id);

/* +-----------------------------------------------------------------+
   | Libev                                                           |
   +-----------------------------------------------------------------+ */

/* The libev main loop. */
extern struct ev_loop *lwt_unix_main_loop;

/* Flag which tells whether we are in a blocking section or not. */
extern int lwt_unix_in_blocking_section;

/* Macro to add in libev callbacks. See the manual for
   explanations. */
#define LWT_UNIX_CHECK                          \
  if (lwt_unix_in_blocking_section) {           \
    lwt_unix_in_blocking_section = 0;           \
    caml_leave_blocking_section();              \
  }

/* +-----------------------------------------------------------------+
   | Threading                                                       |
   +-----------------------------------------------------------------+ */

#if defined(LWT_ON_WINDOWS)

typedef HANDLE lwt_unix_thread;
typedef CRITICAL_SECTION lwt_unix_mutex;
typedef CONDITION_VARIABLE lwt_unix_condition;

#define lwt_unix_initialize_mutex(mutex) InitializeCriticalSection(&(mutex))
#define lwt_unix_delete_mutex(mutex) DeleteCriticalSection(&(mutex))
#define lwt_unix_acquire_mutex(mutex) EnterCriticalSection(&(mutex))
#define lwt_unix_release_mutex(mutex) LeaveCriticalSection(&(mutex))

#define lwt_unix_initialize_condition(cond) InitializeConditionVariable(&(cond))
#define lwt_unix_delete_condition(cond)
#define lwt_unix_wait_condition(cond, mutex) SleepConditionVariableCS(&(cond), &(mutex), INFINITE)
#define lwt_unix_wake_condition(cond) WakeConditionVariable(&(cond))

#define lwt_unix_self() GetCurrentThread()
#define lwt_unix_thread_equal(a, b) (a) == (b)

#else /* defined(LWT_ON_WINDOWS) */

typedef pthread_t lwt_unix_thread;
typedef pthread_mutex_t lwt_unix_mutex;
typedef pthread_cond_t lwt_unix_condition;

#define lwt_unix_initialize_mutex(mutex) pthread_mutex_init(&(mutex), NULL)
#define lwt_unix_delete_mutex(mutex)
#define lwt_unix_acquire_mutex(mutex) pthread_mutex_lock(&(mutex))
#define lwt_unix_release_mutex(mutex) pthread_mutex_unlock(&(mutex))

#define lwt_unix_initialize_condition(cond) pthread_cond_init(&(cond), NULL)
#define lwt_unix_delete_condition(cond)
#define lwt_unix_wait_condition(cond, mutex) pthread_cond_wait(&(cond), &(mutex))
#define lwt_unix_wake_condition(cond) pthread_cond_signal(&(cond))

#define lwt_unix_self() pthread_self()
#define lwt_unix_thread_equal(a, b) pthread_equal((a), (b))

#endif /* defined(LWT_ON_WINDOWS) */

/* Launch a thread in detached mode. */
lwt_unix_thread lwt_unix_launch_thread(void* (*start)(void*), void* data);

/* +-----------------------------------------------------------------+
   | Detached jobs                                                   |
   +-----------------------------------------------------------------+ */

/* How job are executed. */
enum lwt_unix_async_method {
  /* Synchronously. */
  LWT_UNIX_ASYNC_METHOD_NONE = 0,

  /* Asynchronously, on another thread. */
  LWT_UNIX_ASYNC_METHOD_DETACH = 1,

  /* Asynchronously, on the main thread, switcing to another thread if
     necessary. */
  LWT_UNIX_ASYNC_METHOD_SWITCH = 2
};

/* Type of job execution modes. */
typedef enum lwt_unix_async_method lwt_unix_async_method;

/* A job descriptor. */
struct lwt_unix_job {
  /* The next job in the queue. */
  struct lwt_unix_job *next;

  /* Id used to notify the main thread in case the job do not
     terminate immediatly. */
  int notification_id;

  /* The function to call to do the work. */
  void (*worker)(struct lwt_unix_job *job);

  /* Is the job terminated ? In case the job is canceled, it will
     always be 0. */
  int done;

  /* Is the main thread still waiting for the job ? */
  int fast;

  /* Mutex to protect access to [done] and [fast]. */
  lwt_unix_mutex mutex;

  /* Thread running the job. */
  lwt_unix_thread thread;

  /* The async method in used by the job. */
  lwt_unix_async_method async_method;
};

/* Type of job descriptors. */
typedef struct lwt_unix_job* lwt_unix_job;

/* Type of worker functions. */
typedef void (*lwt_unix_job_worker)(lwt_unix_job job);

/* Allocate a caml custom value for the given job. */
value lwt_unix_alloc_job(lwt_unix_job job);

/* Define not implement methods. */
#define LWT_UNIX_JOB_NOT_IMPLEMENTED(name)      \
  CAMLprim value lwt_unix_##name##_job()        \
  {                                             \
    invalid_argument("not implemented");        \
  }                                             \
                                                \
  CAMLprim value lwt_unix_##name##_result()     \
  {                                             \
    invalid_argument("not implemented");        \
  }                                             \
                                                \
  CAMLprim value lwt_unix_##name##_free()       \
  {                                             \
    invalid_argument("not implemented");        \
  }


#endif /* __LWT_UNIX_H */
