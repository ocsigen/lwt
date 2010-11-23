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
#include <setjmp.h>
#include <assert.h>
#include <sched.h>

#include "lwt_unix.h"

#if defined(LWT_ON_WINDOWS)
#  include <windows.h>
#else
#  include <sys/socket.h>
#  include <pthread.h>
#endif

//#define DEBUG_MODE

#if defined(DEBUG_MODE)
#  include <sys/syscall.h>
#  define DEBUG(fmt, ...) { fprintf(stderr, "lwt-debug[%d]: %s: " fmt "\n", (pid_t)syscall(SYS_gettid), __FUNCTION__, ##__VA_ARGS__); fflush(stderr); }
#else
#  define DEBUG(fmt, ...)
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
  int id = Int_val(val_id);

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
   | Job execution                                                   |
   +-----------------------------------------------------------------+ */

/* Execute the given job. */
static void execute_job(lwt_unix_job job)
{
  DEBUG("executing the job");

  /* Execute the job. */
  job->worker(job);

  DEBUG("job done");

  lwt_unix_acquire_mutex(job->mutex);

  DEBUG("marking the job has done");

  /* Job is done. If the main thread stopped until now, asynchronous
     notification is not necessary. */
  job->done = 1;

  /* Send a notification if the main thread continued its execution
     before the job terminated. */
  if (job->fast == 0) {
    lwt_unix_release_mutex(job->mutex);
    DEBUG("notifying the main thread");
    lwt_unix_send_notification(job->notification_id);
  } else {
    lwt_unix_release_mutex(job->mutex);
    DEBUG("not notifying the main thread");
  }
}

/* +-----------------------------------------------------------------+
   | Thread pool                                                     |
   +-----------------------------------------------------------------+ */

/* Number of thread waiting for a job in the pool. */
static int thread_waiting_count = 0;

/* Number of started threads. */
static int thread_count = 0;

/* Maximum number of system threads that can be started. */
static int pool_size = 1000;

/* Condition on which pool threads are waiting. */
static lwt_unix_condition pool_condition;

/* Queue of pending jobs. It points to the last enqueued job.  */
static lwt_unix_job pool_queue = NULL;

/* The mutex which protect access to [pool_queue], [pool_condition]
   and [thread_waiting_count]. */
static lwt_unix_mutex pool_mutex;

/* +-----------------------------------------------------------------+
   | Thread switching                                                |
   +-----------------------------------------------------------------+ */

/* Possible states of the main thread (i.e. the one executing the
   ocaml code). */
enum main_state {
  /* The main thread is running. */
  STATE_RUNNING,

  /* The main thread is doing a blocking call that has not yet
     terminated. */
  STATE_BLOCKED,
};

/* State of the main thread. */
static enum main_state main_state = STATE_RUNNING;

/* The main thread. */
static lwt_unix_thread main_thread;

/* A node in a list of stack frames. */
struct stack_frame {
  /* The stack frame itself. */
  jmp_buf checkpoint;

  /* The next available one. */
  struct stack_frame *next;
};

/* Stack frames available to do a blocking call. */
static struct stack_frame *blocking_call_enter = NULL;

/* Mutex to protect access to [blocking_call_enter]. */
static lwt_unix_mutex blocking_call_enter_mutex;

/* Where to go when the blocking call is done, or when it get
   scheduled. */
static jmp_buf blocking_call_leave;

/* Where to go to become a worjer */
static struct stack_frame *become_worker = NULL;

/* Value returned to the main thread when a blocking call terminates
   without being scheduled. */
#define CALL_SUCCEEDED 1

/* Value returned to the old main thread whan a blocking call
   terminates but has been scheduled. */
#define CALL_SCHEDULED 2

/* The job to be executed on the first available alternative stack. */
static lwt_unix_job blocking_call = NULL;

/* The stack frame used for the current blocking call. */
static struct stack_frame *blocking_call_frame = NULL;

/* Function executed on an alternative stack. */
static void altstack_worker()
{
  /* The first passage is to register a new stack frame. */
  struct stack_frame *node = lwt_unix_new(struct stack_frame);

  if (setjmp(node->checkpoint) == 0) {

    /* Add it to the list of available stack frames. */
    lwt_unix_acquire_mutex(blocking_call_enter_mutex);
    node->next = blocking_call_enter;
    blocking_call_enter = node;
    lwt_unix_release_mutex(blocking_call_enter_mutex);

  } else {

    /* Save the job to execute and the current stack frame before
       another thread can become the main thread. */
    lwt_unix_job job = blocking_call;
    struct stack_frame *frame = blocking_call_frame;

    /* Mark the main thread as blocked. */
    main_state = STATE_BLOCKED;

    DEBUG("signaling the pool condition variable");

    /* Maybe wakeup a worker so it can become the main thread. */
    lwt_unix_acquire_mutex(pool_mutex);
    lwt_unix_wake_condition(pool_condition);
    lwt_unix_release_mutex(pool_mutex);

    DEBUG("executing the blocking call");

    /* Execute the blocking call. */
    execute_job(job);

    DEBUG("blocking call done");

    lwt_unix_acquire_mutex(pool_mutex);

    if (lwt_unix_thread_equal(main_thread, lwt_unix_self())) {
      /* We stayed the main thread, continue the execution
         normally. */
      main_state = STATE_RUNNING;

      lwt_unix_release_mutex(pool_mutex);

      DEBUG("blocing call terminated without blocking, resuming");

      /* Leave the blocking call. */
      longjmp(blocking_call_leave, CALL_SUCCEEDED);
    } else {
      /* We did not stayed the main thread, we now become a worker. */

      assert(become_worker != NULL);

      /* Take and remove the first worker checkpoint. */
      struct stack_frame *node = become_worker;
      become_worker = node->next;

      lwt_unix_release_mutex(pool_mutex);

      DEBUG("blocking call terminated after blocking, becoming a worker");

      /* Add the stack frame used for this call to the list of
         available ones. */
      lwt_unix_acquire_mutex(blocking_call_enter_mutex);
      frame->next = blocking_call_enter;
      blocking_call_enter = frame;
      /* Release the mutex only after the jump. */

      jmp_buf buf;
      memcpy(&buf, &(node->checkpoint), sizeof(jmp_buf));
      free(node);
      longjmp(buf, 1);
    }
  }
}

#define STACK_SIZE (256 * 1024)

/* Allocate a new stack for doing blocking calls. */
void alloc_new_stack()
{
  DEBUG("allocate a new stack");

  stack_t old_stack, new_stack;
  struct sigaction old_sa, new_sa;

  /* Create the new stack. */
  new_stack.ss_flags = 0;
  new_stack.ss_size = STACK_SIZE;
  new_stack.ss_sp = lwt_unix_malloc(STACK_SIZE);

  /* Change the stack used for signals. */
  sigaltstack(&new_stack, &old_stack);

  /* Set up the custom signal handler. */
  new_sa.sa_handler = altstack_worker;
  new_sa.sa_flags = SA_ONSTACK;
  sigemptyset(&new_sa.sa_mask);
  sigaction(SIGUSR1, &new_sa, &old_sa);

  /* Save the stack frame. */
  raise(SIGUSR1);

  /* Restore the old signal handler. */
  sigaction(SIGUSR1, &old_sa, NULL);

  /* Restore the old alternative stack. */
  sigaltstack(&old_stack, NULL);
}

/* +-----------------------------------------------------------------+
   | Threading stuff initialization                                  |
   +-----------------------------------------------------------------+ */

/* Whether threading has been initialized. */
static int threading_initialized = 0;

/* Initialize the pool of thread. */
void initialize_threading()
{
  if (threading_initialized == 0) {
    lwt_unix_initialize_mutex(pool_mutex);
    lwt_unix_initialize_condition(pool_condition);
    lwt_unix_initialize_mutex(blocking_call_enter_mutex);

    main_thread = lwt_unix_self();

    threading_initialized = 1;
  }
}

/* +-----------------------------------------------------------------+
   | Worker loop                                                     |
   +-----------------------------------------------------------------+ */

/* Function executed by threads of the pool. */
static void* worker_loop(void *data)
{
  lwt_unix_job job = (lwt_unix_job)data;

  /* Execute the initial job if any. */
  if (job != NULL) execute_job(job);

  while (1) {
    DEBUG("entering waiting section");

    lwt_unix_acquire_mutex(pool_mutex);

    /* One more thread is waiting for work. */
    thread_waiting_count++;

    DEBUG("waiting for something to do");

    /* Wait for something to do. */
    while (pool_queue == NULL && main_state == STATE_RUNNING)
      lwt_unix_wait_condition(pool_condition, pool_mutex);

    DEBUG("received something to do");

    /* This thread is busy. */
    thread_waiting_count--;

    if (main_state == STATE_BLOCKED) {
      DEBUG("main thread is blocked");
      DEBUG("\e[1;31mswitching\e[0m");

      /* If the main thread is blocked, we become the main thread. */
      main_thread = lwt_unix_self();

      /* The new main thread is running again. */
      main_state = STATE_RUNNING;

      struct stack_frame *node = lwt_unix_new(struct stack_frame);

      /* Save the stack frame so the old main thread can become a
         worker when the blocking call terminates. */
      if (setjmp(node->checkpoint) == 0) {
        DEBUG("checkpoint for future worker done");

        /* Save the stack frame in the list of worker checkpoints. */
        node->next = become_worker;
        become_worker = node;

        DEBUG("going back to the ocaml code");

        /* Go to before the blocking call. */
        longjmp(blocking_call_leave, CALL_SCHEDULED);
      }

      DEBUG("transformation to worker done");

      /* This thread is not running caml code anymore. */
      //caml_c_thread_unregister();

      /* Release this mutex. It was locked before the jump. */
      lwt_unix_release_mutex(blocking_call_enter_mutex);

    } else {
      DEBUG("taking a job to execute");

      /* Take the first queued job. */
      job = pool_queue->next;

      job->thread = lwt_unix_self();
      job->thread_initialized = 1;

      /* Remove it from the queue. */
      if (job->next == job)
        pool_queue = NULL;
      else
        pool_queue->next = job->next;

      lwt_unix_release_mutex(pool_mutex);

      /* Execute the job. */
      execute_job(job);
    }
  }

  return NULL;
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

void lwt_unix_free_job(lwt_unix_job job)
{
  if (job->async_method != LWT_UNIX_ASYNC_METHOD_NONE)
    lwt_unix_delete_mutex(job->mutex);
  free(job);
}

CAMLprim value lwt_unix_start_job(value val_job, value val_notification_id, value val_async_method)
{
  lwt_unix_job job = Job_val(val_job);

  lwt_unix_async_method async_method = Int_val(val_async_method);

  /* Fallback to synchronous call if there is no worker available and
     we can not launch more threads. */
  if (async_method != LWT_UNIX_ASYNC_METHOD_NONE && thread_waiting_count == 0 && thread_count >= pool_size)
    async_method = LWT_UNIX_ASYNC_METHOD_NONE;

  /* Initialises job parameters. */
  job->done = 0;
  job->fast = 1;
  job->async_method = async_method;
  job->notification_id = Int_val(val_notification_id);

  switch (async_method) {

  case LWT_UNIX_ASYNC_METHOD_NONE:
    /* Execute the job synchronously. */
    caml_enter_blocking_section();
    job->worker(job);
    caml_leave_blocking_section();
    return Val_true;

  case LWT_UNIX_ASYNC_METHOD_DETACH:
    if (threading_initialized == 0) initialize_threading();

    lwt_unix_initialize_mutex(job->mutex);
    job->thread_initialized = 0;

    lwt_unix_acquire_mutex(pool_mutex);
    if (thread_waiting_count == 0) {
      /* Launch a new worker. */
      thread_count++;
      lwt_unix_release_mutex(pool_mutex);
      lwt_unix_launch_thread(worker_loop, (void*)job);
    } else {
      /* Add the job at the end of the queue. */
      if (pool_queue == NULL) {
        pool_queue = job;
        job->next = job;
      } else {
        job->next = pool_queue->next;
        pool_queue->next = job;
        pool_queue = job;
      }
      /* Wakeup one worker. */
      lwt_unix_wake_condition(pool_condition);
      lwt_unix_release_mutex(pool_mutex);
    }
    return Val_bool(job->done);

  case LWT_UNIX_ASYNC_METHOD_SWITCH:
    if (threading_initialized == 0) initialize_threading();

    lwt_unix_initialize_mutex(job->mutex);
    job->thread = main_thread;

    /* Ensures there is at least one thread that can become the main
       thread. */
    if (thread_waiting_count == 0) {
      thread_count++;
      lwt_unix_launch_thread(worker_loop, NULL);
    }

    if (blocking_call_enter == NULL) alloc_new_stack();

    DEBUG("taking a stack frame for doing a blocking call");

    /* Take and remove the first available stack frame for system
       calls. */
    lwt_unix_acquire_mutex(blocking_call_enter_mutex);
    assert(blocking_call_enter != NULL);
    struct stack_frame *node = blocking_call_enter;
    blocking_call_enter = node->next;
    lwt_unix_release_mutex(blocking_call_enter_mutex);

    /* Save the stack frame to leave the blocking call. */
    switch (setjmp(blocking_call_leave)) {
    case 0:
      /* Save the job to do. */
      blocking_call = job;

      /* Save the stack frame that will be used for this call in case
         it get scheduled. */
      blocking_call_frame = node;

      DEBUG("jumping to do a blocking call");

      /* Jump to an alternative stack and do the call. */
      longjmp(node->checkpoint, 1);

    case CALL_SUCCEEDED:
      DEBUG("resuming without being scheduled");

      /* Re-add the stack frame used for the call to the list of
         available ones. */
      lwt_unix_acquire_mutex(blocking_call_enter_mutex);
      node->next = blocking_call_enter;
      blocking_call_enter = node;
      lwt_unix_release_mutex(blocking_call_enter_mutex);
      return Val_true;

    case CALL_SCHEDULED:
      DEBUG("resuming after being scheduled");

      /* This mutex was locked before we did the jump. */
      lwt_unix_release_mutex(pool_mutex);

      /* This thread is now running caml code. */
      //caml_c_thread_register();
      return Val_bool(job->done);
    }
  }

  return Val_false;
}

CAMLprim value lwt_unix_check_job(value val_job)
{
  lwt_unix_job job = Job_val(val_job);

  DEBUG("checking job");

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

    DEBUG("job done: %d", Int_val(result));

    return result;
  }

  return Val_int(0);
}

static void nop() {}

CAMLprim value lwt_unix_cancel_job(value val_job)
{
  struct lwt_unix_job *job = Job_val(val_job);

  DEBUG("cancelling job");

  switch (job->async_method) {

  case LWT_UNIX_ASYNC_METHOD_NONE:
    break;

  case LWT_UNIX_ASYNC_METHOD_DETACH:
    while (job->thread_initialized == 0) {
      struct timeval tv;
      tv.tv_sec = 0;
      tv.tv_usec = 100000;
      select(0, NULL, NULL, NULL, &tv);
    };

  case LWT_UNIX_ASYNC_METHOD_SWITCH:
    lwt_unix_acquire_mutex(job->mutex);
    if (job->done == 0) {
#if defined(LWT_ON_WINDOWS)
      /* TODO: do something here. */
#else
      struct sigaction old_sa, new_sa;

      /* Set up a custom signal handler to be sure that the system
         call will be interrupted. */
      new_sa.sa_handler = nop;
      new_sa.sa_flags = 0;
      sigemptyset(&new_sa.sa_mask);
      sigaction(SIGUSR1, &new_sa, &old_sa);

      /* Interrupt the system call. */
      pthread_kill(job->thread, SIGUSR1);

      /* Restore the old signal handler. */
      sigaction(SIGUSR1, &old_sa, NULL);
#endif
    }
    lwt_unix_release_mutex(job->mutex);
    break;
  }

  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Statistics and control                                          |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_unix_pool_size()
{
  return Val_int(pool_size);
}

CAMLprim value lwt_unix_set_pool_size(value val_size)
{
  pool_size = Int_val(val_size);
  return Val_unit;
}

CAMLprim value lwt_unix_thread_count()
{
  return Val_int(thread_count);
}

CAMLprim value lwt_unix_thread_waiting_count()
{
  return Val_int(thread_waiting_count);
}
