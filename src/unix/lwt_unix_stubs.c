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

#if defined(_WIN32) || defined(_WIN64)
#  include <winsock2.h>
#  include <windows.h>
#endif

#define _GNU_SOURCE
#define _POSIX_PTHREAD_SEMANTICS

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/config.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/callback.h>

#include <assert.h>
#include <stdio.h>

#include "lwt_config.h"
#include "lwt_unix.h"

#if !defined(LWT_ON_WINDOWS)
#  include <unistd.h>
#  include <sys/socket.h>
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <sys/param.h>
#  include <sys/un.h>
#  include <sys/mman.h>
#  include <signal.h>
#  include <errno.h>
#  include <string.h>
#  include <fcntl.h>
#  include <dirent.h>
#  include <pwd.h>
#  include <grp.h>
#  include <netdb.h>
#  include <termios.h>
#  include <setjmp.h>
#  include <sched.h>
#  include <signal.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#endif

#if defined(HAVE_EVENTFD)
#  include <sys/eventfd.h>
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

void lwt_unix_not_available(char const *feature)
{
  caml_raise_with_arg(*caml_named_value("lwt:not-available"), caml_copy_string(feature));
}

/* +-----------------------------------------------------------------+
   | Operation on bigarrays                                          |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_unix_blit_bytes_bytes(value val_buf1, value val_ofs1, value val_buf2, value val_ofs2, value val_len)
{
  memcpy((char*)Caml_ba_data_val(val_buf2) + Long_val(val_ofs2),
         (char*)Caml_ba_data_val(val_buf1) + Long_val(val_ofs1),
         Long_val(val_len));
  return Val_unit;
}

CAMLprim value lwt_unix_blit_string_bytes(value val_buf1, value val_ofs1, value val_buf2, value val_ofs2, value val_len)
{
  memcpy((char*)Caml_ba_data_val(val_buf2) + Long_val(val_ofs2),
         String_val(val_buf1) + Long_val(val_ofs1),
         Long_val(val_len));
  return Val_unit;
}

CAMLprim value lwt_unix_blit_bytes_string(value val_buf1, value val_ofs1, value val_buf2, value val_ofs2, value val_len)
{
  memcpy(String_val(val_buf2) + Long_val(val_ofs2),
         (char*)Caml_ba_data_val(val_buf1) + Long_val(val_ofs1),
         Long_val(val_len));
  return Val_unit;
}

CAMLprim value lwt_unix_fill_bytes(value val_buf, value val_ofs, value val_len, value val_char)
{
  memset((char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs), Int_val(val_char), Long_val(val_len));
  return Val_unit;
}

CAMLprim value lwt_unix_mapped(value v_bstr)
{
  return Val_bool(Caml_ba_array_val(v_bstr)->flags & CAML_BA_MAPPED_FILE);
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

/* The mutex used to send and receive notifications. */
static pthread_mutex_t notification_mutex;

/* All pending notifications. */
static long *notifications = NULL;

/* The size of the notification buffer. */
static long notification_count = 0;

/* The index to the next available cell in the notification buffer. */
static long notification_index = 0;

/* The mode currently used for notifications. */
enum notification_mode {
  /* Not yet initialized. */
  NOTIFICATION_MODE_NOT_INITIALIZED,

  /* Initialized but no mode defined. */
  NOTIFICATION_MODE_NONE,

  /* Using an eventfd. */
  NOTIFICATION_MODE_EVENTFD,

  /* Using a pipe. */
  NOTIFICATION_MODE_PIPE,

  /* Using a pair of sockets (only on windows). */
  NOTIFICATION_MODE_WINDOWS
};

/* The current notification mode. */
static enum notification_mode notification_mode = NOTIFICATION_MODE_NOT_INITIALIZED;

/* Send one notification. */
static int (*notification_send)();

/* Read one notification. */
static int (*notification_recv)();

static void init_notifications()
{
  pthread_mutex_init(&notification_mutex, NULL);
  notification_count = 4096;
  notifications = (long*)lwt_unix_malloc(notification_count * sizeof(long));
}

static void resize_notifications()
{
  long new_notification_count = notification_count * 2;
  long *new_notifications = (long*)lwt_unix_malloc(new_notification_count * sizeof(long));
  memcpy((void*)new_notifications, (void*)notifications, notification_count * sizeof(long));
  free(notifications);
  notifications = new_notifications;
  notification_count = new_notification_count;
}

void lwt_unix_send_notification(int id)
{
  pthread_mutex_lock(&notification_mutex);
  if (notification_index > 0) {
    /* There is already a pending notification in the buffer, no
       need to signal the main thread. */
    if (notification_index == notification_count) resize_notifications();
    notifications[notification_index++] = id;
  } else {
    /* There is none, notify the main thread. */
    notifications[notification_index++] = id;
    for (;;) {
      int ret = notification_send();
      if (ret < 0) {
        if (errno != EINTR) {
          pthread_mutex_unlock(&notification_mutex);
          uerror("send_notification", Nothing);
        }
      } else
        break;
    }
  }
  pthread_mutex_unlock(&notification_mutex);
}

value lwt_unix_send_notification_stub(value id)
{
  lwt_unix_send_notification(Long_val(id));
  return Val_unit;
}

value lwt_unix_recv_notifications()
{
  pthread_mutex_lock(&notification_mutex);
  /* Receive the signal. */
  for (;;) {
    int ret = notification_recv();
    if (ret < 0) {
      if (errno != EINTR) {
        pthread_mutex_unlock(&notification_mutex);
        uerror("recv_notifications", Nothing);
      }
    } else
      break;
  }
  /* Read all pending notifications. */
  value result = caml_alloc_tuple(notification_index);
  int i;
  for (i = 0; i < notification_index; i++)
    Field(result, i) = Val_long(notifications[i]);
  /* Reset the index. */
  notification_index = 0;
  pthread_mutex_unlock(&notification_mutex);
  return result;
}

#if defined(LWT_ON_WINDOWS)

static SOCKET set_close_on_exec(SOCKET socket)
{
  SOCKET new_socket;
  if (!DuplicateHandle(GetCurrentProcess(), (HANDLE)socket,
                       GetCurrentProcess(), (HANDLE*)&new_socket,
                       0L, FALSE, DUPLICATE_SAME_ACCESS)) {
    win32_maperr(GetLastError());
    uerror("set_close_on_exec", Nothing);
  }
  closesocket(socket);
  return new_socket;
}

static SOCKET socket_r, socket_w;

static int windows_notification_send()
{
  char buf;
  return send(socket_w, &buf, 1, 0);
}

static int windows_notification_recv()
{
  char buf;
  return recv(socket_r, &buf, 1, 0);
}

value lwt_unix_init_notification()
{
  switch (notification_mode) {
  case NOTIFICATION_MODE_NOT_INITIALIZED:
    notification_mode = NOTIFICATION_MODE_NONE;
    init_notifications();
    break;
  case NOTIFICATION_MODE_WINDOWS:
    notification_mode = NOTIFICATION_MODE_NONE;
    closesocket(socket_r);
    closesocket(socket_w);
    break;
  case NOTIFICATION_MODE_NONE:
    break;
  default:
    caml_failwith("notification system in unknown state");
  }

  /* Since pipes do not works with select, we need to use a pair of
     sockets. The following code simulate the socketpair call of
     unix. */

  union {
    struct sockaddr_in inaddr;
    struct sockaddr addr;
  } a;
  SOCKET listener;
  int e;
  int addrlen = sizeof(a.inaddr);
  int reuse = 1;
  DWORD err;

  socket_r = INVALID_SOCKET;
  socket_w = INVALID_SOCKET;

  listener = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (listener == INVALID_SOCKET)
    goto failure;

  memset(&a, 0, sizeof(a));
  a.inaddr.sin_family = AF_INET;
  a.inaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  a.inaddr.sin_port = 0;

  if (setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, (char*) &reuse, sizeof(reuse)) == -1)
    goto failure;

  if  (bind(listener, &a.addr, sizeof(a.inaddr)) == SOCKET_ERROR)
    goto failure;

  memset(&a, 0, sizeof(a));
  if  (getsockname(listener, &a.addr, &addrlen) == SOCKET_ERROR)
    goto failure;

  a.inaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  a.inaddr.sin_family = AF_INET;

  if (listen(listener, 1) == SOCKET_ERROR)
    goto failure;

  socket_r = WSASocket(AF_INET, SOCK_STREAM, 0, NULL, 0, WSA_FLAG_OVERLAPPED);
  if (socket_r == INVALID_SOCKET)
    goto failure;

  if (connect(socket_r, &a.addr, sizeof(a.inaddr)) == SOCKET_ERROR)
    goto failure;

  socket_w = accept(listener, NULL, NULL);
  if (socket_w == INVALID_SOCKET)
    goto failure;

  closesocket(listener);

  socket_r = set_close_on_exec(socket_r);
  socket_w = set_close_on_exec(socket_w);
  notification_mode = NOTIFICATION_MODE_WINDOWS;
  notification_send = windows_notification_send;
  notification_recv = windows_notification_recv;
  return win_alloc_socket(socket_r);

 failure:
  err = WSAGetLastError();
  closesocket(listener);
  closesocket(socket_r);
  closesocket(socket_w);
  win32_maperr(err);
  uerror("init_notification", Nothing);
  /* Just to make the compiler happy. */
  return Val_unit;
}

#else /* defined(LWT_ON_WINDOWS) */

static void set_close_on_exec(int fd)
{
  int flags = fcntl(fd, F_GETFD, 0);
  if (flags == -1 || fcntl(fd, F_SETFD, flags | FD_CLOEXEC) == -1)
    uerror("set_close_on_exec", Nothing);
}

#if defined(HAVE_EVENTFD)

static int notification_fd;

static int eventfd_notification_send()
{
  uint64_t buf = 1;
  return write(notification_fd, (char*)&buf, 8);
}

static int eventfd_notification_recv()
{
  uint64_t buf;
  return read(notification_fd, (char*)&buf, 8);
}

#endif /* defined(HAVE_EVENTFD) */

static int notification_fds[2];

static int pipe_notification_send()
{
  char buf;
  return write(notification_fds[1], &buf, 1);
}

static int pipe_notification_recv()
{
  char buf;
  return read(notification_fds[0], &buf, 1);
}

value lwt_unix_init_notification()
{
  switch (notification_mode) {
#if defined(HAVE_EVENTFD)
  case NOTIFICATION_MODE_EVENTFD:
    notification_mode = NOTIFICATION_MODE_NONE;
    if (close(notification_fd) == -1) uerror("close", Nothing);
    break;
#endif
  case NOTIFICATION_MODE_PIPE:
    notification_mode = NOTIFICATION_MODE_NONE;
    if (close(notification_fds[0]) == -1) uerror("close", Nothing);
    if (close(notification_fds[1]) == -1) uerror("close", Nothing);
    break;
  case NOTIFICATION_MODE_NOT_INITIALIZED:
    notification_mode = NOTIFICATION_MODE_NONE;
    init_notifications();
    break;
  case NOTIFICATION_MODE_NONE:
    break;
  default:
    caml_failwith("notification system in unknown state");
  }

#if defined(HAVE_EVENTFD)
  notification_fd = eventfd(0, 0);
  if (notification_fd != -1) {
    notification_mode = NOTIFICATION_MODE_EVENTFD;
    notification_send = eventfd_notification_send;
    notification_recv = eventfd_notification_recv;
    set_close_on_exec(notification_fd);
    return Val_int(notification_fd);
  }
#endif

  if (pipe(notification_fds) == -1) uerror("pipe", Nothing);
  set_close_on_exec(notification_fds[0]);
  set_close_on_exec(notification_fds[1]);
  notification_mode = NOTIFICATION_MODE_PIPE;
  notification_send = pipe_notification_send;
  notification_recv = pipe_notification_recv;
  return Val_int(notification_fds[0]);
}

#endif /* defined(LWT_ON_WINDOWS) */

/* +-----------------------------------------------------------------+
   | Launching a thread                                              |
   +-----------------------------------------------------------------+ */

pthread_t lwt_unix_launch_thread(void* (*start)(void*), void* data)
{
  pthread_t thread;
  pthread_attr_t attr;

  pthread_attr_init(&attr);

  /* The thread is created in detached state so we do not have to join
     it when it terminates: */
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

  int result = pthread_create(&thread, &attr, start, data);

  if (result) unix_error(result, "launch_thread", Nothing);

  pthread_attr_destroy (&attr);

  return thread;
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

  pthread_mutex_lock(&job->mutex);

  DEBUG("marking the job has done");

  /* Job is done. If the main thread stopped until now, asynchronous
     notification is not necessary. */
  job->done = 1;

  /* Send a notification if the main thread continued its execution
     before the job terminated. */
  if (job->fast == 0) {
    pthread_mutex_unlock(&job->mutex);
    DEBUG("notifying the main thread");
    lwt_unix_send_notification(job->notification_id);
  } else {
    pthread_mutex_unlock(&job->mutex);
    DEBUG("not notifying the main thread");
  }
}

/* +-----------------------------------------------------------------+
   | Config                                                          |
   +-----------------------------------------------------------------+ */

/* The signal used to allocate new stack. */
static int signal_alloc_stack = -1;

/* The signal used to kill a thread. */
static int signal_kill_thread = -1;

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
static pthread_cond_t pool_condition;

/* Queue of pending jobs. It points to the last enqueued job.  */
static lwt_unix_job pool_queue = NULL;

/* The mutex which protect access to [pool_queue], [pool_condition]
   and [thread_waiting_count]. */
static pthread_mutex_t pool_mutex;

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
static pthread_t main_thread;

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
static pthread_mutex_t blocking_call_enter_mutex;

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

/* Flag which become [1] once the stack has been allocated. */
static int stack_allocated;

/* Function executed on an alternative stack. */
static void altstack_worker()
{
  if (stack_allocated == 1) return;
  stack_allocated = 1;

  /* The first passage is to register a new stack frame. */
  struct stack_frame *node = lwt_unix_new(struct stack_frame);

  if (setjmp(node->checkpoint) == 0) {

    /* Add it to the list of available stack frames. */
    pthread_mutex_lock(&blocking_call_enter_mutex);
    node->next = blocking_call_enter;
    blocking_call_enter = node;
    pthread_mutex_unlock(&blocking_call_enter_mutex);

  } else {

    /* Save the job to execute and the current stack frame before
       another thread can become the main thread. */
    lwt_unix_job job = blocking_call;
    struct stack_frame *frame = blocking_call_frame;

    /* Mark the main thread as blocked. */
    main_state = STATE_BLOCKED;

    DEBUG("signaling the pool condition variable");

    /* Maybe wakeup a worker so it can become the main thread. */
    pthread_mutex_lock(&pool_mutex);
    pthread_cond_signal(&pool_condition);
    pthread_mutex_unlock(&pool_mutex);

    DEBUG("executing the blocking call");

    /* Execute the blocking call. */
    execute_job(job);

    DEBUG("blocking call done");

    pthread_mutex_lock(&pool_mutex);

    if (pthread_equal(main_thread, pthread_self())) {
      /* We stayed the main thread, continue the execution
         normally. */
      main_state = STATE_RUNNING;

      pthread_mutex_unlock(&pool_mutex);

      DEBUG("blocing call terminated without blocking, resuming");

      /* Leave the blocking call. */
      longjmp(blocking_call_leave, CALL_SUCCEEDED);
    } else {
      /* We did not stayed the main thread, we now become a worker. */

      assert(become_worker != NULL);

      /* Take and remove the first worker checkpoint. */
      struct stack_frame *node = become_worker;
      become_worker = node->next;

      pthread_mutex_unlock(&pool_mutex);

      DEBUG("blocking call terminated after blocking, becoming a worker");

      /* Add the stack frame used for this call to the list of
         available ones. */
      pthread_mutex_lock(&blocking_call_enter_mutex);
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
#if !defined(LWT_ON_WINDOWS)
  DEBUG("allocate a new stack");

  stack_t old_stack, new_stack;
  struct sigaction old_sa, new_sa;

  /* Create the new stack. */
  new_stack.ss_flags = 0;
  new_stack.ss_size = STACK_SIZE;
  new_stack.ss_sp = lwt_unix_malloc(STACK_SIZE);

  /* Change the stack used for signals. */
  sigaltstack(&new_stack, &old_stack);

  stack_allocated = 0;

  /* Set up the custom signal handler. */
  new_sa.sa_handler = altstack_worker;
  new_sa.sa_flags = SA_ONSTACK;
  sigemptyset(&new_sa.sa_mask);
  sigaction(signal_alloc_stack, &new_sa, &old_sa);

  /* Save the stack frame. */
  raise(signal_alloc_stack);

  /* Restore the old signal handler. */
  sigaction(signal_alloc_stack, &old_sa, NULL);

  /* Restore the old alternative stack. */
  sigaltstack(&old_stack, NULL);
#endif
}

/* +-----------------------------------------------------------------+
   | Threading stuff initialization                                  |
   +-----------------------------------------------------------------+ */

/* Whether threading has been initialized. */
static int threading_initialized = 0;

static void nop() {}

#if !defined(SIGRTMIN) || !defined(SIGRTMAX)
#  define SIGRTMIN 0
#  define SIGRTMAX 0
#endif

/* Initialize the pool of thread. */
void initialize_threading()
{
  if (threading_initialized == 0) {
    pthread_mutex_init(&pool_mutex, NULL);
    pthread_cond_init(&pool_condition, NULL);
    pthread_mutex_init(&blocking_call_enter_mutex, NULL);

    main_thread = pthread_self();

#if !defined(LWT_ON_WINDOWS)
    if (SIGRTMIN < SIGRTMAX) {
      signal_alloc_stack = SIGRTMIN;
      if (SIGRTMIN + 1 < SIGRTMAX)
        signal_kill_thread = SIGRTMIN + 1;
      else
        signal_kill_thread = SIGUSR1;
    } else {
      signal_alloc_stack = SIGUSR1;
      signal_kill_thread = SIGUSR2;
    }

    /* Define a handler for the signal used for killing threads to be
       sure system calls get interrupted. */
    struct sigaction sa;
    sa.sa_handler = nop;
    sa.sa_flags = 0;
    sigemptyset(&sa.sa_mask);
    sigaction(signal_kill_thread, &sa, NULL);
#endif

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

    pthread_mutex_lock(&pool_mutex);

    /* One more thread is waiting for work. */
    thread_waiting_count++;

    DEBUG("waiting for something to do");

    /* Wait for something to do. */
    while (pool_queue == NULL && main_state == STATE_RUNNING)
      pthread_cond_wait(&pool_condition, &pool_mutex);

    DEBUG("received something to do");

    /* This thread is busy. */
    thread_waiting_count--;

    if (main_state == STATE_BLOCKED) {
      DEBUG("main thread is blocked");
      DEBUG("\e[1;31mswitching\e[0m");

      /* If the main thread is blocked, we become the main thread. */
      main_thread = pthread_self();

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
      pthread_mutex_unlock(&blocking_call_enter_mutex);

    } else {
      DEBUG("taking a job to execute");

      /* Take the first queued job. */
      job = pool_queue->next;

      job->thread = pthread_self();
      job->thread_initialized = 1;

      /* Remove it from the queue. */
      if (job->next == job)
        pool_queue = NULL;
      else
        pool_queue->next = job->next;

      pthread_mutex_unlock(&pool_mutex);

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
    pthread_mutex_destroy(&job->mutex);
  free(job);
}

CAMLprim value lwt_unix_start_job(value val_job, value val_async_method)
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

  switch (async_method) {

  case LWT_UNIX_ASYNC_METHOD_NONE:
    /* Execute the job synchronously. */
    caml_enter_blocking_section();
    job->worker(job);
    caml_leave_blocking_section();
    return Val_true;

  case LWT_UNIX_ASYNC_METHOD_DETACH:
    if (threading_initialized == 0) initialize_threading();

    pthread_mutex_init(&job->mutex, NULL);
    job->thread_initialized = 0;

    pthread_mutex_lock(&pool_mutex);
    if (thread_waiting_count == 0) {
      /* Launch a new worker. */
      thread_count++;
      pthread_mutex_unlock(&pool_mutex);
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
      pthread_cond_signal(&pool_condition);
      pthread_mutex_unlock(&pool_mutex);
    }
    return Val_bool(job->done);

  case LWT_UNIX_ASYNC_METHOD_SWITCH:
#if defined(LWT_ON_WINDOWS)
    caml_invalid_argument("the switch method is not implemented on windows");
#endif

    if (threading_initialized == 0) initialize_threading();

    pthread_mutex_init(&job->mutex, NULL);
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
    pthread_mutex_lock(&blocking_call_enter_mutex);
    assert(blocking_call_enter != NULL);
    struct stack_frame *node = blocking_call_enter;
    blocking_call_enter = node->next;
    pthread_mutex_unlock(&blocking_call_enter_mutex);

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
      pthread_mutex_lock(&blocking_call_enter_mutex);
      node->next = blocking_call_enter;
      blocking_call_enter = node;
      pthread_mutex_unlock(&blocking_call_enter_mutex);
      return Val_true;

    case CALL_SCHEDULED:
      DEBUG("resuming after being scheduled");

      /* This mutex was locked before we did the jump. */
      pthread_mutex_unlock(&pool_mutex);

      /* This thread is now running caml code. */
      //caml_c_thread_register();
      return Val_bool(job->done);
    }
  }

  return Val_false;
}

CAMLprim value lwt_unix_check_job(value val_job, value val_notification_id)
{
  lwt_unix_job job = Job_val(val_job);

  DEBUG("checking job");

  switch (job->async_method) {

  case LWT_UNIX_ASYNC_METHOD_NONE:
    return Val_int(1);

  case LWT_UNIX_ASYNC_METHOD_DETACH:
  case LWT_UNIX_ASYNC_METHOD_SWITCH:
    pthread_mutex_lock(&job->mutex);
    /* We are not waiting anymore. */
    job->fast = 0;
    /* Set the notification id for asynchronous wakeup. */
    job->notification_id = Int_val(val_notification_id);
    value result = Val_bool(job->done);
    pthread_mutex_unlock(&job->mutex);

    DEBUG("job done: %d", Int_val(result));

    return result;
  }

  return Val_int(0);
}

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
    pthread_mutex_lock(&job->mutex);
    if (job->done == 0 && signal_kill_thread >= 0)
      pthread_kill(job->thread, signal_kill_thread);
    pthread_mutex_unlock(&job->mutex);
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
