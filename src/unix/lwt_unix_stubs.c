/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#define _GNU_SOURCE
#define _POSIX_PTHREAD_SEMANTICS

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/config.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/domain.h>

#include <assert.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>

#include "lwt_unix.h"

#if !defined(LWT_ON_WINDOWS)
#include <arpa/inet.h>
#include <dirent.h>
#include <fcntl.h>
#include <grp.h>
#include <netdb.h>
#include <netinet/in.h>
#include <pwd.h>
#include <sched.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/un.h>
#include <termios.h>
#include <unistd.h>
#endif

#if defined(HAVE_EVENTFD)
#include <sys/eventfd.h>
#endif

//#define DEBUG_MODE

#if defined(DEBUG_MODE)
#include <sys/syscall.h>
#define DEBUG(fmt, ...)                                               \
  {                                                                   \
    fprintf(stderr, "lwt-debug[%d]: %s: " fmt "\n",                   \
            (pid_t)syscall(SYS_gettid), __FUNCTION__, ##__VA_ARGS__); \
    fflush(stderr);                                                   \
  }
#else
#define DEBUG(fmt, ...)
#endif

/* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ */

void *lwt_unix_malloc(size_t size) {
  void *ptr = malloc(size);
  if (ptr == NULL) {
    perror("cannot allocate memory");
    abort();
  }
  return ptr;
}

void *lwt_unix_realloc(void *ptr, size_t size) {
  void *new_ptr = realloc(ptr, size);
  if (new_ptr == NULL) {
    perror("cannot allocate memory");
    abort();
  }
  return new_ptr;
}

char *lwt_unix_strdup(char *str) {
  char *new_str = strdup(str);
  if (new_str == NULL) {
    perror("cannot allocate memory");
    abort();
  }
  return new_str;
}

void lwt_unix_not_available(char const *feature) {
  caml_raise_with_arg(*caml_named_value("lwt:not-available"),
                      caml_copy_string(feature));
}

/* +-----------------------------------------------------------------+
   | Operation on bigarrays                                          |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_unix_blit(value val_buf1, value val_ofs1, value val_buf2,
                             value val_ofs2, value val_len) {
  memmove((char *)Caml_ba_data_val(val_buf2) + Long_val(val_ofs2),
          (char *)Caml_ba_data_val(val_buf1) + Long_val(val_ofs1),
          Long_val(val_len));
  return Val_unit;
}

CAMLprim value lwt_unix_blit_from_bytes(value val_buf1, value val_ofs1,
                                        value val_buf2, value val_ofs2,
                                        value val_len) {
  memcpy((char *)Caml_ba_data_val(val_buf2) + Long_val(val_ofs2),
         Bytes_val(val_buf1) + Long_val(val_ofs1), Long_val(val_len));
  return Val_unit;
}

CAMLprim value lwt_unix_blit_from_string(value val_buf1, value val_ofs1,
                                        value val_buf2, value val_ofs2,
                                        value val_len) {
  memcpy((char *)Caml_ba_data_val(val_buf2) + Long_val(val_ofs2),
         String_val(val_buf1) + Long_val(val_ofs1), Long_val(val_len));
  return Val_unit;
}


CAMLprim value lwt_unix_blit_to_bytes(value val_buf1, value val_ofs1,
                                      value val_buf2, value val_ofs2,
                                      value val_len) {
  memcpy(Bytes_val(val_buf2) + Long_val(val_ofs2),
         (char *)Caml_ba_data_val(val_buf1) + Long_val(val_ofs1),
         Long_val(val_len));
  return Val_unit;
}

CAMLprim value lwt_unix_fill_bytes(value val_buf, value val_ofs, value val_len,
                                   value val_char) {
  memset((char *)Caml_ba_data_val(val_buf) + Long_val(val_ofs),
         Int_val(val_char), Long_val(val_len));
  return Val_unit;
}

CAMLprim value lwt_unix_mapped(value v_bstr) {
  return Val_bool(Caml_ba_array_val(v_bstr)->flags & CAML_BA_MAPPED_FILE);
}

/* +-----------------------------------------------------------------+
   | Byte order                                                      |
   +-----------------------------------------------------------------+ */

value lwt_unix_system_byte_order() {
#ifdef ARCH_BIG_ENDIAN
  return Val_int(1);
#else
  return Val_int(0);
#endif
}

/* +-----------------------------------------------------------------+
   | Threading                                                       |
   +-----------------------------------------------------------------+ */

#if defined(HAVE_PTHREAD)

int lwt_unix_launch_thread(void *(*start)(void *), void *data) {
  pthread_t thread;
  pthread_attr_t attr;
  sigset_t mask, old_mask;

  pthread_attr_init(&attr);

  /* The thread is created in detached state so we do not have to join
     it when it terminates: */
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

  /* Block all signals, otherwise ocaml handlers defined with the
     module Sys may be executed in the new thread, oops... */
  sigfillset(&mask);
  pthread_sigmask(SIG_SETMASK, &mask, &old_mask);

  int zero_if_created_otherwise_errno =
      pthread_create(&thread, &attr, start, data);

  /* Restore the signal mask for the calling thread. */
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);

  pthread_attr_destroy(&attr);

  return zero_if_created_otherwise_errno;
}

lwt_unix_thread lwt_unix_thread_self() { return pthread_self(); }

int lwt_unix_thread_equal(lwt_unix_thread thread1, lwt_unix_thread thread2) {
  return pthread_equal(thread1, thread2);
}

void lwt_unix_mutex_init(lwt_unix_mutex *mutex) {
  pthread_mutex_init(mutex, NULL);
}

void lwt_unix_mutex_destroy(lwt_unix_mutex *mutex) {
  pthread_mutex_destroy(mutex);
}

void lwt_unix_mutex_lock(lwt_unix_mutex *mutex) { pthread_mutex_lock(mutex); }

void lwt_unix_mutex_unlock(lwt_unix_mutex *mutex) {
  pthread_mutex_unlock(mutex);
}

void lwt_unix_condition_init(lwt_unix_condition *condition) {
  pthread_cond_init(condition, NULL);
}

void lwt_unix_condition_destroy(lwt_unix_condition *condition) {
  pthread_cond_destroy(condition);
}

void lwt_unix_condition_signal(lwt_unix_condition *condition) {
  pthread_cond_signal(condition);
}

void lwt_unix_condition_broadcast(lwt_unix_condition *condition) {
  pthread_cond_broadcast(condition);
}

void lwt_unix_condition_wait(lwt_unix_condition *condition,
                             lwt_unix_mutex *mutex) {
  pthread_cond_wait(condition, mutex);
}

#elif defined(LWT_ON_WINDOWS)
//TODO: windows
#else

#error "no threading library available!"

#endif

/* +-----------------------------------------------------------------+
   | Socketpair on windows                                           |
   +-----------------------------------------------------------------+ */

#if defined(LWT_ON_WINDOWS)
//TODO: windows
#endif

/* +-----------------------------------------------------------------+
   | Notifications                                                   |
   +-----------------------------------------------------------------+ */

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

/* Domain-specific notification state */
struct domain_notification_state {
  lwt_unix_mutex notification_mutex;
  intnat *notifications;
  long notification_count;
  long notification_index;
  enum notification_mode notification_mode;
#if defined(HAVE_EVENTFD)
  int notification_fd;
#endif
  int notification_fds[2];
};

/* table to store per-domain notification state */
#define MAX_DOMAINS 64 // TODO: review values
static struct domain_notification_state domain_states[MAX_DOMAINS];
static int alloced_domain_states[MAX_DOMAINS] = {0};

/* Send one notification. */
static int (*notification_send)(int domain_id);

/* Read one notification. */
static int (*notification_recv)(int domain_id);

static void alloc_domain_notifications(int domain_id) {
    domain_states[domain_id].notification_mode = NOTIFICATION_MODE_NOT_INITIALIZED;
    alloced_domain_states[domain_id] = 1;
}

static void init_domain_notifications(int domain_id) {
    lwt_unix_mutex_init(&domain_states[domain_id].notification_mutex);
    domain_states[domain_id].notification_count = 4096;
    domain_states[domain_id].notifications =
        (intnat *)lwt_unix_malloc(domain_states[domain_id].notification_count * sizeof(intnat));
    domain_states[domain_id].notification_index = 0;
}

static void resize_notifications(int domain_id) {
    struct domain_notification_state *state = &domain_states[domain_id];
    long new_notification_count = state->notification_count * 2;
    intnat *new_notifications =
        (intnat *)lwt_unix_malloc(new_notification_count * sizeof(intnat));
    memcpy((void *)new_notifications, (void *)state->notifications,
           state->notification_count * sizeof(intnat));
    free(state->notifications);
    state->notifications = new_notifications;
    state->notification_count = new_notification_count;
}

void lwt_unix_send_notification(intnat domain_id, intnat id) {
  int ret;
#if !defined(LWT_ON_WINDOWS)
  sigset_t new_mask;
  sigset_t old_mask;
  int error;
  sigfillset(&new_mask);
  pthread_sigmask(SIG_SETMASK, &new_mask, &old_mask);
#else
	//TODO: windows
#endif
  lwt_unix_mutex_lock(&domain_states[domain_id].notification_mutex);
  struct domain_notification_state *state = &domain_states[domain_id];
  if (state->notification_index > 0) {
    /* There is already a pending notification in the buffer, no
       need to signal the main thread. */
    if (state->notification_index == state->notification_count) resize_notifications(domain_id);
    state->notifications[state->notification_index++] = id;
  } else {
    /* There is none, notify the main thread. */
    state->notifications[state->notification_index++] = id;
    ret = notification_send(domain_id);
#if defined(LWT_ON_WINDOWS)
	//TODO: windows
#else
    if (ret < 0) {
      error = errno;
      lwt_unix_mutex_unlock(&domain_states[domain_id].notification_mutex);
      pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
      unix_error(error, "send_notification", Nothing);
    }
#endif
  }
  lwt_unix_mutex_unlock(&domain_states[domain_id].notification_mutex);
#if !defined(LWT_ON_WINDOWS)
	//TODO: windows
#endif
}

value lwt_unix_send_notification_stub(value domain_id, value id) {
  lwt_unix_send_notification(Long_val(domain_id), Long_val(id));
  return Val_unit;
}

value lwt_unix_recv_notifications(intnat domain_id) {
  int ret, i, current_index;
  value result;
#if !defined(LWT_ON_WINDOWS)
  sigset_t new_mask;
  sigset_t old_mask;
  int error;
  sigfillset(&new_mask);
  pthread_sigmask(SIG_SETMASK, &new_mask, &old_mask);
#else
	//TODO: windows
#endif
  /* Initialize domain state if needed */
  lwt_unix_mutex_lock(&domain_states[domain_id].notification_mutex);
  /* Receive the signal. */
  ret = notification_recv(domain_id);
#if defined(LWT_ON_WINDOWS)
	//TODO: windows
#else
  if (ret < 0) {
    error = errno;
    lwt_unix_mutex_unlock(&domain_states[domain_id].notification_mutex);
    pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
    unix_error(error, "recv_notifications", Nothing);
  }
#endif

    struct domain_notification_state *state = &domain_states[domain_id];

    do {
      /*
       release the mutex while calling caml_alloc,
       which may call gc and switch the thread,
       resulting in a classical deadlock,
       when thread in question tries another send
      */
      current_index = state->notification_index;
      lwt_unix_mutex_unlock(&domain_states[domain_id].notification_mutex);
      result = caml_alloc_tuple(current_index);
      lwt_unix_mutex_lock(&domain_states[domain_id].notification_mutex);
      /* check that no new notifications appeared meanwhile (rare) */
    } while (current_index != state->notification_index);

    /* Read all pending notifications. */
    for (i = 0; i < state->notification_index; i++)
      Field(result, i) = Val_long(state->notifications[i]);
    /* Reset the index. */
    state->notification_index = 0;
  lwt_unix_mutex_unlock(&domain_states[domain_id].notification_mutex);
#if !defined(LWT_ON_WINDOWS)
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif
  return result;
}

value lwt_unix_recv_notifications_stub(value domain_id) {
  value res = lwt_unix_recv_notifications(Long_val(domain_id));
  return res;
}

#if defined(LWT_ON_WINDOWS)

//TODO: windows

#else /* defined(LWT_ON_WINDOWS) */

static void set_close_on_exec(int fd) {
  int flags = fcntl(fd, F_GETFD, 0);
  if (flags == -1 || fcntl(fd, F_SETFD, flags | FD_CLOEXEC) == -1)
    uerror("set_close_on_exec", Nothing);
}

#if defined(HAVE_EVENTFD)

static int eventfd_notification_send(int domain_id) {
  uint64_t buf = 1;
  if (domain_id < 0 || domain_id >= MAX_DOMAINS) {
    return -1;
  }
  struct domain_notification_state *state = &domain_states[domain_id];
  int result = write(state->notification_fd, (char *)&buf, 8);
  return result;
}

static int eventfd_notification_recv(int domain_id) {
  uint64_t buf;
  if (domain_id < 0 || domain_id >= MAX_DOMAINS) {
    return -1;
  }
  struct domain_notification_state *state = &domain_states[domain_id];
  int result = read(state->notification_fd, (char *)&buf, 8);
  return result;
}

#endif /* defined(HAVE_EVENTFD) */

static int pipe_notification_send(int domain_id) {
  char buf = 0;
  if (domain_id < 0 || domain_id >= MAX_DOMAINS) {
    return -1;
  }
  struct domain_notification_state *state = &domain_states[domain_id];
  int result = write(state->notification_fds[1], &buf, 1);
  return result;
}

static int pipe_notification_recv(int domain_id) {
  char buf;
  if (domain_id < 0 || domain_id >= MAX_DOMAINS) {
    return -1;
  }
  struct domain_notification_state *state = &domain_states[domain_id];
  int result = read(state->notification_fds[0], &buf, 1);
  return result;
}

value lwt_unix_init_notification(int domain_id) {
  if (domain_id < 0 || domain_id >= MAX_DOMAINS) {
    caml_failwith("invalid domain_id in lwt_unix_init_notification");
  }
  if (alloced_domain_states[domain_id] == 0)
    alloc_domain_notifications(domain_id);
  struct domain_notification_state *state = &domain_states[domain_id];
  switch (state->notification_mode) {
#if defined(HAVE_EVENTFD)
    case NOTIFICATION_MODE_EVENTFD:
      state->notification_mode = NOTIFICATION_MODE_NONE;
      if (close(state->notification_fd) == -1) uerror("close", Nothing);
      break;
#endif
    case NOTIFICATION_MODE_PIPE:
      state->notification_mode = NOTIFICATION_MODE_NONE;
      if (close(state->notification_fds[0]) == -1) uerror("close", Nothing);
      if (close(state->notification_fds[1]) == -1) uerror("close", Nothing);
      break;
    case NOTIFICATION_MODE_NOT_INITIALIZED:
      state->notification_mode = NOTIFICATION_MODE_NONE;
      init_domain_notifications(domain_id);
      break;
    case NOTIFICATION_MODE_NONE:
      break;
    default:
      caml_failwith("notification system in unknown state");
  }

#if defined(HAVE_EVENTFD)
  state->notification_fd = eventfd(0, 0);
  if (state->notification_fd != -1) {
    state->notification_mode = NOTIFICATION_MODE_EVENTFD;
    notification_send = eventfd_notification_send;
    notification_recv = eventfd_notification_recv;
    set_close_on_exec(state->notification_fd);
    return Val_int(state->notification_fd);
  }
#endif

  if (pipe(state->notification_fds) == -1) uerror("pipe", Nothing);
  set_close_on_exec(state->notification_fds[0]);
  set_close_on_exec(state->notification_fds[1]);
  state->notification_mode = NOTIFICATION_MODE_PIPE;
  notification_send = pipe_notification_send;
  notification_recv = pipe_notification_recv;
  return Val_int(state->notification_fds[0]);
}

#endif /* defined(LWT_ON_WINDOWS) */

CAMLprim value lwt_unix_init_notification_stub(value domain_id) {
  value res = lwt_unix_init_notification(Long_val(domain_id));
  return res;
}

/* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ */

#ifndef NSIG
#define NSIG 64
#endif

/* Notifications id for each monitored signal. */
static intnat signal_notifications[NSIG];

CAMLextern int caml_convert_signal_number(int);

/* Send a notification when a signal is received. */
void handle_signal(int signum) {
  if (signum >= 0 && signum < NSIG) {
    intnat id = signal_notifications[signum];
    if (id != -1) {
#if defined(LWT_ON_WINDOWS)
	//TODO: windows
#endif
      //TODO: domain_self instead of root (0)? caml doesn't expose
      //caml_ml_domain_id in domain.h :(
      lwt_unix_send_notification(0, id);
    }
  }
}

CAMLprim value lwt_unix_handle_signal(value val_signum) {
  handle_signal(caml_convert_signal_number(Int_val(val_signum)));
  return Val_unit;
}

#if defined(LWT_ON_WINDOWS)
	//TODO: windows
#endif

/* Install a signal handler. */
CAMLprim value lwt_unix_set_signal(value val_signum, value val_notification, value val_forwarded) {
#if !defined(LWT_ON_WINDOWS)
  struct sigaction sa;
#endif
  int signum = caml_convert_signal_number(Int_val(val_signum));
  intnat notification = Long_val(val_notification);

  if (signum < 0 || signum >= NSIG)
    caml_invalid_argument("Lwt_unix.on_signal: unavailable signal");

  signal_notifications[signum] = notification;

  if (Bool_val(val_forwarded)) return Val_unit;

#if defined(LWT_ON_WINDOWS)
	//TODO: windows
#else
  sa.sa_handler = handle_signal;
#if OCAML_VERSION >= 50000
  sa.sa_flags = SA_ONSTACK;
#else
  sa.sa_flags = 0;
#endif
  sigemptyset(&sa.sa_mask);
  if (sigaction(signum, &sa, NULL) == -1) {
    signal_notifications[signum] = -1;
    uerror("sigaction", Nothing);
  }
#endif
  return Val_unit;
}

/* Remove a signal handler. */
CAMLprim value lwt_unix_remove_signal(value val_signum, value val_forwarded) {
#if !defined(LWT_ON_WINDOWS)
  struct sigaction sa;
#endif
  /* The signal number is valid here since it was when we did the
     set_signal. */
  int signum = caml_convert_signal_number(Int_val(val_signum));
  signal_notifications[signum] = -1;

  if (Bool_val(val_forwarded)) return Val_unit;

#if defined(LWT_ON_WINDOWS)
	//TODO: windows
#else
  sa.sa_handler = SIG_DFL;
  sa.sa_flags = 0;
  sigemptyset(&sa.sa_mask);
  sigaction(signum, &sa, NULL);
#endif
  return Val_unit;
}

/* Mark all signals as non-monitored. */
CAMLprim value lwt_unix_init_signals(value Unit) {
  int i;
  for (i = 0; i < NSIG; i++) signal_notifications[i] = -1;
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Job execution                                                   |
   +-----------------------------------------------------------------+ */

/* Execute the given job. */
void execute_job(lwt_unix_job job) {
  DEBUG("executing the job");

  lwt_unix_mutex_lock(&job->mutex);

  /* Mark the job as running. */
  job->state = LWT_UNIX_JOB_STATE_RUNNING;

  lwt_unix_mutex_unlock(&job->mutex);

  /* Execute the job. */
  job->worker(job);

  DEBUG("job done");

  lwt_unix_mutex_lock(&job->mutex);

  DEBUG("marking the job has done");

  /* Job is done. If the main thread stopped until now, asynchronous
     notification is not necessary. */
  job->state = LWT_UNIX_JOB_STATE_DONE;

  /* Send a notification if the main thread continued its execution
     before the job terminated. */
  if (job->fast == 0) {
    lwt_unix_mutex_unlock(&job->mutex);
    DEBUG("notifying the main thread");
    lwt_unix_send_notification(job->domain_id, job->notification_id);
  } else {
    lwt_unix_mutex_unlock(&job->mutex);
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
   | Threading stuff initialization                                  |
   +-----------------------------------------------------------------+ */

/* Whether threading has been initialized. */
static int threading_initialized = 0;

/* Initialize the pool of thread. */
void initialize_threading() {
  if (threading_initialized == 0) {
    lwt_unix_mutex_init(&pool_mutex);
    lwt_unix_condition_init(&pool_condition);

    threading_initialized = 1;
  }
}

/* +-----------------------------------------------------------------+
   | Worker loop                                                     |
   +-----------------------------------------------------------------+ */

/* Function executed by threads of the pool.
 * Note: all signals are masked for this thread. */
void *worker_loop(void *data) {
  lwt_unix_job job = (lwt_unix_job)data;

  /* Execute the initial job if any. */
  if (job != NULL) execute_job(job);

  while (1) {
    DEBUG("entering waiting section");

    lwt_unix_mutex_lock(&pool_mutex);

    DEBUG("waiting for something to do");

/* Wait for something to do. */
    while (pool_queue == NULL) {
      ++thread_waiting_count;
      lwt_unix_condition_wait(&pool_condition, &pool_mutex);
    }

    DEBUG("received something to do");

      DEBUG("taking a job to execute");

      /* Take the first queued job. */
      job = pool_queue->next;

      /* Remove it from the queue. */
      if (job->next == job)
        pool_queue = NULL;
      else
        pool_queue->next = job->next;

      lwt_unix_mutex_unlock(&pool_mutex);

      /* Execute the job. */
      execute_job(job);
  }

  return NULL;
}

/* +-----------------------------------------------------------------+
   | Jobs                                                            |
   +-----------------------------------------------------------------+ */

/* Description of jobs. */
struct custom_operations job_ops = {
    "lwt.unix.job",      custom_finalize_default,  custom_compare_default,
    custom_hash_default, custom_serialize_default, custom_deserialize_default,
    custom_compare_ext_default,
    NULL
};

/* Get the job structure contained in a custom value. */
#define Job_val(v) *(lwt_unix_job *)Data_custom_val(v)

value lwt_unix_alloc_job(lwt_unix_job job) {
  value val_job = caml_alloc_custom(&job_ops, sizeof(lwt_unix_job), 0, 1);
  Job_val(val_job) = job;
  return val_job;
}

void lwt_unix_free_job(lwt_unix_job job) {
  if (job->async_method != LWT_UNIX_ASYNC_METHOD_NONE)
    lwt_unix_mutex_destroy(&job->mutex);
  free(job);
}

CAMLprim value lwt_unix_start_job(value val_job, value val_async_method) {
  lwt_unix_job job = Job_val(val_job);
  lwt_unix_async_method async_method = Int_val(val_async_method);
  int done = 0;

  /* Fallback to synchronous call if there is no worker available and
     we can not launch more threads. */
  if (async_method != LWT_UNIX_ASYNC_METHOD_NONE && thread_waiting_count == 0 &&
      thread_count >= pool_size)
    async_method = LWT_UNIX_ASYNC_METHOD_NONE;

  /* Initialises job parameters. */
  job->state = LWT_UNIX_JOB_STATE_PENDING;
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
    case LWT_UNIX_ASYNC_METHOD_SWITCH:
      initialize_threading();

      lwt_unix_mutex_init(&job->mutex);

      lwt_unix_mutex_lock(&pool_mutex);
      if (thread_waiting_count == 0) {
        /* Try to start a new worker. */
        int zero_if_started_otherwise_errno =
            lwt_unix_launch_thread(worker_loop, (void *)job);

        /* Increment the worker thread count while still holding the mutex. */
        if (zero_if_started_otherwise_errno == 0)
            ++thread_count;

        lwt_unix_mutex_unlock(&pool_mutex);

        /* If the worker thread was not started, raise an exception. This must
           be done with the mutex unlocked, as it can involve a surprising
           control transfer. */
        if (zero_if_started_otherwise_errno != 0) {
            unix_error(
                zero_if_started_otherwise_errno, "launch_thread", Nothing);
        }
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
        --thread_waiting_count;
        lwt_unix_condition_signal(&pool_condition);
        lwt_unix_mutex_unlock(&pool_mutex);
      }

      done = job->state == LWT_UNIX_JOB_STATE_DONE;
      if (done) {
        /* Wait for the mutex to be released because the job is going to
           be freed immediately. */
        lwt_unix_mutex_lock(&job->mutex);
        lwt_unix_mutex_unlock(&job->mutex);
      }

      return Val_bool(done);
  }

  return Val_false;
}

CAMLprim value lwt_unix_check_job(value val_job, value val_notification_id) {
  lwt_unix_job job = Job_val(val_job);
  value result;

  DEBUG("checking job");

  switch (job->async_method) {
    case LWT_UNIX_ASYNC_METHOD_NONE:
      return Val_int(1);

    case LWT_UNIX_ASYNC_METHOD_DETACH:
    case LWT_UNIX_ASYNC_METHOD_SWITCH:
      lwt_unix_mutex_lock(&job->mutex);
      /* We are not waiting anymore. */
      job->fast = 0;
      /* Set the notification id for asynchronous wakeup. */
      job->notification_id = Long_val(val_notification_id);
      result = Val_bool(job->state == LWT_UNIX_JOB_STATE_DONE);
      lwt_unix_mutex_unlock(&job->mutex);

      DEBUG("job done: %d", Int_val(result));

      return result;
  }

  return Val_int(0);
}

CAMLprim value lwt_unix_self_result(value val_job) {
  lwt_unix_job job = Job_val(val_job);
  return job->result(job);
}

CAMLprim value lwt_unix_run_job_sync(value val_job) {
  lwt_unix_job job = Job_val(val_job);
  /* So lwt_unix_free_job won't try to destroy the mutex. */
  job->async_method = LWT_UNIX_ASYNC_METHOD_NONE;
  caml_enter_blocking_section();
  job->worker(job);
  caml_leave_blocking_section();
  return job->result(job);
}

CAMLprim value lwt_unix_reset_after_fork(value Unit) {
  if (threading_initialized) {
    /* There is no more waiting threads. */
    thread_waiting_count = 0;

    /* There is no more threads. */
    thread_count = 0;

    /* Empty the queue. */
    pool_queue = NULL;

    threading_initialized = 0;
  }

  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Statistics and control                                          |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_unix_pool_size(value Unit) { return Val_int(pool_size); }

CAMLprim value lwt_unix_set_pool_size(value val_size) {
  pool_size = Int_val(val_size);
  return Val_unit;
}

CAMLprim value lwt_unix_thread_count(value Unit) {
  return Val_int(thread_count);
}

CAMLprim value lwt_unix_thread_waiting_count(value Unit) {
  return Val_int(thread_waiting_count);
}
