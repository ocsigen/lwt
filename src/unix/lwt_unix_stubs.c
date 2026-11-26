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

value lwt_unix_system_byte_order(void) {
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

lwt_unix_thread lwt_unix_thread_self(void) { return pthread_self(); }

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

int lwt_unix_launch_thread(void *(*start)(void *), void *data) {
  HANDLE handle =
      CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)start, data, 0, NULL);
  if (handle) {
    CloseHandle(handle);
    return 0;
  }
  win32_maperr(GetLastError());
  return errno;
}

lwt_unix_thread lwt_unix_thread_self(void) { return GetCurrentThreadId(); }

int lwt_unix_thread_equal(lwt_unix_thread thread1, lwt_unix_thread thread2) {
  return thread1 == thread2;
}

void lwt_unix_mutex_init(lwt_unix_mutex *mutex) {
  InitializeCriticalSection(mutex);
}

void lwt_unix_mutex_destroy(lwt_unix_mutex *mutex) {
  DeleteCriticalSection(mutex);
}

void lwt_unix_mutex_lock(lwt_unix_mutex *mutex) { EnterCriticalSection(mutex); }

void lwt_unix_mutex_unlock(lwt_unix_mutex *mutex) {
  LeaveCriticalSection(mutex);
}

struct wait_list {
  HANDLE event;
  struct wait_list *next;
};

struct lwt_unix_condition {
  CRITICAL_SECTION mutex;
  struct wait_list *waiters;
};

void lwt_unix_condition_init(lwt_unix_condition *condition) {
  InitializeCriticalSection(&condition->mutex);
  condition->waiters = NULL;
}

void lwt_unix_condition_destroy(lwt_unix_condition *condition) {
  DeleteCriticalSection(&condition->mutex);
}

void lwt_unix_condition_signal(lwt_unix_condition *condition) {
  struct wait_list *node;
  EnterCriticalSection(&condition->mutex);

  node = condition->waiters;
  if (node) {
    condition->waiters = node->next;
    SetEvent(node->event);
  }
  LeaveCriticalSection(&condition->mutex);
}

void lwt_unix_condition_broadcast(lwt_unix_condition *condition) {
  struct wait_list *node;
  EnterCriticalSection(&condition->mutex);
  for (node = condition->waiters; node; node = node->next)
    SetEvent(node->event);
  condition->waiters = NULL;
  LeaveCriticalSection(&condition->mutex);
}

void lwt_unix_condition_wait(lwt_unix_condition *condition,
                             lwt_unix_mutex *mutex) {
  struct wait_list node;

  /* Create the event for the notification. */
  node.event = CreateEvent(NULL, FALSE, FALSE, NULL);

  /* Add the node to the condition. */
  EnterCriticalSection(&condition->mutex);
  node.next = condition->waiters;
  condition->waiters = &node;
  LeaveCriticalSection(&condition->mutex);

  /* Release the mutex. */
  LeaveCriticalSection(mutex);

  /* Wait for a signal. */
  WaitForSingleObject(node.event, INFINITE);

  /* The event is no more used. */
  CloseHandle(node.event);

  /* Re-acquire the mutex. */
  EnterCriticalSection(mutex);
}

#else

#error "no threading library available!"

#endif

/* +-----------------------------------------------------------------+
   | Socketpair on windows                                           |
   +-----------------------------------------------------------------+ */

#if defined(LWT_ON_WINDOWS)

#if OCAML_VERSION < 41400
static int win_set_inherit(HANDLE fd, BOOL inherit)
{
  if (! SetHandleInformation(fd,
                             HANDLE_FLAG_INHERIT,
                             inherit ? HANDLE_FLAG_INHERIT : 0)) {
    win32_maperr(GetLastError());
    return -1;
  }
  return 0;
}
#endif

static SOCKET lwt_win_socket(int domain, int type, int protocol,
                             LPWSAPROTOCOL_INFO info,
                             BOOL inherit)
{
  SOCKET s;
  DWORD flags = WSA_FLAG_OVERLAPPED;

#ifndef WSA_FLAG_NO_HANDLE_INHERIT
#define WSA_FLAG_NO_HANDLE_INHERIT 0x80
#endif

  if (! inherit)
    flags |= WSA_FLAG_NO_HANDLE_INHERIT;

  s = WSASocket(domain, type, protocol, info, 0, flags);
  if (s == INVALID_SOCKET) {
    if (! inherit && WSAGetLastError() == WSAEINVAL) {
      /* WSASocket probably doesn't suport WSA_FLAG_NO_HANDLE_INHERIT,
       * retry without. */
      flags &= ~(DWORD)WSA_FLAG_NO_HANDLE_INHERIT;
      s = WSASocket(domain, type, protocol, info, 0, flags);
      if (s == INVALID_SOCKET)
        goto err;
      win_set_inherit((HANDLE) s, FALSE);
      return s;
    }
    goto err;
  }

  return s;

 err:
  win32_maperr(WSAGetLastError());
  return INVALID_SOCKET;
}

static void lwt_unix_socketpair(int domain, int type, int protocol,
                                SOCKET sockets[2], BOOL inherit) {
  union {
    struct sockaddr_in inaddr;
    struct sockaddr_in6 inaddr6;
    struct sockaddr addr;
  } a;
  SOCKET listener;
  int addrlen;
  int reuse = 1;
  DWORD err;

  if (domain != PF_INET && domain != PF_INET6)
    unix_error(ENOPROTOOPT, "socketpair", Nothing);

  sockets[0] = INVALID_SOCKET;
  sockets[1] = INVALID_SOCKET;

  listener = lwt_win_socket(domain, type, protocol, NULL, inherit);
  if (listener == INVALID_SOCKET) goto failure;

  memset(&a, 0, sizeof(a));
  if (domain == PF_INET) {
    a.inaddr.sin_family = domain;
    a.inaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    a.inaddr.sin_port = 0;
  } else {
    a.inaddr6.sin6_family = domain;
    a.inaddr6.sin6_addr = in6addr_loopback;
    a.inaddr6.sin6_port = 0;
  }

  if (setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, (char *)&reuse,
                 sizeof(reuse)) == -1)
    goto failure;

  addrlen = domain == PF_INET ? sizeof(a.inaddr) : sizeof(a.inaddr6);
  if (bind(listener, &a.addr, addrlen) == SOCKET_ERROR) goto failure;

  memset(&a, 0, sizeof(a));
  if (getsockname(listener, &a.addr, &addrlen) == SOCKET_ERROR) goto failure;

  if (domain == PF_INET) {
    a.inaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    a.inaddr.sin_family = AF_INET;
  } else {
    a.inaddr6.sin6_addr = in6addr_loopback;
    a.inaddr6.sin6_family = AF_INET6;
  }

  if (listen(listener, 1) == SOCKET_ERROR) goto failure;

  sockets[0] = lwt_win_socket(domain, type, protocol, NULL, inherit);
  if (sockets[0] == INVALID_SOCKET) goto failure;

  addrlen = domain == PF_INET ? sizeof(a.inaddr) : sizeof(a.inaddr6);
  if (connect(sockets[0], &a.addr, addrlen) == SOCKET_ERROR)
    goto failure;

  sockets[1] = accept(listener, NULL, NULL);
  if (sockets[1] == INVALID_SOCKET) goto failure;

  closesocket(listener);
  return;

failure:
  err = WSAGetLastError();
  closesocket(listener);
  closesocket(sockets[0]);
  closesocket(sockets[1]);
  win32_maperr(err);
  uerror("socketpair", Nothing);
}

static const int socket_domain_table[] =
  {PF_UNIX, PF_INET, PF_INET6};

static const int socket_type_table[] =
  {SOCK_STREAM, SOCK_DGRAM, SOCK_RAW, SOCK_SEQPACKET};

CAMLprim value lwt_unix_socketpair_stub(value cloexec, value domain, value type,
                                        value protocol) {
  CAMLparam4(cloexec, domain, type, protocol);
  CAMLlocal1(result);
  SOCKET sockets[2];
  lwt_unix_socketpair(socket_domain_table[Int_val(domain)],
                      socket_type_table[Int_val(type)], Int_val(protocol),
                      sockets,
                      ! unix_cloexec_p(cloexec));
  result = caml_alloc_tuple(2);
  Store_field(result, 0, win_alloc_socket(sockets[0]));
  Store_field(result, 1, win_alloc_socket(sockets[1]));
  CAMLreturn(result);
}

#endif

/* +-----------------------------------------------------------------+
   | Notifications                                                   |
   +-----------------------------------------------------------------+ */

/* +-----------------------------------------------------------------+
   | Notification channels                                           |
   +-----------------------------------------------------------------+ */

/* One channel per Lwt loop, holding the mode, the descriptors, the send and
   receive functions, the mutex and the buffer.

   The notification id encodes the channel's index, since notifications are
   sent from pool threads without the runtime lock and from signal handlers,
   which cannot tell which domain they are on. The table has a fixed size: a
   new loop takes the first free slot, so the slots of retired channels are
   reused. A generation goes with the index, so that an id of a loop that has
   gone is dropped. */

/* Must fit in LWT_NOTIFICATION_INDEX_BITS below. */
#define LWT_NOTIFICATION_CHANNELS 256

/* The layout of an id, derived from the word size. An OCaml int has one bit
   fewer than the word. Index and generation have 8 bits each, and the local
   counter takes the rest: 47 bits on a 64-bit platform, 15 on a 32-bit one,
   which still allows 32768 notifications alive at once on each loop. Beyond
   that, make_notification fails. */
#define LWT_NOTIFICATION_ID_BITS ((int)(sizeof(intnat) * 8 - 1))
#define LWT_NOTIFICATION_INDEX_BITS 8
#define LWT_NOTIFICATION_GEN_BITS 8
#define LWT_NOTIFICATION_LOCAL_BITS                                     \
  (LWT_NOTIFICATION_ID_BITS - LWT_NOTIFICATION_INDEX_BITS -             \
   LWT_NOTIFICATION_GEN_BITS)
#define LWT_NOTIFICATION_LOCAL_MASK ((((intnat)1) << LWT_NOTIFICATION_LOCAL_BITS) - 1)
#define LWT_NOTIFICATION_GEN_MASK ((((intnat)1) << LWT_NOTIFICATION_GEN_BITS) - 1)
#define LWT_NOTIFICATION_INDEX(id) \
  ((int)(((id) >> (LWT_NOTIFICATION_LOCAL_BITS + LWT_NOTIFICATION_GEN_BITS)) & \
         (LWT_NOTIFICATION_CHANNELS - 1)))
#define LWT_NOTIFICATION_GEN(id) \
  ((unsigned)(((id) >> LWT_NOTIFICATION_LOCAL_BITS) & LWT_NOTIFICATION_GEN_MASK))

#ifndef NSIG
#define NSIG 64
#endif

/* The subscriptions to signals, 0 where there is none; the signal code further
   down explains them. */
static intnat signal_notifications[NSIG][LWT_NOTIFICATION_CHANNELS];

/* The mode currently used for notifications. */
enum notification_mode {
  /* Initialized but no mode defined. */
  NOTIFICATION_MODE_NONE,

  /* Using an eventfd. */
  NOTIFICATION_MODE_EVENTFD,

  /* Using a pipe. */
  NOTIFICATION_MODE_PIPE,

  /* Using a pair of sockets (only on windows). */
  NOTIFICATION_MODE_WINDOWS
};

struct notification_channel {
  /* Protects the buffer and its index. Taken by senders, including a signal
     handler, which is why every sender masks signals around it. */
  lwt_unix_mutex mutex;

  /* Pending notification ids, and the next free cell. */
  intnat *notifications;
  long count;
  long index;

  enum notification_mode mode;
  int (*send)(struct notification_channel *chan);
  int (*recv)(struct notification_channel *chan);

#if defined(LWT_ON_WINDOWS)
  SOCKET socket_r, socket_w;
#else
  int fd;      /* eventfd */
  int fds[2];  /* pipe */
#endif

  /* Bumped each time this slot is reused, so a stale id is recognizable. */
  unsigned generation;

  /* 0 when the slot is free. */
  int in_use;

  /* Signals that arrived while the buffer was full, each with the id it was
     sent to. The handler may not grow the buffer, so it marks the signal here,
     and recv_notifications adds its notification. The id is the one in force
     when the signal arrived, so that a handler installed meanwhile does not
     receive it. Protected by the mutex. */
  int signal_overflow[NSIG];
  intnat signal_overflow_id[NSIG];
};

static struct notification_channel
    *notification_channels[LWT_NOTIFICATION_CHANNELS];

/* Guards allocation and release of slots, never the send path. Initialized
   statically where the platform allows it, so that two domains creating their
   loop at once cannot find it half built. */
#if defined(HAVE_PTHREAD)
#define LWT_NOTIFICATION_MUTEX_IS_STATIC 1
static lwt_unix_mutex notification_channels_mutex = PTHREAD_MUTEX_INITIALIZER;
#else
static lwt_unix_mutex notification_channels_mutex;
#endif

/* The mutex of the signal table, declared here for the initializer below. */
#if defined(HAVE_PTHREAD)
static lwt_unix_mutex signal_table_mutex = PTHREAD_MUTEX_INITIALIZER;
#else
static lwt_unix_mutex signal_table_mutex;
#endif

void initialize_threading(void);

/* Builds the mutexes and the condition that have no static initializer
   (Windows). OCaml calls
   this once, at module initialization, before any other domain exists. */
CAMLprim value lwt_unix_init_notifications(value unit) {
#if !defined(LWT_NOTIFICATION_MUTEX_IS_STATIC)
  lwt_unix_mutex_init(&notification_channels_mutex);
  lwt_unix_mutex_init(&signal_table_mutex);
#endif
  initialize_threading();
  return Val_unit;
}

/* The channel an id names, or NULL if it names none any more: an index out of
   range, a freed slot, or a generation that has moved on. It runs in signal
   handlers, so it reads the slot without the mutex, and the loop that owns the
   channel may write the slot meanwhile. So the slot pointer, [in_use] and
   [generation] are published with release stores and read with acquire
   loads. */
#if defined(__GNUC__) || defined(__clang__)
#define LWT_LOAD_ACQUIRE(p) __atomic_load_n((p), __ATOMIC_ACQUIRE)
#define LWT_STORE_RELEASE(p, v) __atomic_store_n((p), (v), __ATOMIC_RELEASE)
#else
/* Other compilers (MSVC) get plain accesses. Their targets, x86 and x64,
   order these as acquire and release. */
#define LWT_LOAD_ACQUIRE(p) (*(p))
#define LWT_STORE_RELEASE(p, v) (*(p) = (v))
#endif

static struct notification_channel *channel_of_id(intnat id) {
  int i = LWT_NOTIFICATION_INDEX(id);
  struct notification_channel *chan = LWT_LOAD_ACQUIRE(&notification_channels[i]);
  if (chan == NULL || LWT_LOAD_ACQUIRE(&chan->in_use) == 0) return NULL;
  if (LWT_LOAD_ACQUIRE(&chan->generation) != LWT_NOTIFICATION_GEN(id)) return NULL;
  return chan;
}

/* Whether [chan] still is the channel that [id] names. Called with the
   channel's mutex held: while the mutex was released, the loop may have
   retired the channel, and another loop may have taken its slot. */
static int channel_names(struct notification_channel *chan, intnat id) {
  return LWT_LOAD_ACQUIRE(&chan->in_use) != 0 &&
         LWT_LOAD_ACQUIRE(&chan->generation) == LWT_NOTIFICATION_GEN(id);
}

/* Grows the full buffer of [chan], for a notification of [id]. Called with
   the channel's mutex held, and returns with it held. The mutex is released
   around malloc and free, and is never held across them: a signal handler may
   wait for the mutex on a thread that it interrupted inside malloc, and that
   thread holds the allocator's lock.

   While the mutex is released, another sender may grow the buffer, the loop
   may empty it, and the loop may retire the channel. So the state is checked
   again each time the mutex is taken back. Sets [*gone] if [chan] no longer
   names [id], and the caller then drops the notification. Otherwise there is
   room in the buffer. Returns the buffer that the caller must free once it has
   released the mutex, or NULL. */
static intnat *grow_notifications(struct notification_channel *chan, intnat id,
                                  int *gone) {
  intnat *spare = NULL;
  *gone = 0;
  while (chan->index == chan->count) {
    long count = chan->count;
    intnat *bigger;
    lwt_unix_mutex_unlock(&chan->mutex);
    free(spare);
    bigger = (intnat *)lwt_unix_malloc(count * 2 * sizeof(intnat));
    lwt_unix_mutex_lock(&chan->mutex);
    if (!channel_names(chan, id)) {
      *gone = 1;
      return bigger;
    }
    if (chan->count == count && chan->index == count) {
      memcpy((void *)bigger, (void *)chan->notifications,
             count * sizeof(intnat));
      spare = chan->notifications;
      chan->notifications = bigger;
      chan->count = count * 2;
    } else {
      /* Another sender grew the buffer, or the loop emptied it. */
      spare = bigger;
    }
  }
  return spare;
}

/* Sends [id]. Returns 0, or the error of the write that wakes the main thread,
   for the caller to report once the lock is released and the signal mask
   restored.

   [signum] is the signal being handled, or -1 for an ordinary send. The
   Windows Ctrl+C handler passes SIGINT too: it runs on a console thread,
   without the runtime lock, so it may not raise either. A signal handler may
   neither allocate nor raise. So with a full buffer it marks the
   signal pending instead of growing the buffer, and recv_notifications adds
   the signal's notification. No wake-up is needed then, as a notification is
   already pending. Several deliveries of one signal merge into one, as POSIX
   does with pending signals. */
static int send_notification(intnat id, int signum) {
  int ret;
  int error = 0;
  struct notification_channel *chan;
  intnat *spare = NULL;
#if !defined(LWT_ON_WINDOWS)
  sigset_t new_mask;
  sigset_t old_mask;
  sigfillset(&new_mask);
  pthread_sigmask(SIG_SETMASK, &new_mask, &old_mask);
#endif
  chan = channel_of_id(id);
  if (chan == NULL) {
    /* The loop this id belonged to is gone: drop the notification. */
#if !defined(LWT_ON_WINDOWS)
    pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif
    return 0;
  }
  lwt_unix_mutex_lock(&chan->mutex);
  /* Checked again under the mutex: the loop may have retired its channel since
     the lookup. Retiring takes this mutex, so the transport stays open until we
     release it. */
  if (LWT_LOAD_ACQUIRE(&chan->in_use) == 0 ||
      LWT_LOAD_ACQUIRE(&chan->generation) != LWT_NOTIFICATION_GEN(id)) {
    lwt_unix_mutex_unlock(&chan->mutex);
#if !defined(LWT_ON_WINDOWS)
    pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif
    return 0;
  }
  if (chan->index > 0 && chan->index == chan->count) {
    int gone;
    /* The buffer is full, and so a notification is already pending. */
    if (signum >= 0) {
      chan->signal_overflow[signum] = 1;
      chan->signal_overflow_id[signum] = id;
      goto done;
    }
    /* This may release the mutex, so decide about the wake-up only after. */
    spare = grow_notifications(chan, id, &gone);
    if (gone) goto done;
  }
  if (chan->index > 0) {
    /* There is already a pending notification in the buffer, no
       need to signal the main thread. */
    chan->notifications[chan->index++] = id;
  } else {
    /* There is none, notify the main thread. */
    chan->notifications[chan->index++] = id;
    ret = chan->send(chan);
#if defined(LWT_ON_WINDOWS)
    if (ret == SOCKET_ERROR) {
      error = WSAGetLastError();
      /* Not initialized: we are probably shutting down, so ignore it. */
      if (error == WSANOTINITIALISED) error = 0;
    }
#else
    if (ret < 0) error = errno;
#endif
  }
done:
  lwt_unix_mutex_unlock(&chan->mutex);
#if !defined(LWT_ON_WINDOWS)
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif
  /* Not even free(NULL) on the signal handler's path. */
  if (spare != NULL) free(spare);
  return error;
}

void lwt_unix_send_notification(intnat id) {
  int error = send_notification(id, -1);
  if (error != 0) {
#if defined(LWT_ON_WINDOWS)
    win32_maperr(error);
    uerror("send_notification", Nothing);
#else
    unix_error(error, "send_notification", Nothing);
#endif
  }
}

value lwt_unix_send_notification_stub(value id) {
  lwt_unix_send_notification(Long_val(id));
  return Val_unit;
}

/* The size of the buffer that notification [id] goes to, for the tests:
   signal_full_buffer checks with it that the signal handler does not grow the
   buffer. The id names the buffer's channel. 0 if it names none any more.
   Read without the mutex, which is enough for a test: a sender may change the
   size meanwhile, and the test then reads the old size or the new one. */
value lwt_unix_notification_buffer_size(value id) {
  struct notification_channel *chan = channel_of_id(Long_val(id));
  return Val_long(chan == NULL ? 0 : chan->count);
}

/* How many notifications wait in the channel at the index of [id], whatever
   the generation of [id], or -1 if that slot is free. For the tests:
   domain_notifications checks with it that an id of a departed loop is not
   queued for the next owner of its slot. Read without the mutex, as above. */
value lwt_unix_notification_pending(value id) {
  struct notification_channel *chan = LWT_LOAD_ACQUIRE(
      &notification_channels[LWT_NOTIFICATION_INDEX(Long_val(id))]);
  if (chan == NULL || LWT_LOAD_ACQUIRE(&chan->in_use) == 0) return Val_long(-1);
  return Val_long(chan->index);
}

/* How many signals the handler marked pending on [chan] instead of buffering
   them. Under the mutex. */
static int signal_overflow_count(struct notification_channel *chan) {
  int signum, n = 0;
  for (signum = 0; signum < NSIG; signum++)
    if (chan->signal_overflow[signum]) n++;
  return n;
}

/* Drains one channel, the caller's. Takes the index rather than reading any
   global: the domain doing the draining is the one whose loop woke up. */
value lwt_unix_recv_notifications(value val_index) {
  int ret, i, signum, current_index;
  value result;
  struct notification_channel *chan;
#if !defined(LWT_ON_WINDOWS)
  sigset_t new_mask;
  sigset_t old_mask;
  int error;
  sigfillset(&new_mask);
  pthread_sigmask(SIG_SETMASK, &new_mask, &old_mask);
#else
  DWORD error;
#endif
  chan = notification_channels[Int_val(val_index)];
  if (chan == NULL || chan->in_use == 0) {
#if !defined(LWT_ON_WINDOWS)
    pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif
    caml_failwith("Lwt_unix: draining a notification channel that is gone");
  }
  lwt_unix_mutex_lock(&chan->mutex);
  /* Receive the signal. */
  ret = chan->recv(chan);
#if defined(LWT_ON_WINDOWS)
  if (ret == SOCKET_ERROR) {
    error = WSAGetLastError();
    lwt_unix_mutex_unlock(&chan->mutex);
    win32_maperr(error);
    uerror("recv_notifications", Nothing);
  }
#else
  if (ret < 0) {
    error = errno;
    lwt_unix_mutex_unlock(&chan->mutex);
    pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
    unix_error(error, "recv_notifications", Nothing);
  }
#endif

  do {
    /*
     release the mutex while calling caml_alloc,
     which may call gc and switch the thread,
     resulting in a classical deadlock,
     when thread in question tries another send
    */
    current_index = chan->index + signal_overflow_count(chan);
    lwt_unix_mutex_unlock(&chan->mutex);
    result = caml_alloc_tuple(current_index);
    lwt_unix_mutex_lock(&chan->mutex);
    /* check that no new notifications appeared meanwhile (rare) */
  } while (current_index != chan->index + signal_overflow_count(chan));

  /* Read all pending notifications. */
  for (i = 0; i < chan->index; i++)
    Field(result, i) = Val_long(chan->notifications[i]);
  /* Then the signals the handler could not buffer, each with the id it was
     sent to. */
  for (signum = 0; signum < NSIG; signum++)
    if (chan->signal_overflow[signum]) {
      chan->signal_overflow[signum] = 0;
      Field(result, i++) = Val_long(chan->signal_overflow_id[signum]);
    }
  /* Reset the index. */
  chan->index = 0;
  lwt_unix_mutex_unlock(&chan->mutex);
#if !defined(LWT_ON_WINDOWS)
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif
  return result;
}

/* Builds the id a loop hands out: the channel's index and generation, plus the
   local counter OCaml manages. Done here so that the layout lives in one place,
   next to the code that decodes it. */
CAMLprim value lwt_unix_encode_notification(value val_index, value val_local) {
  struct notification_channel *chan = notification_channels[Int_val(val_index)];
  intnat gen = (chan == NULL) ? 0 : (intnat)(chan->generation);
  intnat local = Long_val(val_local) & LWT_NOTIFICATION_LOCAL_MASK;
  return Val_long((((intnat)Int_val(val_index))
                   << (LWT_NOTIFICATION_LOCAL_BITS + LWT_NOTIFICATION_GEN_BITS)) |
                  ((gen & LWT_NOTIFICATION_GEN_MASK)
                   << LWT_NOTIFICATION_LOCAL_BITS) |
                  local);
}

/* The index of the channel an id names. The OCaml side uses it to purge a
   retired channel's entries from its table. */
CAMLprim value lwt_unix_notification_channel_index(value val_id) {
  return Val_int(LWT_NOTIFICATION_INDEX(Long_val(val_id)));
}

/* How many local parts an id can have, so that make_notification knows when
   a loop has used them all. */
CAMLprim value lwt_unix_notification_local_count(value unit) {
  return Val_long(LWT_NOTIFICATION_LOCAL_MASK + 1);
}

#if defined(LWT_ON_WINDOWS)

static int windows_notification_send(struct notification_channel *chan) {
  char buf = '!';
  return send(chan->socket_w, &buf, 1, 0);
}

static int windows_notification_recv(struct notification_channel *chan) {
  char buf;
  return recv(chan->socket_r, &buf, 1, 0);
}

/* Gives the channel its transport, and returns the descriptor to watch. */
static value channel_open_transport(struct notification_channel *chan) {
  SOCKET sockets[2];
  /* Since pipes do not works with select, we need to use a pair of
     sockets. */
  lwt_unix_socketpair(AF_INET, SOCK_STREAM, IPPROTO_TCP, sockets, FALSE);
  chan->socket_r = sockets[0];
  chan->socket_w = sockets[1];
  chan->mode = NOTIFICATION_MODE_WINDOWS;
  chan->send = windows_notification_send;
  chan->recv = windows_notification_recv;
  return win_alloc_socket(chan->socket_r);
}

static void channel_close_transport(struct notification_channel *chan) {
  if (chan->mode == NOTIFICATION_MODE_WINDOWS) {
    closesocket(chan->socket_r);
    closesocket(chan->socket_w);
  }
  chan->mode = NOTIFICATION_MODE_NONE;
}

#else /* defined(LWT_ON_WINDOWS) */

static void set_close_on_exec(int fd) {
  int flags = fcntl(fd, F_GETFD, 0);
  if (flags == -1 || fcntl(fd, F_SETFD, flags | FD_CLOEXEC) == -1)
    uerror("set_close_on_exec", Nothing);
}

#if defined(HAVE_EVENTFD)

static int eventfd_notification_send(struct notification_channel *chan) {
  uint64_t buf = 1;
  return write(chan->fd, (char *)&buf, 8);
}

static int eventfd_notification_recv(struct notification_channel *chan) {
  uint64_t buf;
  return read(chan->fd, (char *)&buf, 8);
}

#endif /* defined(HAVE_EVENTFD) */

static int pipe_notification_send(struct notification_channel *chan) {
  char buf = 0;
  return write(chan->fds[1], &buf, 1);
}

static int pipe_notification_recv(struct notification_channel *chan) {
  char buf;
  return read(chan->fds[0], &buf, 1);
}

/* Gives the channel its transport, and returns the descriptor a loop watches.
   An eventfd when the platform has one, a pipe otherwise. The choice is made
   per channel, since two loops need not agree on it. */
static value channel_open_transport(struct notification_channel *chan) {
#if defined(HAVE_EVENTFD)
  chan->fd = eventfd(0, 0);
  if (chan->fd != -1) {
    chan->mode = NOTIFICATION_MODE_EVENTFD;
    chan->send = eventfd_notification_send;
    chan->recv = eventfd_notification_recv;
    set_close_on_exec(chan->fd);
    return Val_int(chan->fd);
  }
#endif
  if (pipe(chan->fds) == -1) uerror("pipe", Nothing);
  set_close_on_exec(chan->fds[0]);
  set_close_on_exec(chan->fds[1]);
  chan->mode = NOTIFICATION_MODE_PIPE;
  chan->send = pipe_notification_send;
  chan->recv = pipe_notification_recv;
  return Val_int(chan->fds[0]);
}

static void channel_close_transport(struct notification_channel *chan) {
  switch (chan->mode) {
#if defined(HAVE_EVENTFD)
    case NOTIFICATION_MODE_EVENTFD:
      close(chan->fd);
      break;
#endif
    case NOTIFICATION_MODE_PIPE:
      close(chan->fds[0]);
      close(chan->fds[1]);
      break;
    default:
      break;
  }
  chan->mode = NOTIFICATION_MODE_NONE;
}

#endif /* defined(LWT_ON_WINDOWS) */

/* Moves a transport opened on the stack into a channel. */
static void channel_adopt_transport(struct notification_channel *chan,
                                    const struct notification_channel *t) {
  chan->mode = t->mode;
  chan->send = t->send;
  chan->recv = t->recv;
#if defined(LWT_ON_WINDOWS)
  chan->socket_r = t->socket_r;
  chan->socket_w = t->socket_w;
#else
  chan->fd = t->fd;
  chan->fds[0] = t->fds[0];
  chan->fds[1] = t->fds[1];
#endif
}

/* Creates a channel for the calling loop and returns (descriptor, index).
   The index is the caller's handle on it, and all that OCaml needs to keep. */
CAMLprim value lwt_unix_new_notification_channel(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(result, fd);
  int i, found = -1;
  struct notification_channel *chan;
  struct notification_channel transport;

  /* The transport is opened before the table is locked, into a channel on the
     stack: opening can fail (EMFILE) and raise, and raising with the mutex held
     would hang every later loop. */
  memset((void *)&transport, 0, sizeof(struct notification_channel));
  fd = channel_open_transport(&transport);

  lwt_unix_mutex_lock(&notification_channels_mutex);
  for (i = 0; i < LWT_NOTIFICATION_CHANNELS; i++) {
    if (notification_channels[i] == NULL ||
        notification_channels[i]->in_use == 0) {
      found = i;
      break;
    }
  }
  if (found == -1) {
    lwt_unix_mutex_unlock(&notification_channels_mutex);
    channel_close_transport(&transport);
    caml_failwith(
        "Lwt_unix: too many notification channels (one per Lwt loop; raise "
        "LWT_NOTIFICATION_CHANNELS)");
  }

  chan = notification_channels[found];
  if (chan == NULL) {
    /* A slot used for the first time: allocate, and let the buffer live as long
       as the process. Reusing the slot later reuses the buffer too. */
    chan = (struct notification_channel *)lwt_unix_malloc(
        sizeof(struct notification_channel));
    memset((void *)chan, 0, sizeof(struct notification_channel));
    lwt_unix_mutex_init(&chan->mutex);
    chan->count = 4096;
    chan->notifications =
        (intnat *)lwt_unix_malloc(chan->count * sizeof(intnat));
    chan->generation = 0;
    chan->index = 0;
    channel_adopt_transport(chan, &transport);
    /* The slot pointer is published last: [channel_of_id] reads this slot
       without the mutex, so nothing may name the channel before it is
       complete. */
    LWT_STORE_RELEASE(&chan->in_use, 1);
    LWT_STORE_RELEASE(&notification_channels[found], chan);
  } else {
    /* A slot used before: a new generation, so that the ids of its
       previous owner stop naming it. Under the channel's mutex, with signals
       masked, so that a late sender sees the old state or the new one. */
#if !defined(LWT_ON_WINDOWS)
    sigset_t new_mask, old_mask;
    sigfillset(&new_mask);
    pthread_sigmask(SIG_SETMASK, &new_mask, &old_mask);
#endif
    lwt_unix_mutex_lock(&chan->mutex);
    LWT_STORE_RELEASE(&chan->generation,
        (chan->generation + 1) & (unsigned)LWT_NOTIFICATION_GEN_MASK);
    chan->index = 0;
    memset(chan->signal_overflow, 0, sizeof chan->signal_overflow);
    channel_adopt_transport(chan, &transport);
    LWT_STORE_RELEASE(&chan->in_use, 1);
    lwt_unix_mutex_unlock(&chan->mutex);
#if !defined(LWT_ON_WINDOWS)
    pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif
  }
  lwt_unix_mutex_unlock(&notification_channels_mutex);

  result = caml_alloc_tuple(2);
  Store_field(result, 0, fd);
  Store_field(result, 1, Val_int(found));
  CAMLreturn(result);
}

/* Retires the calling loop's channel: its descriptors are closed and its slot
   becomes free. Ids that named it are dropped from then on. */
CAMLprim value lwt_unix_free_notification_channel(value val_index) {
  int i = Int_val(val_index);
  struct notification_channel *chan;
#if !defined(LWT_ON_WINDOWS)
  sigset_t new_mask;
  sigset_t old_mask;
#endif
  lwt_unix_mutex_lock(&notification_channels_mutex);
  chan = notification_channels[i];
  if (chan != NULL && chan->in_use) {
    /* Under the channel's mutex, with signals masked: a sender that found the
       channel in use waits here, and then sees it retired. */
#if !defined(LWT_ON_WINDOWS)
    sigfillset(&new_mask);
    pthread_sigmask(SIG_SETMASK, &new_mask, &old_mask);
#endif
    lwt_unix_mutex_lock(&chan->mutex);
    LWT_STORE_RELEASE(&chan->in_use, 0);
    chan->index = 0;
    channel_close_transport(chan);
    lwt_unix_mutex_unlock(&chan->mutex);
#if !defined(LWT_ON_WINDOWS)
    pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif
  }
  lwt_unix_mutex_unlock(&notification_channels_mutex);
  return Val_unit;
}

/* For the child of a fork: the inherited transport is the parent's, so it is
   replaced. The channel keeps its slot and generation, so that every id made
   before the fork still names it. The notifications still pending in its
   buffer are the parent's, and are dropped, as are the signals the handler
   could not buffer: a signal was sent to the parent, and the child cancels the
   parent's jobs.

   The mutex is not taken, since the child has a single thread. It is rebuilt
   instead: a thread of the parent may have held it at the fork, and that
   thread does not exist in the child.

   The new transport is opened first, and the switch is made with every signal
   blocked, as senders do around the buffer: a signal handler running in the
   middle would write to a descriptor about to be closed, and leave an id in
   the buffer with no wake-up. The old transport is closed afterwards. */
CAMLprim value lwt_unix_reset_notification_channel(value val_index) {
  CAMLparam1(val_index);
  CAMLlocal1(fd);
  struct notification_channel transport, old;
  struct notification_channel *chan = notification_channels[Int_val(val_index)];
#if !defined(LWT_ON_WINDOWS)
  sigset_t new_mask, old_mask;
#endif
  if (chan == NULL || chan->in_use == 0)
    caml_failwith("Lwt_unix: resetting a notification channel that is gone");
  memset((void *)&transport, 0, sizeof(struct notification_channel));
  memset((void *)&old, 0, sizeof(struct notification_channel));
  fd = channel_open_transport(&transport);
  lwt_unix_mutex_init(&chan->mutex);
#if !defined(LWT_ON_WINDOWS)
  sigfillset(&new_mask);
  pthread_sigmask(SIG_SETMASK, &new_mask, &old_mask);
#endif
  channel_adopt_transport(&old, chan);
  channel_adopt_transport(chan, &transport);
  chan->index = 0;
  memset(chan->signal_overflow, 0, sizeof chan->signal_overflow);
#if !defined(LWT_ON_WINDOWS)
  pthread_sigmask(SIG_SETMASK, &old_mask, NULL);
#endif
  channel_close_transport(&old);
  CAMLreturn(fd);
}

/* +-----------------------------------------------------------------+
   | Signals                                                         |
   +-----------------------------------------------------------------+ */

/* Subscribers per signal: one notification id per interested loop, indexed by
   that loop's channel, so that every loop that asked gets the signal. A table
   of integers, 0 meaning nobody, since [handle_signal] may not allocate, lock
   or follow a freed pointer. It is written under [signal_table_mutex] and
   read by the handler without it, so it is accessed with release stores and
   acquire loads, which are async-signal-safe. The table is defined further
   up, with the channels. */

CAMLextern int caml_convert_signal_number(int);

/* Send a notification to every loop subscribed to this signal. [errno] is
   saved and restored: sending the notification may set it, and the handler
   may have interrupted a thread between a failing system call and its reading
   of [errno]. */
static void handle_signal(int signum) {
  int i;
  int saved_errno = errno;
  if (signum >= 0 && signum < NSIG) {
#if defined(LWT_ON_WINDOWS)
    /* The signal handler must be reinstalled if we use the signal
       function. */
    signal(signum, handle_signal);
#endif
    for (i = 0; i < LWT_NOTIFICATION_CHANNELS; i++) {
      intnat id = LWT_LOAD_ACQUIRE(&signal_notifications[signum][i]);
      if (id != 0) (void)send_notification(id, signum);
    }
  }
  errno = saved_errno;
}

/* [signal_table_mutex] serializes the changes of subscription across loops:
   updating the count, writing the id and installing the process-wide handler
   must happen in one step, or a subscribed loop could be left with no handler.
   The runtime lock is per domain in OCaml 5, so it does not serialize them.
   [handle_signal] never takes this mutex. */

/* Per signal, the subscriptions that need the process-wide handler: those
   whose loop's engine does not forward the signal itself. Each subscription
   remembers whether it was counted, since a loop may change engines between
   subscribing and unsubscribing. Under [signal_table_mutex]. */
static int signal_handler_users[NSIG];
static char signal_counted[NSIG][LWT_NOTIFICATION_CHANNELS];

#if defined(LWT_ON_WINDOWS)
/* Defined with the Windows code below. */
static BOOL WINAPI handle_break(DWORD event);
#endif

/* Installs the process-wide handler for [signum]. Returns 0, or the error code
   with the name of the call that failed in [*name], so that the caller can
   raise once it has released the mutex. The code is an errno value on every
   platform: signal() sets errno on Windows too, and a Windows error code is
   mapped to one. */
static int install_process_handler(int signum, const char **name) {
#if defined(LWT_ON_WINDOWS)
  if (signum == SIGINT) {
    if (!SetConsoleCtrlHandler(handle_break, TRUE)) {
      *name = "SetConsoleCtrlHandler";
      win32_maperr(GetLastError());
      return errno;
    }
  } else {
    if (signal(signum, handle_signal) == SIG_ERR) {
      *name = "signal";
      return errno;
    }
  }
#else
  struct sigaction sa;
  sa.sa_handler = handle_signal;
#if OCAML_VERSION >= 50000
  sa.sa_flags = SA_ONSTACK;
#else
  sa.sa_flags = 0;
#endif
  sigemptyset(&sa.sa_mask);
  if (sigaction(signum, &sa, NULL) == -1) {
    *name = "sigaction";
    return errno;
  }
#endif
  return 0;
}

static void uninstall_process_handler(int signum) {
#if defined(LWT_ON_WINDOWS)
  if (signum == SIGINT)
    SetConsoleCtrlHandler(NULL, FALSE);
  else
    signal(signum, SIG_DFL);
#else
  struct sigaction sa;
  sa.sa_handler = SIG_DFL;
  sa.sa_flags = 0;
  sigemptyset(&sa.sa_mask);
  sigaction(signum, &sa, NULL);
#endif
}

/* [error] is an errno value. */
static void raise_handler_error(int error, const char *name) {
  unix_error(error, name, Nothing);
}

/* For an engine that forwards signals ([Lwt_engine.forwards_signal]). Called
   from OCaml, not from a signal handler, so the notifications take the
   ordinary path: a buffer may grow, and an error is raised only once every
   subscribed loop has been sent the signal. */
CAMLprim value lwt_unix_handle_signal(value val_signum) {
  int i, error = 0;
  int signum = caml_convert_signal_number(Int_val(val_signum));
  if (signum >= 0 && signum < NSIG) {
    for (i = 0; i < LWT_NOTIFICATION_CHANNELS; i++) {
      intnat id = LWT_LOAD_ACQUIRE(&signal_notifications[signum][i]);
      if (id != 0) {
        int e = send_notification(id, -1);
        if (error == 0) error = e;
      }
    }
  }
  if (error != 0) {
#if defined(LWT_ON_WINDOWS)
    /* send_notification returns a Windows error code there. */
    win32_maperr(error);
    error = errno;
#endif
    raise_handler_error(error, "send_notification");
  }
  return Val_unit;
}

#if defined(LWT_ON_WINDOWS)
/* Handle Ctrl+C on windows. */
static BOOL WINAPI handle_break(DWORD event) {
  int i, sent = 0;
  if (event != CTRL_C_EVENT && event != CTRL_BREAK_EVENT) return FALSE;
  for (i = 0; i < LWT_NOTIFICATION_CHANNELS; i++) {
    intnat id = LWT_LOAD_ACQUIRE(&signal_notifications[SIGINT][i]);
    if (id != 0) {
      (void)send_notification(id, SIGINT);
      sent = 1;
    }
  }
  return sent ? TRUE : FALSE;
}
#endif

/* Install a signal handler. */
CAMLprim value lwt_unix_set_signal(value val_signum, value val_notification, value val_forwarded) {
  int signum = caml_convert_signal_number(Int_val(val_signum));
  intnat notification = Long_val(val_notification);
  int slot;
  int error = 0;
  const char *name = NULL;

  if (signum < 0 || signum >= NSIG)
    caml_invalid_argument("Lwt_unix.on_signal: unavailable signal");

  /* The id names the subscribing loop's channel, so it also says where to
     record the subscription. */
  slot = LWT_NOTIFICATION_INDEX(notification);
  lwt_unix_mutex_lock(&signal_table_mutex);
  LWT_STORE_RELEASE(&signal_notifications[signum][slot], notification);

  /* Only the first subscription that needs the process-wide handler installs
     it. A later one must neither reinstall it nor be told it failed. */
  if (!Bool_val(val_forwarded) && !signal_counted[signum][slot]) {
    if (signal_handler_users[signum] == 0)
      error = install_process_handler(signum, &name);
    if (error == 0) {
      signal_handler_users[signum]++;
      signal_counted[signum][slot] = 1;
    } else
      LWT_STORE_RELEASE(&signal_notifications[signum][slot], 0);
  }
  lwt_unix_mutex_unlock(&signal_table_mutex);

  if (error != 0) raise_handler_error(error, name);
  return Val_unit;
}

/* Remove one loop's subscription, and the process-wide handler with the last of
   them. */
CAMLprim value lwt_unix_remove_signal(value val_signum, value val_notification) {
  /* The signal number is valid here since it was when we did the
     set_signal. */
  int signum = caml_convert_signal_number(Int_val(val_signum));
  int slot = LWT_NOTIFICATION_INDEX(Long_val(val_notification));

  lwt_unix_mutex_lock(&signal_table_mutex);
  LWT_STORE_RELEASE(&signal_notifications[signum][slot], 0);
  if (signal_counted[signum][slot]) {
    signal_counted[signum][slot] = 0;
    if (--signal_handler_users[signum] == 0) uninstall_process_handler(signum);
  }
  lwt_unix_mutex_unlock(&signal_table_mutex);
  return Val_unit;
}

/* Puts the process-wide handler back, for a signal some loop is subscribed to,
   after another library replaced it ([Lwt_unix.reinstall_signal_handler]). */
CAMLprim value lwt_unix_reinstall_signal(value val_signum) {
  int signum = caml_convert_signal_number(Int_val(val_signum));
  int error = 0;
  const char *name = NULL;

  if (signum < 0 || signum >= NSIG)
    caml_invalid_argument("Lwt_unix.reinstall_signal_handler: unavailable signal");

  lwt_unix_mutex_lock(&signal_table_mutex);
  if (signal_handler_users[signum] > 0)
    error = install_process_handler(signum, &name);
  lwt_unix_mutex_unlock(&signal_table_mutex);

  if (error != 0) raise_handler_error(error, name);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Job execution                                                   |
   +-----------------------------------------------------------------+ */

/* Execute the given job. */
static void execute_job(lwt_unix_job job) {
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
    /* Not lwt_unix_send_notification, which raises on error: this thread
       does not hold the runtime lock, and raising here is not allowed. A
       failed wake-up write is ignored, as in the signal handler. */
    (void)send_notification(job->notification_id, -1);
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

/* Condition on which pool threads are waiting. Built statically where the
   platform allows, with the mutex below, since two loops may submit their
   first job at once. On Windows, [initialize_threading] builds both, once, at
   module initialization. */
#if defined(HAVE_PTHREAD)
#define LWT_POOL_SYNC_IS_STATIC 1
static lwt_unix_condition pool_condition = PTHREAD_COND_INITIALIZER;
#else
static lwt_unix_condition pool_condition;
#endif

/* Queue of pending jobs. It points to the last enqueued job.  */
static lwt_unix_job pool_queue = NULL;

/* The mutex which protect access to [pool_queue], [pool_condition]
   and [thread_waiting_count]. */
#if defined(HAVE_PTHREAD)
static lwt_unix_mutex pool_mutex = PTHREAD_MUTEX_INITIALIZER;
#else
static lwt_unix_mutex pool_mutex;
#endif

/* +-----------------------------------------------------------------+
   | Threading stuff initialization                                  |
   +-----------------------------------------------------------------+ */

#if !defined(LWT_POOL_SYNC_IS_STATIC)
/* Whether threading has been initialized, where the synchronization is not
   static. */
static int threading_initialized = 0;
#endif

/* Initialize the pool of thread. A no-op where the mutex and the condition
   are static. */
void initialize_threading(void) {
#if !defined(LWT_POOL_SYNC_IS_STATIC)
  if (threading_initialized == 0) {
    lwt_unix_mutex_init(&pool_mutex);
    lwt_unix_condition_init(&pool_condition);

    threading_initialized = 1;
  }
#endif
}

/* +-----------------------------------------------------------------+
   | Worker loop                                                     |
   +-----------------------------------------------------------------+ */

/* Function executed by threads of the pool.
 * Note: all signals are masked for this thread. */
static void *worker_loop(void *data) {
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

  /* Ensure threading is initialized before using pool_mutex.
     On Windows, CRITICAL_SECTION must be initialized before use;
     unlike pthreads, zero-initialized CRITICAL_SECTIONs are invalid. */
  if (async_method != LWT_UNIX_ASYNC_METHOD_NONE) {
    initialize_threading();
  }

  /* Fallback to synchronous call if there is no worker available and
     we can not launch more threads. */
  if (async_method != LWT_UNIX_ASYNC_METHOD_NONE) {
    lwt_unix_mutex_lock(&pool_mutex);
  	if (thread_waiting_count == 0 && thread_count >= pool_size)
      async_method = LWT_UNIX_ASYNC_METHOD_NONE;
    lwt_unix_mutex_unlock(&pool_mutex);
	}

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

      lwt_unix_mutex_lock(&job->mutex);
      done = job->state == LWT_UNIX_JOB_STATE_DONE;
      lwt_unix_mutex_unlock(&job->mutex);
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
  /* There is no more waiting threads. */
  thread_waiting_count = 0;

  /* There is no more threads. */
  thread_count = 0;

  /* Empty the queue. */
  pool_queue = NULL;

  /* The child has a single thread, and a worker of the parent may have held
     the mutex at the moment of the fork: build the mutex and the condition
     again. */
#if defined(LWT_POOL_SYNC_IS_STATIC)
  lwt_unix_mutex_init(&pool_mutex);
  lwt_unix_condition_init(&pool_condition);
#else
  threading_initialized = 0;
#endif

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
