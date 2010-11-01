/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_stubs
 * Copyright (C) 2009-2010 Jérémie Dimino
 *               2009 Mauricio Fernandez
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
#include <signal.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>

#include "lwt_unix.h"

#if defined(LWT_WINDOWS)
#  include <windows.h>
#else
#  include <sys/socket.h>
#  include <pthread.h>
#endif

/* +-----------------------------------------------------------------+
   | Read/write                                                      |
   +-----------------------------------------------------------------+ */

#if !defined(LWT_WINDOWS)

/* This code is a simplified version of the default unix_write and
   unix_read functions of caml.

   Since we know that reading or writing will never block we can
   directly use the buffer from the managed memory without copying it
   (thus removing the limitation of 16KB by operation).
*/

value lwt_unix_write(value fd, value buf, value ofs, value len)
{
  int ret;
  ret = write(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len));
  if (ret == -1) uerror("lwt_unix_write", Nothing);
  return Val_int(ret);
}

value lwt_unix_read(value fd, value buf, value ofs, value len)
{
  int ret;
  ret = read(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len));
  if (ret == -1) uerror("lwt_unix_read", Nothing);
  return Val_int(ret);
}

#else

value lwt_unix_write(value fd, value buf, value ofs, value len)
{
  invalid_argument("write not implemented");
}

value lwt_unix_read(value fd, value buf, value ofs, value len)
{
  invalid_argument("read not implemented");
}

#endif

/* +-----------------------------------------------------------------+
   | Recv/send                                                       |
   +-----------------------------------------------------------------+ */

#if !defined(LWT_WINDOWS)

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

value lwt_unix_recv(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = recv(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len),
             convert_flag_list(flags, msg_flag_table));
  if (ret == -1) uerror("lwt_unix_recv", Nothing);
  return Val_int(ret);
}

value lwt_unix_send(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = send(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len),
             convert_flag_list(flags, msg_flag_table));
  if (ret == -1) uerror("lwt_unix_send", Nothing);
  return Val_int(ret);
}

#else

value lwt_unix_recv(value fd, value buf, value ofs, value len, value flags)
{
  invalid_argument("recv not implemented");
}

value lwt_unix_send(value fd, value buf, value ofs, value len, value flags)
{
  invalid_argument("send not implemented");
}

#endif

/* +-----------------------------------------------------------------+
   | {recv/send}_msg                                                 |
   +-----------------------------------------------------------------+ */

#if !defined(LWT_WINDOWS)

/* Convert a caml list of io-vectors into a C array io io-vector
   structures */
static void store_iovs(struct iovec *iovs, value iovs_val)
{
  CAMLparam0();
  CAMLlocal2(list, x);
  for(list = iovs_val; Is_block(list); list = Field(list, 1), iovs++) {
    x = Field(list, 0);
    iovs->iov_base = &Byte(String_val(Field(x, 0)), Long_val(Field(x, 1)));
    iovs->iov_len = Long_val(Field(x, 2));
  }
  CAMLreturn0;
}

CAMLprim value lwt_unix_recv_msg(value sock_val, value n_iovs_val, value iovs_val)
{
  CAMLparam3(sock_val, n_iovs_val, iovs_val);
  CAMLlocal3(list, result, x);

  int n_iovs = Int_val(n_iovs_val);
  struct iovec iovs[n_iovs];
  store_iovs(iovs, iovs_val);

  struct msghdr msg;
  memset(&msg, 0, sizeof(msg));
  msg.msg_iov = iovs;
  msg.msg_iovlen = n_iovs;
  msg.msg_controllen = CMSG_SPACE(256 * sizeof(int));
  msg.msg_control = alloca(msg.msg_controllen);
  memset(msg.msg_control, 0, msg.msg_controllen);

  int ret = recvmsg(Int_val(sock_val), &msg, 0);
  if (ret == -1) uerror("lwt_unix_recv_msg", Nothing);

  struct cmsghdr *cm;
  list = Val_int(0);
  for (cm = CMSG_FIRSTHDR(&msg); cm; cm = CMSG_NXTHDR(&msg, cm))
    if (cm->cmsg_level == SOL_SOCKET && cm->cmsg_type == SCM_RIGHTS) {
      int *fds = (int*)CMSG_DATA(cm);
      int nfds = (cm->cmsg_len - CMSG_LEN(0)) / sizeof(int);
      int i;
      for(i = nfds - 1; i >= 0; i--) {
        x = caml_alloc_tuple(2);
        Store_field(x, 0, Val_int(fds[i]));
        Store_field(x, 1, list);
        list = x;
      };
      break;
    };

  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(ret));
  Store_field(result, 1, list);
  CAMLreturn(result);
}

CAMLprim value lwt_unix_send_msg(value sock_val, value n_iovs_val, value iovs_val, value n_fds_val, value fds_val)
{
  CAMLparam5(sock_val, n_iovs_val, iovs_val, n_fds_val, fds_val);
  CAMLlocal1(x);

  int n_iovs = Int_val(n_iovs_val);
  struct iovec iovs[n_iovs];
  store_iovs(iovs, iovs_val);

  struct msghdr msg;
  memset(&msg, 0, sizeof(msg));
  msg.msg_iov = iovs;
  msg.msg_iovlen = n_iovs;

  int n_fds = Int_val(n_fds_val);
  if (n_fds > 0) {
    msg.msg_controllen = CMSG_SPACE(n_fds * sizeof(int));
    msg.msg_control = alloca(msg.msg_controllen);
    memset(msg.msg_control, 0, msg.msg_controllen);

    struct cmsghdr *cm;
    cm = CMSG_FIRSTHDR(&msg);
    cm->cmsg_level = SOL_SOCKET;
    cm->cmsg_type = SCM_RIGHTS;
    cm->cmsg_len = CMSG_LEN(n_fds * sizeof(int));

    int *fds = (int*)CMSG_DATA(cm);
    for(x = fds_val; Is_block(x); x = Field(x, 1), fds++)
      *fds = Int_val(Field(x, 0));
  };

  int ret = sendmsg(Int_val(sock_val), &msg, 0);
  if (ret == -1) uerror("lwt_unix_send_msg", Nothing);
  CAMLreturn(Val_int(ret));
}

#else

value lwt_unix_recv_msg(value sock_val, value n_iovs_val, value iovs_val)
{
  invalid_argument("recv_msg not implemented");
}

value lwt_unix_send_msg(value sock_val, value n_iovs_val, value iovs_val, value n_fds_val, value fds_val)
{
  invalid_argument("send_msg not implemented");
}

#endif

/* +-----------------------------------------------------------------+
   | Credentials                                                     |
   +-----------------------------------------------------------------+ */

#if defined(SO_PEERCRED) && !defined(LWT_WINDOWS)

#include <sys/un.h>

CAMLprim value lwt_unix_get_credentials(value fd)
{
    CAMLparam1(fd);
    CAMLlocal1(res);
    struct ucred cred;
    socklen_t cred_len = sizeof(cred);

    if (getsockopt(Int_val(fd), SOL_SOCKET, SO_PEERCRED, &cred, &cred_len) == -1)
      uerror("lwt_unix_get_credentials", Nothing);

    res = caml_alloc_tuple(3);
    Store_field(res, 0, Val_int(cred.pid));
    Store_field(res, 1, Val_int(cred.uid));
    Store_field(res, 2, Val_int(cred.gid));
    CAMLreturn(res);
}

#else

CAMLprim value lwt_unix_get_credentials(value fd_val)
{
  invalid_argument("get_credentials not implemented");
}

#endif

/* +-----------------------------------------------------------------+
   | Terminal sizes                                                  |
   +-----------------------------------------------------------------+ */

#if defined(LWT_WINDOWS)

#include <wincon.h>

CAMLprim value lwt_unix_term_size(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(result);
  HANDLE handle;
  CONSOLE_SCREEN_BUFFER_INFO info;

  handle = GetStdHandle(STD_OUTPUT_HANDLE);
  if (handle == INVALID_HANDLE_VALUE)
    caml_failwith("GetStdHandle");

  if (!GetConsoleScreenBufferInfo(handle, &info))
    caml_failwith("GetConsoleScreenBufferInfo");

  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(info.dwSize.X));
  Store_field(result, 1, Val_int(info.dwSize.Y));
  CAMLreturn(result);
}

#else

#include <sys/ioctl.h>
#include <termios.h>

value lwt_unix_term_size(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(result);
  struct winsize size;
  if (ioctl(STDIN_FILENO, TIOCGWINSZ, &size) < 0)
    caml_failwith("ioctl(TIOCGWINSZ)");

  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(size.ws_row));
  Store_field(result, 1, Val_int(size.ws_col));
  CAMLreturn(result);
}

#endif

value lwt_unix_sigwinch()
{
#ifdef SIGWINCH
  return Val_int(SIGWINCH);
#else
  return Val_int(0);
#endif
}

/* +-----------------------------------------------------------------+
   | wait4                                                           |
   +-----------------------------------------------------------------+ */

#if defined(HAS_WAIT4) && !defined(LWT_WINDOWS)

/* Some code duplicated from OCaml's otherlibs/unix/wait.c */

#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>

CAMLextern int caml_convert_signal_number (int);
CAMLextern int caml_rev_convert_signal_number (int);

#if !(defined(WIFEXITED) && defined(WEXITSTATUS) && defined(WIFSTOPPED) && \
      defined(WSTOPSIG) && defined(WTERMSIG))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status) & 0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#define WIFSTOPPED(status) (((status) & 0xFF) == 0xFF)
#define WSTOPSIG(status) (((status) >> 8) & 0xFF)
#define WTERMSIG(status) ((status) & 0x3F)
#endif

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

static value alloc_process_status(int status)
{
  value st;

  if (WIFEXITED(status)) {
    st = alloc_small(1, TAG_WEXITED);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  }
  else if (WIFSTOPPED(status)) {
    st = alloc_small(1, TAG_WSTOPPED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WSTOPSIG(status)));
  }
  else {
    st = alloc_small(1, TAG_WSIGNALED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WTERMSIG(status)));
  }
  return st;
}

static int wait_flag_table[] = {
  WNOHANG, WUNTRACED
};

value lwt_unix_wait4(value flags, value pid_req)
{
  CAMLparam1(flags);
  CAMLlocal2(times, res);

  int pid, status, cv_flags;
  cv_flags = caml_convert_flag_list(flags, wait_flag_table);

  struct rusage ru;

  caml_enter_blocking_section();
  pid = wait4(Int_val(pid_req), &status, cv_flags, &ru);
  caml_leave_blocking_section();
  if (pid == -1) uerror("wait4", Nothing);

  times = alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(times, 0, ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6);
  Store_double_field(times, 1, ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);

  res = caml_alloc_tuple(3);
  Store_field(res, 0, Val_int(pid));
  Store_field(res, 1, alloc_process_status(status));
  Store_field(res, 2, times);
  CAMLreturn(res);
}

value lwt_unix_has_wait4(value unit)
{
  return Val_int(1);
}

#else

value lwt_unix_wait4(value flags, value pid_req)
{
  invalid_argument("wait4 not implemented");
}

value lwt_unix_has_wait4(value unit)
{
  return Val_int(0);
}

#endif

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

#if defined(LWT_WINDOWS)
HANDLE notification_pipe_mutex;
#else
pthread_mutex_t notification_pipe_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

value lwt_unix_init_notification(value fd)
{
  notification_fd_writer = FD_val(fd);

#if defined(LWT_WINDOWS)
  notification_pipe_mutex = CreateMutex(NULL, FALSE, NULL);
#endif

  return Val_unit;
}

void lwt_unix_send_notification(int id)
{
  char buf[4];
  buf[0] = id;
  buf[1] = id >> 8;
  buf[2] = id >> 16;
  buf[3] = id >> 24;

  caml_enter_blocking_section();

#if defined(LWT_WINDOWS)
  WaitForSingleObject(notification_pipe_mutex, INFINITE);
#else
  pthread_mutex_lock(&notification_pipe_mutex);
#endif

  int offset = 0;
  while (offset < 4) {
    int n = write(notification_fd_writer, &(buf[offset]), 4 - offset);

    if (n <= 0) {
#if defined(LWT_WINDOWS)
      ReleaseMutex(notification_pipe_mutex);
#else
      pthread_mutex_unlock(&notification_pipe_mutex);
#endif
      caml_leave_blocking_section();
      uerror("lwt_unix_send_notification", Nothing);
    }

    offset += n;
  }

#if defined(LWT_WINDOWS)
  ReleaseMutex(notification_pipe_mutex);
#else
  pthread_mutex_unlock(&notification_pipe_mutex);
#endif

  caml_leave_blocking_section();
}

value lwt_unix_send_notification_stub(value id)
{
  lwt_unix_send_notification(Int_val(id));
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Threads                                                         |
   +-----------------------------------------------------------------+ */

#if !defined(X_STACKSIZE)
#  define X_STACKSIZE sizeof (long) * 4096
#endif

#if !defined(LWT_WINDOWS)

#if !defined(PTHREAD_STACK_MIN)
#  define PTHREAD_STACK_MIN 0
#endif

void lwt_unix_launch_thread(void* (*start)(void*), void* data)
{
  pthread_t thread;
  pthread_attr_t attr;

  pthread_attr_init(&attr);

  /* The thread is created in detached state so we do not have to join
     it when it terminates: */
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);

  /* Use the minimum amount of stack: */
  pthread_attr_setstacksize(&attr, PTHREAD_STACK_MIN < X_STACKSIZE ? X_STACKSIZE : PTHREAD_STACK_MIN);

  int res = pthread_create(&thread, &attr, start, data);

  if (res) {
    errno = res;
    uerror("lwt_unix_launch_thread", Nothing);
  }

  pthread_attr_destroy (&attr);
}

#else

void lwt_unix_launch_thread(void* (*start)(void*), void* data)
{
  if (CreateThread(NULL, X_STACKSIZE, (LPTHREAD_START_ROUTINE)start, (LPVOID)data, 0, NULL) == NULL)
    uerror("lwt_unix_launch_thread", Nothing);
}

#endif

/* +-----------------------------------------------------------------+
   | Test for readability/writability                                |
   +-----------------------------------------------------------------+ */

#if !defined(LWT_WINDOWS)

#include <poll.h>

value lwt_unix_readable(value fd)
{
  struct pollfd pollfd;
  pollfd.fd = Int_val(fd);
  pollfd.events = POLLIN;
  pollfd.revents = 0;
  if (poll(&pollfd, 1, 0) < 0)
    uerror("lwt_unix_readable", Nothing);
  return (Val_bool(pollfd.revents & POLLIN));
}

value lwt_unix_writable(value fd)
{
  struct pollfd pollfd;
  pollfd.fd = Int_val(fd);
  pollfd.events = POLLOUT;
  pollfd.revents = 0;
  if (poll(&pollfd, 1, 0) < 0)
    uerror("lwt_unix_readable", Nothing);
  return (Val_bool(pollfd.revents & POLLOUT));
}

#else

value lwt_unix_readable(value fd)
{
  return Val_int(1);
}

value lwt_unix_writable(value fd)
{
  return Val_int(1);
}

#endif
