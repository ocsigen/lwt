/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_stubs
 * Copyright (C) 2009 Jérémie Dimino
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

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>
#include <caml/config.h>

/* +-----------------------------------------------------------------+
   | Read/write                                                      |
   +-----------------------------------------------------------------+ */

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

/* +-----------------------------------------------------------------+
   | Select                                                          |
   +-----------------------------------------------------------------+ */

#ifdef HAS_SELECT

#include <sys/types.h>
#include <sys/time.h>
#ifdef HAS_SYS_SELECT_H
#include <sys/select.h>
#endif
#include <string.h>
#include <unistd.h>

typedef fd_set file_descr_set;

static void fdlist_to_fdset(value fdlist, fd_set *fdset)
{
  value l;
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    int fd = Int_val(Field(l, 0));
    FD_SET(fd, fdset);
  }
}

static value fdset_to_fdlist(value fdlist, fd_set *fdset)
{
  value l;
  value res = Val_int(0);

  Begin_roots2(l, res);
    for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
      int fd = Int_val(Field(l, 0));
      if (FD_ISSET(fd, fdset)) {
        value newres = alloc_small(2, 0);
        Field(newres, 0) = Val_int(fd);
        Field(newres, 1) = res;
        res = newres;
      }
    }
  End_roots();
  return res;
}

static void fdlist_maxfd(value fdlist, int *maxfd)
{
  value l;
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    int fd = Int_val(Field(l, 0));
    if (fd > *maxfd) *maxfd = fd;
  }
}

CAMLprim value lwt_unix_select(value readfds,
                           value writefds,
                           value exceptfds,
                           value timeout)
{
  int maxfd;
  double tm;
  struct timeval tv;
  struct timeval * tvp;
  int retcode;
  value res;

  Begin_roots3 (readfds, writefds, exceptfds);
    maxfd = -1;
    fdlist_maxfd(readfds, &maxfd);
    fdlist_maxfd(writefds, &maxfd);
    fdlist_maxfd(exceptfds, &maxfd);
    int num_fdset = maxfd / FD_SETSIZE + 1;
    fd_set read[num_fdset], write[num_fdset], except[num_fdset];
    int count = num_fdset * sizeof(fd_set);
    memset(&read, 0, count);
    memset(&write, 0, count);
    memset(&except, 0, count);
    fdlist_to_fdset(readfds, &(read[0]));
    fdlist_to_fdset(writefds, &(write[0]));
    fdlist_to_fdset(exceptfds, &(except[0]));
    tm = Double_val(timeout);
    if (tm < 0.0)
      tvp = (struct timeval *) NULL;
    else {
      tv.tv_sec = (int) tm;
      tv.tv_usec = (int) (1e6 * (tm - tv.tv_sec));
      tvp = &tv;
    }
    enter_blocking_section();
    retcode = select(maxfd + 1, &(read[0]), &(write[0]), &(except[0]), tvp);
    leave_blocking_section();
    if (retcode == -1) uerror("select", Nothing);
    readfds = fdset_to_fdlist(readfds, &(read[0]));
    writefds = fdset_to_fdlist(writefds, &(write[0]));
    exceptfds = fdset_to_fdlist(exceptfds, &(except[0]));
    res = alloc_small(3, 0);
    Field(res, 0) = readfds;
    Field(res, 1) = writefds;
    Field(res, 2) = exceptfds;
  End_roots();
  return res;
}

#else

CAMLprim value lwt_unix_select(value readfds, value writefds, value exceptfds,
                               value timeout)
{ invalid_argument("select not implemented"); }

#endif

/* +-----------------------------------------------------------------+
   | Terminal sizes                                                  |
   +-----------------------------------------------------------------+ */

#if defined(__MINGW32__)

#include <windows.h>
#include <wincon.h>

CAMLprim value lwt_unix_term_size(value unit)
{
  CAMLparam1(unit);
  CAMLlocal(result);
  HANDLE handle;
  CONSOLE_SCREEN_BUFFER_INFO info;

  hConOut = GetStdHandle(STD_OUTPUT_HANDLE);
  if (hConOut == INVALID_HANDLE_VALUE)
    caml_failwith("GetStdHandle");

  if (!GetConsoleScreenBufferInfo(hConOut, &scr))
    caml_failwith("GetConsoleScreenBufferInfo");

  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(scr.dwSize.X));
  Store_field(result, 1, Val_int(scr.dwSize.Y));
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

/* Some code duplicated from OCaml's otherlibs/unix/wait.c */

#include <sys/types.h>
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

#if defined(HAS_WAIT4)

  struct rusage ru;

  caml_enter_blocking_section();
  pid = wait4(Int_val(pid_req), &status, cv_flags, &ru);
  caml_leave_blocking_section();
  if (pid == -1) uerror("wait4", Nothing);

  times = alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(times, 0, ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6);
  Store_double_field(times, 1, ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);

#elif defined(HAS_WAITPID)

  enter_blocking_section();
  pid = waitpid(Int_val(pid_req), &status, cv_flags);
  leave_blocking_section();
  if (pid == -1) uerror("waitpid", Nothing);

  times = alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(times, 0, 0);
  Store_double_field(times, 1, 0);

#else

  caml_invalid_argument("wait4 not implemented");

#endif

  res = caml_alloc_tuple(3);
  Store_field(res, 0, Val_int(pid));
  Store_field(res, 1, alloc_process_status(status));
  Store_field(res, 2, times);
  CAMLreturn(res);
}

value lwt_unix_has_wait4(value unit)
{
#ifdef HAS_WAIT4
  return Val_int(1);
#else
  return Val_int(0);
#endif
}
