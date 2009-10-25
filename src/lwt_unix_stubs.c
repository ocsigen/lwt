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
#include <signal.h>

#include "config.h"

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
   | Signals                                                         |
   +-----------------------------------------------------------------+ */

CAMLextern int caml_convert_signal_number (int);
CAMLextern int caml_rev_convert_signal_number (int);

/* Since version 2.6.22, linux support the [signalfd] system calls
   which create a file descriptor for receiving signals.

   If available we use it because it is the best way to receive
   signals.
*/

#ifdef HAVE_SIGNALFD

#include <signal.h>
#include <sys/signalfd.h>

static sigset_t mask;
static int sfd = -1;

value lwt_signalfd_available()
{
  return Val_true;
}

value lwt_signalfd_size()
{
  return Val_int(sizeof(struct signalfd_siginfo));
}

value lwt_signalfd_init()
{
  sigemptyset(&mask);
  sfd = signalfd(-1, &mask, 0);
  if (sfd < 0)
    uerror("signalfd", Nothing);
  return Val_int(sfd);
}

void lwt_signalfd_add(value signum)
{
  sigaddset(&mask, caml_convert_signal_number(Int_val(signum)));
  if (sigprocmask(SIG_BLOCK, &mask, NULL) < 0)
    uerror("sigprocmask", Nothing);
  if (signalfd(sfd, &mask, 0) < 0)
    uerror("signalfd", Nothing);
}

void lwt_signalfd_del(value signum)
{
  sigdelset(&mask, caml_convert_signal_number(Int_val(signum)));
  if (signalfd(sfd, &mask, 0) < 0)
    uerror("signalfd", Nothing);
  sigset_t del_mask;
  sigemptyset(&del_mask);
  sigaddset(&del_mask, Int_val(signum));
  if (sigprocmask(SIG_UNBLOCK, &mask, NULL) < 0)
    uerror("sigprocmask", Nothing);
}

value lwt_signalfd_read(value buffer)
{
  struct signalfd_siginfo *info = (struct signalfd_siginfo*)String_val(buffer);
  return Val_int(caml_rev_convert_signal_number(info->ssi_signo));
}

#else

value lwt_signalfd_available()
{
  return Val_false;
}

#define FAKE(name)                              \
  void lwt_signalfd_##name()                    \
  {                                             \
    invalid_argument("not implmented");         \
  }

FAKE(size)
FAKE(init)
FAKE(add)
FAKE(del)
FAKE(read)

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
#include <caml/config.h>
#include <caml/signals.h>
#include <stdio.h>

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
