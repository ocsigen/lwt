/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_stubs
 * Copyright (C) 2009 Jérémie Dimino
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

/* Since version 2.6.22, linux support the [signalfd] system calls
   which create a file descriptor for receiving signals.

   If available we use it because it is the best way to receive
   signals.
*/

#ifdef HAVE_SIGNALFD

#include <signal.h>
#include <sys/signalfd.h>

CAMLextern int caml_convert_signal_number (int);
CAMLextern int caml_rev_convert_signal_number (int);

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

value lwt_unix_term_size()
{
  HANDLE handle;
  CONSOLE_SCREEN_BUFFER_INFO info;

  hConOut = GetStdHandle(STD_OUTPUT_HANDLE);
  if (hConOut == INVALID_HANDLE_VALUE)
    caml_failwith("GetStdHandle");

  if (!GetConsoleScreenBufferInfo(hConOut, &scr))
    caml_failwith("GetConsoleScreenBufferInfo");

  value result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(scr.dwSize.X);
  Field(result, 1) = Val_int(scr.dwSize.Y);
  return result;
}

#else

#include <sys/ioctl.h>
#include <termios.h>

value lwt_unix_term_size()
{
  struct winsize size;
  if (ioctl(STDIN_FILENO, TIOCGWINSZ, &size) < 0)
    caml_failwith("ioctl(TIOCGWINSZ)");

  value result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(size.ws_row);
  Field(result, 1) = Val_int(size.ws_col);
  return result;
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
