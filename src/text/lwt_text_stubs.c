/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_text_stubs
 * Copyright (C) 2011 Jérémie Dimino
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

#include <lwt_config.h>

#if defined(LWT_ON_WINDOWS)
#  include <windows.h>
#  include <wincon.h>
#else
#  include <sys/ioctl.h>
#  include <termios.h>
#  include <errno.h>
#  include <signal.h>
#endif

#include <lwt_unix.h>

#include <caml/alloc.h>
#include <caml/fail.h>

/* +-----------------------------------------------------------------+
   | Terminal sizes                                                  |
   +-----------------------------------------------------------------+ */

#if defined(LWT_ON_WINDOWS)

CAMLprim value lwt_text_term_size(value fd)
{
  HANDLE handle;
  CONSOLE_SCREEN_BUFFER_INFO info;

  if (!GetConsoleScreenBufferInfo(Handle_val(fd), &info)) {
    win32_maperr(GetLastError());
    uerror("GetConsoleScreenBufferInfo", Nothing);
  }

  value result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(info.dwSize.X);
  Field(result, 1) = Val_int(info.dwSize.Y);
  return result;
}

#else

CAMLprim value lwt_text_term_size(value fd)
{
  struct winsize size;

  if (ioctl(Int_val(fd), TIOCGWINSZ, &size) < 0)
    uerror("ioctl", Nothing);

  value result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(size.ws_row);
  Field(result, 1) = Val_int(size.ws_col);
  return result;
}

CAMLprim value lwt_text_sigwinch()
{
#ifdef SIGWINCH
  return Val_int(SIGWINCH);
#else
  return Val_int(0);
#endif
}

#endif
