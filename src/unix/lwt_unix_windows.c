/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_unix
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

/* Windows version of stubs. */

CAMLprim value lwt_unix_write()
{
  caml_invalid_argument("not implemented");
}

CAMLprim value lwt_unix_bytes_write()
{
  caml_invalid_argument("not implemented");
}

CAMLprim value lwt_unix_read()
{
  caml_invalid_argument("not implemented");
}

CAMLprim value lwt_unix_bytes_read()
{
  caml_invalid_argument("not implemented");
}

value lwt_unix_recv()
{
  caml_invalid_argument("recv not implemented");
}

value lwt_unix_bytes_recv()
{
  caml_invalid_argument("recv not implemented");
}

value lwt_unix_recvfrom()
{
  caml_invalid_argument("recvfrom not implemented");
}

value lwt_unix_bytes_recvfrom()
{
  caml_invalid_argument("recvfrom not implemented");
}

value lwt_unix_send()
{
  caml_invalid_argument("send not implemented");
}

value lwt_unix_bytes_send()
{
  caml_invalid_argument("send not implemented");
}

value lwt_unix_sendto()
{
  caml_invalid_argument("sendto not implemented");
}

value lwt_unix_sendto_byte()
{
  caml_invalid_argument("sendto not implemented");
}

value lwt_unix_bytes_sendto()
{
  caml_invalid_argument("sendto not implemented");
}

value lwt_unix_bytes_sendto_byte()
{
  caml_invalid_argument("sendto not implemented");
}

value lwt_unix_recv_msg(value sock_val, value n_iovs_val, value iovs_val)
{
  caml_invalid_argument("recv_msg not implemented");
}

value lwt_unix_bytes_recv_msg(value sock_val, value n_iovs_val, value iovs_val)
{
  caml_invalid_argument("recv_msg not implemented");
}

value lwt_unix_send_msg(value sock_val, value n_iovs_val, value iovs_val, value n_fds_val, value fds_val)
{
  caml_invalid_argument("send_msg not implemented");
}

value lwt_unix_bytes_send_msg(value sock_val, value n_iovs_val, value iovs_val, value n_fds_val, value fds_val)
{
  caml_invalid_argument("send_msg not implemented");
}

CAMLprim value lwt_unix_get_credentials(value fd_val)
{
  caml_invalid_argument("get_credentials not implemented");
}

value lwt_unix_wait4(value flags, value pid_req)
{
  caml_invalid_argument("wait4 not implemented");
}

value lwt_unix_has_wait4(value unit)
{
  return Val_int(0);
}

CAMLprim value lwt_unix_readable(value fd)
{
  return Val_int(1);
}

CAMLprim value lwt_unix_writable(value fd)
{
  return Val_int(1);
}

/* +-----------------------------------------------------------------+
   | Memory mapped files                                             |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_unix_madvise (value val_buffer, value val_offset, value val_length, value val_advice)
{
  return Val_unit;
}

CAMLprim value lwt_unix_get_page_size()
{
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return Val_long(si.dwPageSize);
}

CAMLprim value lwt_unix_mincore(value val_buffer, value val_offset, value val_length, value val_states)
{
  long len = Wosize_val(val_states);
  long i;
  for (i = 0; i < len; i++)
    Field(val_states, i) = Val_true;
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Terminal sizes                                                  |
   +-----------------------------------------------------------------+ */

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

value lwt_unix_sigwinch()
{
#ifdef SIGWINCH
  return Val_int(SIGWINCH);
#else
  return Val_int(0);
#endif
}

/* +-----------------------------------------------------------------+
   | CPUs                                                            |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_unix_get_cpu()
{
  caml_invalid_argument("not implemented");
}

CAMLprim value lwt_unix_get_affinity(value val_pid)
{
  caml_invalid_argument("not implemented");
}

CAMLprim value lwt_unix_set_affinity(value val_pid, value val_cpus)
{
  caml_invalid_argument("not implemented");
}

/* +-----------------------------------------------------------------+
   | Jobs                                                            |
   +-----------------------------------------------------------------+ */

LWT_UNIX_JOB_NOT_IMPLEMENTED(guess_blocking)
LWT_UNIX_JOB_NOT_IMPLEMENTED(wait_mincore)
LWT_UNIX_JOB_NOT_IMPLEMENTED(open)
LWT_UNIX_JOB_NOT_IMPLEMENTED(close)
LWT_UNIX_JOB_NOT_IMPLEMENTED(read)
LWT_UNIX_JOB_NOT_IMPLEMENTED(bytes_read)
LWT_UNIX_JOB_NOT_IMPLEMENTED(write)
LWT_UNIX_JOB_NOT_IMPLEMENTED(bytes_write)
LWT_UNIX_JOB_NOT_IMPLEMENTED(lseek)
LWT_UNIX_JOB_NOT_IMPLEMENTED(truncate)
LWT_UNIX_JOB_NOT_IMPLEMENTED(ftruncate)
LWT_UNIX_JOB_NOT_IMPLEMENTED(stat)
LWT_UNIX_JOB_NOT_IMPLEMENTED(lstat)
LWT_UNIX_JOB_NOT_IMPLEMENTED(fstat)
LWT_UNIX_JOB_NOT_IMPLEMENTED(isatty)
LWT_UNIX_JOB_NOT_IMPLEMENTED(lseek_64)
LWT_UNIX_JOB_NOT_IMPLEMENTED(truncate_64)
LWT_UNIX_JOB_NOT_IMPLEMENTED(ftruncate_64)
LWT_UNIX_JOB_NOT_IMPLEMENTED(stat_64)
LWT_UNIX_JOB_NOT_IMPLEMENTED(lstat_64)
LWT_UNIX_JOB_NOT_IMPLEMENTED(fstat_64)
LWT_UNIX_JOB_NOT_IMPLEMENTED(unlink)
LWT_UNIX_JOB_NOT_IMPLEMENTED(rename)
LWT_UNIX_JOB_NOT_IMPLEMENTED(link)
LWT_UNIX_JOB_NOT_IMPLEMENTED(chmod)
LWT_UNIX_JOB_NOT_IMPLEMENTED(fchmod)
LWT_UNIX_JOB_NOT_IMPLEMENTED(chown)
LWT_UNIX_JOB_NOT_IMPLEMENTED(fchown)
LWT_UNIX_JOB_NOT_IMPLEMENTED(access)
LWT_UNIX_JOB_NOT_IMPLEMENTED(mkdir)
LWT_UNIX_JOB_NOT_IMPLEMENTED(rmdir)
LWT_UNIX_JOB_NOT_IMPLEMENTED(chdir)
LWT_UNIX_JOB_NOT_IMPLEMENTED(chroot)
LWT_UNIX_JOB_NOT_IMPLEMENTED(opendir)
LWT_UNIX_JOB_NOT_IMPLEMENTED(readdir)
LWT_UNIX_JOB_NOT_IMPLEMENTED(readdir_n)
LWT_UNIX_JOB_NOT_IMPLEMENTED(rewinddir)
LWT_UNIX_JOB_NOT_IMPLEMENTED(closedir)
LWT_UNIX_JOB_NOT_IMPLEMENTED(mkfifo)
LWT_UNIX_JOB_NOT_IMPLEMENTED(symlink)
LWT_UNIX_JOB_NOT_IMPLEMENTED(readlink)
LWT_UNIX_JOB_NOT_IMPLEMENTED(lockf)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getlogin)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getpwnam)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getgrnam)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getpwuid)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getgrgid)
LWT_UNIX_JOB_NOT_IMPLEMENTED(gethostname)
LWT_UNIX_JOB_NOT_IMPLEMENTED(gethostbyname)
LWT_UNIX_JOB_NOT_IMPLEMENTED(gethostbyaddr)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getprotobyname)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getprotobynumber)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getservbyname)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getservbyport)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getaddrinfo)
LWT_UNIX_JOB_NOT_IMPLEMENTED(getnameinfo)
LWT_UNIX_JOB_NOT_IMPLEMENTED(tcgetattr)
LWT_UNIX_JOB_NOT_IMPLEMENTED(tcsetattr)
LWT_UNIX_JOB_NOT_IMPLEMENTED(tcsendbreak)
LWT_UNIX_JOB_NOT_IMPLEMENTED(tcdrain)
LWT_UNIX_JOB_NOT_IMPLEMENTED(tcflush)
LWT_UNIX_JOB_NOT_IMPLEMENTED(tcflow)
