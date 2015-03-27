/* Lightweight thread library for OCaml
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

CAMLprim value lwt_unix_is_socket(value fd)
{
  return (Val_bool(Descr_kind_val(fd) == KIND_SOCKET));
}

CAMLprim value lwt_unix_write(value fd, value buf, value vofs, value vlen)
{
  intnat ofs, len, written;
  DWORD numbytes, numwritten;
  DWORD err = 0;

  Begin_root (buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    written = 0;
    if (len > 0) {
      numbytes = len;
      if (Descr_kind_val(fd) == KIND_SOCKET) {
        int ret;
        SOCKET s = Socket_val(fd);
        ret = send(s, &Byte(buf, ofs), numbytes, 0);
        if (ret == SOCKET_ERROR) err = WSAGetLastError();
        numwritten = ret;
      } else {
        HANDLE h = Handle_val(fd);
        if (! WriteFile(h, &Byte(buf, ofs), numbytes, &numwritten, NULL))
          err = GetLastError();
      }
      if (err) {
        win32_maperr(err);
        uerror("write", Nothing);
      }
      written = numwritten;
    }
  End_roots();
  return Val_long(written);
}

CAMLprim value lwt_unix_bytes_write(value fd, value buf, value vofs, value vlen)
{
  intnat ofs, len, written;
  DWORD numbytes, numwritten;
  DWORD err = 0;

  Begin_root (buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    written = 0;
    if (len > 0) {
      numbytes = len;
      if (Descr_kind_val(fd) == KIND_SOCKET) {
        int ret;
        SOCKET s = Socket_val(fd);
        ret = send(s, (char*)Caml_ba_array_val(buf)->data + ofs, numbytes, 0);
        if (ret == SOCKET_ERROR) err = WSAGetLastError();
        numwritten = ret;
      } else {
        HANDLE h = Handle_val(fd);
        if (! WriteFile(h, (char*)Caml_ba_array_val(buf)->data + ofs, numbytes, &numwritten, NULL))
          err = GetLastError();
      }
      if (err) {
        win32_maperr(err);
        uerror("write", Nothing);
      }
      written = numwritten;
    }
  End_roots();
  return Val_long(written);
}

CAMLprim value lwt_unix_read(value fd, value buf, value vofs, value vlen)
{
  intnat ofs, len, written;
  DWORD numbytes, numwritten;
  DWORD err = 0;

  Begin_root (buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    written = 0;
    if (len > 0) {
      numbytes = len;
      if (Descr_kind_val(fd) == KIND_SOCKET) {
        int ret;
        SOCKET s = Socket_val(fd);
        ret = recv(s, &Byte(buf, ofs), numbytes, 0);
        if (ret == SOCKET_ERROR) err = WSAGetLastError();
        numwritten = ret;
      } else {
        HANDLE h = Handle_val(fd);
        if (! ReadFile(h, &Byte(buf, ofs), numbytes, &numwritten, NULL))
          err = GetLastError();
      }
      if (err) {
        win32_maperr(err);
        uerror("write", Nothing);
      }
      written = numwritten;
    }
  End_roots();
  return Val_long(written);
}

CAMLprim value lwt_unix_bytes_read(value fd, value buf, value vofs, value vlen)
{
  intnat ofs, len, written;
  DWORD numbytes, numwritten;
  DWORD err = 0;

  Begin_root (buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    written = 0;
    if (len > 0) {
      numbytes = len;
      if (Descr_kind_val(fd) == KIND_SOCKET) {
        int ret;
        SOCKET s = Socket_val(fd);
        ret = recv(s, (char*)Caml_ba_array_val(buf)->data + ofs, numbytes, 0);
        if (ret == SOCKET_ERROR) err = WSAGetLastError();
        numwritten = ret;
      } else {
        HANDLE h = Handle_val(fd);
        if (! ReadFile(h, (char*)Caml_ba_array_val(buf)->data + ofs, numbytes, &numwritten, NULL))
          err = GetLastError();
      }
      if (err) {
        win32_maperr(err);
        uerror("write", Nothing);
      }
      written = numwritten;
    }
  End_roots();
  return Val_long(written);
}

/* +-----------------------------------------------------------------+
   | Memory mapped files                                             |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_unix_get_page_size(value Unit)
{
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return Val_long(si.dwPageSize);
}

/* +-----------------------------------------------------------------+
   | JOB: read                                                       |
   +-----------------------------------------------------------------+ */

struct job_read {
  struct lwt_unix_job job;
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;
  int kind;
  DWORD length;
  DWORD result;
  DWORD error_code;
  value string;
  DWORD offset;
  char buffer[];
};

static void worker_read(struct job_read *job)
{
  if (job->kind == KIND_SOCKET) {
    int ret;
    ret = recv(job->fd.socket, job->buffer, job->length, 0);
    if (ret == SOCKET_ERROR) job->error_code = WSAGetLastError();
    job->result = ret;
  } else {
    if (!ReadFile(job->fd.handle, job->buffer, job->length, &(job->result), NULL))
      job->error_code = GetLastError();
  }
}

static value result_read(struct job_read *job)
{
  value result;
  DWORD error = job->error_code;
  if (error) {
    caml_remove_generational_global_root(&job->string);
    lwt_unix_free_job(&job->job);
    win32_maperr(error);
    uerror("read", Nothing);
  }
  memcpy(String_val(job->string) + job->offset, job->buffer, job->result);
  result = Val_long(job->result);
  caml_remove_generational_global_root(&job->string);
  lwt_unix_free_job(&job->job);
  return result;
}

CAMLprim value lwt_unix_read_job(value val_fd, value val_string, value val_offset, value val_length)
{
  struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
  long length = Long_val(val_length);
  LWT_UNIX_INIT_JOB(job, read, length);
  job->kind = fd->kind;
  if (fd->kind == KIND_HANDLE)
    job->fd.handle = fd->fd.handle;
  else
    job->fd.socket = fd->fd.socket;
  job->length = length;
  job->error_code = 0;
  job->string = val_string;
  job->offset = Long_val(val_offset);
  caml_register_generational_global_root(&(job->string));
  return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: bytes_read                                                 |
   +-----------------------------------------------------------------+ */

struct job_bytes_read {
  struct lwt_unix_job job;
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;
  int kind;
  char *buffer;
  DWORD length;
  DWORD result;
  DWORD error_code;
};

static void worker_bytes_read(struct job_bytes_read *job)
{
  if (job->kind == KIND_SOCKET) {
    int ret;
    ret = recv(job->fd.socket, job->buffer, job->length, 0);
    if (ret == SOCKET_ERROR) job->error_code = WSAGetLastError();
    job->result = ret;
  } else {
    if (!ReadFile(job->fd.handle, job->buffer, job->length, &(job->result), NULL))
      job->error_code = GetLastError();
  }
}

static value result_bytes_read(struct job_bytes_read *job)
{
  value result;
  DWORD error = job->error_code;
  if (error) {
    lwt_unix_free_job(&job->job);
    win32_maperr(error);
    uerror("bytes_read", Nothing);
  }
  result = Val_long(job->result);
  lwt_unix_free_job(&job->job);
  return result;
}

CAMLprim value lwt_unix_bytes_read_job(value val_fd, value val_buffer, value val_offset, value val_length)
{
  struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
  LWT_UNIX_INIT_JOB(job, bytes_read, 0);
  job->kind = fd->kind;
  if (fd->kind == KIND_HANDLE)
    job->fd.handle = fd->fd.handle;
  else
    job->fd.socket = fd->fd.socket;
  job->buffer = (char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
  job->length = Long_val(val_length);
  job->error_code = 0;
  return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: write                                                      |
   +-----------------------------------------------------------------+ */

struct job_write {
  struct lwt_unix_job job;
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;
  int kind;
  DWORD length;
  DWORD result;
  DWORD error_code;
  char buffer[];
};

static void worker_write(struct job_write *job)
{
  if (job->kind == KIND_SOCKET) {
    int ret;
    ret = send(job->fd.socket, job->buffer, job->length, 0);
    if (ret == SOCKET_ERROR) job->error_code = WSAGetLastError();
    job->result = ret;
  } else {
    if (!WriteFile(job->fd.handle, job->buffer, job->length, &(job->result), NULL))
      job->error_code = GetLastError();
  }
}

static value result_write(struct job_write *job)
{
  value result;
  DWORD error = job->error_code;
  if (error) {
    lwt_unix_free_job(&job->job);
    win32_maperr(error);
    uerror("write", Nothing);
  }
  result = Val_long(job->result);
  lwt_unix_free_job(&job->job);
  return result;
}

CAMLprim value lwt_unix_write_job(value val_fd, value val_string, value val_offset, value val_length)
{
  struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
  long length = Long_val(val_length);
  LWT_UNIX_INIT_JOB(job, write, length);
  job->kind = fd->kind;
  if (fd->kind == KIND_HANDLE)
    job->fd.handle = fd->fd.handle;
  else
    job->fd.socket = fd->fd.socket;
  memcpy(job->buffer, String_val(val_string) + Long_val(val_offset), length);
  job->length = length;
  job->error_code = 0;
  return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: bytes_write                                                |
   +-----------------------------------------------------------------+ */

struct job_bytes_write {
  struct lwt_unix_job job;
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;
  int kind;
  char *buffer;
  DWORD length;
  DWORD result;
  DWORD error_code;
};

static void worker_bytes_write(struct job_bytes_write *job)
{
  if (job->kind == KIND_SOCKET) {
    int ret;
    ret = send(job->fd.socket, job->buffer, job->length, 0);
    if (ret == SOCKET_ERROR) job->error_code = WSAGetLastError();
    job->result = ret;
  } else {
    if (!WriteFile(job->fd.handle, job->buffer, job->length, &(job->result), NULL))
      job->error_code = GetLastError();
  }
}

CAMLprim value result_bytes_write(struct job_bytes_write *job)
{
  value result;
  DWORD error = job->error_code;
  if (error) {
    lwt_unix_free_job(&job->job);
    win32_maperr(error);
    uerror("bytes_write", Nothing);
  }
  result = Val_long(job->result);
  lwt_unix_free_job(&job->job);
  return result;
}

CAMLprim value lwt_unix_bytes_write_job(value val_fd, value val_buffer, value val_offset, value val_length)
{
  struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
  LWT_UNIX_INIT_JOB(job, bytes_write, 0);
  job->job.worker = (lwt_unix_job_worker)worker_bytes_write;
  job->kind = fd->kind;
  if (fd->kind == KIND_HANDLE)
    job->fd.handle = fd->fd.handle;
  else
    job->fd.socket = fd->fd.socket;
  job->buffer = (char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
  job->length = Long_val(val_length);
  job->error_code = 0;
  return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: fsync                                                      |
   +-----------------------------------------------------------------+ */

struct job_fsync {
  struct lwt_unix_job job;
  HANDLE handle;
  DWORD error_code;
};

static void worker_fsync(struct job_fsync *job)
{
  if (!FlushFileBuffers(job->handle))
    job->error_code = GetLastError();
}

static value result_fsync(struct job_fsync *job)
{
  DWORD error = job->error_code;
  if (error) {
    lwt_unix_free_job(&job->job);
    win32_maperr(error);
    uerror("fsync", Nothing);
  }
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

CAMLprim value lwt_unix_fsync_job(value val_fd)
{
  struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
  if (fd->kind != KIND_HANDLE) {
    caml_invalid_argument("Lwt_unix.fsync");
  } else {
    LWT_UNIX_INIT_JOB(job, fsync, 0);
    job->handle = fd->fd.handle;
    job->error_code = 0;
    return lwt_unix_alloc_job(&(job->job));
  }
}

/* +-----------------------------------------------------------------+
   | JOB: system                                                     |
   +-----------------------------------------------------------------+ */

struct job_system {
  struct lwt_unix_job job;
  HANDLE handle;
};

static void worker_system(struct job_system *job)
{
  WaitForSingleObject(job->handle, INFINITE);
}

static value result_system(struct job_system *job)
{
  HANDLE handle = job->handle;
  DWORD code;
  DWORD err;
  lwt_unix_free_job(&job->job);
  if (!GetExitCodeProcess(handle, &code)) {
    err = GetLastError();
    CloseHandle(handle);
    win32_maperr(err);
    uerror("GetExitCodeProcess", Nothing);
  }
  CloseHandle(handle);
  return Val_int(code);
}

CAMLprim value lwt_unix_system_job(value cmdline)
{
  STARTUPINFO si;
  PROCESS_INFORMATION pi;

  ZeroMemory(&si, sizeof(si));
  ZeroMemory(&pi, sizeof(pi));
  si.cb = sizeof(si);
  if (!CreateProcess(NULL, String_val(cmdline), NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
    win32_maperr(GetLastError());
    uerror("CreateProcess", Nothing);
  } else {
    LWT_UNIX_INIT_JOB(job, system, 0);
    CloseHandle(pi.hThread);
    job->handle = pi.hProcess;
    return lwt_unix_alloc_job(&(job->job));
  }
}

/* +-----------------------------------------------------------------+
   | Unavailable primitives                                          |
   +-----------------------------------------------------------------+ */

LWT_NOT_AVAILABLE2(unix_mcast_set_loop)
LWT_NOT_AVAILABLE2(unix_mcast_set_ttl)
LWT_NOT_AVAILABLE4(unix_mcast_modify_membership)
LWT_NOT_AVAILABLE1(unix_get_credentials)
LWT_NOT_AVAILABLE1(unix_get_cpu)
LWT_NOT_AVAILABLE1(unix_get_affinity)
LWT_NOT_AVAILABLE2(unix_set_affinity)
LWT_NOT_AVAILABLE5(unix_bytes_recv)
LWT_NOT_AVAILABLE5(unix_bytes_send)
LWT_NOT_AVAILABLE3(unix_bytes_recv_msg)
LWT_NOT_AVAILABLE3(unix_bytes_send_msg)
LWT_NOT_AVAILABLE6(unix_bytes_sendto)
LWT_NOT_AVAILABLE6(unix_bytes_sendto_byte)
LWT_NOT_AVAILABLE6(unix_madvise)
LWT_NOT_AVAILABLE4(unix_mincore)
LWT_NOT_AVAILABLE2(unix_wait_mincore_job)
LWT_NOT_AVAILABLE1(unix_guess_blocking_job)
LWT_NOT_AVAILABLE1(unix_readable)
LWT_NOT_AVAILABLE1(unix_writable)
LWT_NOT_AVAILABLE1(unix_open_job)
LWT_NOT_AVAILABLE1(unix_stat_job)
LWT_NOT_AVAILABLE1(unix_lstat_job)
LWT_NOT_AVAILABLE1(unix_fstat_job)
LWT_NOT_AVAILABLE1(unix_isatty_job)
LWT_NOT_AVAILABLE1(unix_stat_64_job)
LWT_NOT_AVAILABLE1(unix_lstat_64_job)
LWT_NOT_AVAILABLE1(unix_fstat_64_job)
LWT_NOT_AVAILABLE1(unix_opendir_job)
LWT_NOT_AVAILABLE1(unix_readdir_job)
LWT_NOT_AVAILABLE2(unix_readdir_n_job)
LWT_NOT_AVAILABLE1(unix_rewinddir_job)
LWT_NOT_AVAILABLE1(unix_closedir_job)
LWT_NOT_AVAILABLE1(unix_readlink_job)
LWT_NOT_AVAILABLE3(unix_lockf_job)
LWT_NOT_AVAILABLE5(unix_recv)
LWT_NOT_AVAILABLE5(unix_send)
LWT_NOT_AVAILABLE5(unix_recvfrom)
LWT_NOT_AVAILABLE6(unix_sendto)
LWT_NOT_AVAILABLE6(unix_sendto_byte)
LWT_NOT_AVAILABLE3(unix_recv_msg)
LWT_NOT_AVAILABLE3(unix_send_msg)
LWT_NOT_AVAILABLE1(unix_gethostname_job)
LWT_NOT_AVAILABLE1(unix_gethostbyname_job)
LWT_NOT_AVAILABLE1(unix_gethostbyaddr_job)
LWT_NOT_AVAILABLE1(unix_getprotobyname_job)
LWT_NOT_AVAILABLE1(unix_getprotobynumber_job)
LWT_NOT_AVAILABLE2(unix_getservbyname_job)
LWT_NOT_AVAILABLE2(unix_getservbyport_job)
LWT_NOT_AVAILABLE3(unix_getaddrinfo_job)
LWT_NOT_AVAILABLE2(unix_getnameinfo_job)
LWT_NOT_AVAILABLE1(unix_tcgetattr_job)
LWT_NOT_AVAILABLE3(unix_tcsetattr_job)
LWT_NOT_AVAILABLE1(unix_getlogin_job)
LWT_NOT_AVAILABLE1(unix_getpwnam_job)
LWT_NOT_AVAILABLE1(unix_getgrnam_job)
LWT_NOT_AVAILABLE1(unix_getpwuid_job)
LWT_NOT_AVAILABLE1(unix_getgrgid_job)
LWT_NOT_AVAILABLE4(unix_wait4)
LWT_NOT_AVAILABLE5(unix_bytes_recvfrom)
