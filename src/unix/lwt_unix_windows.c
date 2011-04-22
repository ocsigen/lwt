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

CAMLprim value lwt_unix_get_page_size()
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
  char *buffer;
  DWORD length;
  DWORD result;
  DWORD error_code;
};

#define Job_read_val(v) *(struct job_read**)Data_custom_val(v)

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

CAMLprim value lwt_unix_read_job(value val_fd, value val_length)
{
  struct job_read *job = lwt_unix_new(struct job_read);
  struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
  long length = Long_val(val_length);
  job->job.worker = (lwt_unix_job_worker)worker_read;
  job->kind = fd->kind;
  if (fd->kind == KIND_HANDLE)
    job->fd.handle = fd->fd.handle;
  else
    job->fd.socket = fd->fd.socket;
  job->buffer = (char*)lwt_unix_malloc(length);
  job->length = length;
  job->error_code = 0;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_read_result(value val_job, value val_string, value val_offset)
{
  struct job_read *job = Job_read_val(val_job);
  if (job->error_code) {
    win32_maperr(job->error_code);
    uerror("read", Nothing);
  }
  memcpy(String_val(val_string) + Long_val(val_offset), job->buffer, job->result);
  return Val_long(job->result);
}

CAMLprim value lwt_unix_read_free(value val_job)
{
  struct job_read *job = Job_read_val(val_job);
  free(job->buffer);
  lwt_unix_free_job(&job->job);
  return Val_unit;
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

#define Job_bytes_read_val(v) *(struct job_bytes_read**)Data_custom_val(v)

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

CAMLprim value lwt_unix_bytes_read_job(value val_fd, value val_buffer, value val_offset, value val_length)
{
  struct job_bytes_read *job = lwt_unix_new(struct job_bytes_read);
  struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
  long length = Long_val(val_length);
  job->job.worker = (lwt_unix_job_worker)worker_bytes_read;
  job->kind = fd->kind;
  if (fd->kind == KIND_HANDLE)
    job->fd.handle = fd->fd.handle;
  else
    job->fd.socket = fd->fd.socket;
  job->buffer = (char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
  job->length = length;
  job->error_code = 0;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_bytes_read_result(value val_job)
{
  struct job_bytes_read *job = Job_bytes_read_val(val_job);
  if (job->error_code) {
    win32_maperr(job->error_code);
    uerror("bytes_read", Nothing);
  }
  return Val_long(job->result);
}

CAMLprim value lwt_unix_bytes_read_free(value val_job)
{
  struct job_bytes_read *job = Job_bytes_read_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
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
  char *buffer;
  DWORD length;
  DWORD result;
  DWORD error_code;
};

#define Job_write_val(v) *(struct job_write**)Data_custom_val(v)

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

CAMLprim value lwt_unix_write_job(value val_fd, value val_string, value val_offset, value val_length)
{
  struct job_write *job = lwt_unix_new(struct job_write);
  struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
  long length = Long_val(val_length);
  job->job.worker = (lwt_unix_job_worker)worker_write;
  job->kind = fd->kind;
  if (fd->kind == KIND_HANDLE)
    job->fd.handle = fd->fd.handle;
  else
    job->fd.socket = fd->fd.socket;
  job->buffer = (char*)lwt_unix_malloc(length);
  memcpy(job->buffer, String_val(val_string) + Long_val(val_offset), length);
  job->length = length;
  job->error_code = 0;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_write_result(value val_job)
{
  struct job_write *job = Job_write_val(val_job);
  if (job->error_code) {
    win32_maperr(job->error_code);
    uerror("write", Nothing);
  }
  return Val_long(job->result);
}

CAMLprim value lwt_unix_write_free(value val_job)
{
  struct job_write *job = Job_write_val(val_job);
  free(job->buffer);
  lwt_unix_free_job(&job->job);
  return Val_unit;
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

#define Job_bytes_write_val(v) *(struct job_bytes_write**)Data_custom_val(v)

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

CAMLprim value lwt_unix_bytes_write_job(value val_fd, value val_buffer, value val_offset, value val_length)
{
  struct job_bytes_write *job = lwt_unix_new(struct job_bytes_write);
  struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
  long length = Long_val(val_length);
  job->job.worker = (lwt_unix_job_worker)worker_bytes_write;
  job->kind = fd->kind;
  if (fd->kind == KIND_HANDLE)
    job->fd.handle = fd->fd.handle;
  else
    job->fd.socket = fd->fd.socket;
  job->buffer = (char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
  job->length = length;
  job->error_code = 0;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_bytes_write_result(value val_job)
{
  struct job_bytes_write *job = Job_bytes_write_val(val_job);
  if (job->error_code) {
    win32_maperr(job->error_code);
    uerror("bytes_write", Nothing);
  }
  return Val_long(job->result);
}

CAMLprim value lwt_unix_bytes_write_free(value val_job)
{
  struct job_bytes_write *job = Job_bytes_write_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: fsync                                                      |
   +-----------------------------------------------------------------+ */

struct job_fsync {
  struct lwt_unix_job job;
  HANDLE handle;
  DWORD error_code;
};

#define Job_fsync_val(v) *(struct job_fsync**)Data_custom_val(v)

static void worker_fsync(struct job_fsync *job)
{
  if (!FlushFileBuffers(job->handle))
    job->error_code = GetLastError();
}

CAMLprim value lwt_unix_fsync_job(value val_fd)
{
  struct job_fsync *job = lwt_unix_new(struct job_fsync);
  struct filedescr *fd = (struct filedescr *)Data_custom_val(val_fd);
  job->job.worker = (lwt_unix_job_worker)worker_fsync;
  job->handle = fd->fd.handle;
  job->error_code = 0;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_fsync_result(value val_job, value val_string, value val_offset)
{
  struct job_fsync *job = Job_fsync_val(val_job);
  if (job->error_code) {
    win32_maperr(job->error_code);
    uerror("fsync", Nothing);
  }
  return Val_unit;
}

CAMLprim value lwt_unix_fsync_free(value val_job)
{
  struct job_fsync *job = Job_fsync_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}
