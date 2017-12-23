/* OCaml promise library
 * http://www.ocsigen.org/lwt
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
    if (!FlushFileBuffers(job->handle)) job->error_code = GetLastError();
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
    if (!CreateProcess(NULL, String_val(cmdline), NULL, NULL, TRUE, 0, NULL,
                       NULL, &si, &pi)) {
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

