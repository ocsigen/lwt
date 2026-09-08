/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */

#include "lwt_config.h"

#if defined(LWT_ON_WINDOWS)

#if OCAML_VERSION < 41300
#define CAML_INTERNALS
#endif
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#include "lwt_unix.h"

struct job_waitpid {
    struct lwt_unix_job job;
    HANDLE handle;
    DWORD exit_code;
    DWORD error_code;
    DWORD wait_result;
    int pid;
    int use_wnohang;
};

static void worker_waitpid(struct job_waitpid *job)
{
    DWORD timeout = job->use_wnohang ? 0 : INFINITE;

    job->wait_result = WaitForSingleObject(job->handle, timeout);
    if (job->wait_result == WAIT_OBJECT_0) {
        if (!GetExitCodeProcess(job->handle, &job->exit_code)) {
            job->error_code = GetLastError();
        } else {
            job->error_code = ERROR_SUCCESS;
        }
    } else if (job->wait_result == WAIT_TIMEOUT) {
        /* WNOHANG case: process still running */
        job->error_code = ERROR_SUCCESS;
    } else {
        job->error_code = GetLastError();
    }
}

/* Helper to allocate Unix.WEXITED(code) */
static value alloc_wexited(int code)
{
    value status = caml_alloc_small(1, 0); /* Tag 0 = WEXITED */
    Field(status, 0) = Val_int(code);
    return status;
}

static value result_waitpid(struct job_waitpid *job)
{
    CAMLparam0();
    CAMLlocal2(result, status);
    DWORD exit_code = job->exit_code;
    DWORD error_code = job->error_code;
    DWORD wait_result = job->wait_result;
    int pid = job->pid;
    HANDLE handle = job->handle;

    lwt_unix_free_job(&job->job);

    if (error_code != ERROR_SUCCESS) {
        CloseHandle(handle);
        win32_maperr(error_code);
        uerror("waitpid", Nothing);
    }

    if (wait_result == WAIT_TIMEOUT) {
        /* WNOHANG: process still running, return (0, WEXITED 0) */
        CloseHandle(handle);
        status = alloc_wexited(0);
        result = caml_alloc_tuple(2);
        Store_field(result, 0, Val_int(0));
        Store_field(result, 1, status);
        CAMLreturn(result);
    }

    CloseHandle(handle);

    /* Process exited: return (pid, WEXITED exit_code) */
    status = alloc_wexited((int)exit_code);
    result = caml_alloc_tuple(2);
    Store_field(result, 0, Val_int(pid));
    Store_field(result, 1, status);
    CAMLreturn(result);
}

CAMLprim value lwt_unix_waitpid_job(value v_flags, value v_pid)
{
    CAMLparam2(v_flags, v_pid);
    int pid = Int_val(v_pid);
    HANDLE handle;
    int use_wnohang = 0;
    value l = v_flags;

    /* Check for WNOHANG flag (tag 0 in Unix.wait_flag) */
    while (l != Val_emptylist) {
        if (Int_val(Field(l, 0)) == 0)
            use_wnohang = 1;
        l = Field(l, 1);
    }

    /* Note: OpenProcess by PID is inherently racy on Windows — if the
       process has exited and the PID has been reused, we may open an
       unrelated process. This is consistent with OCaml's own Unix.waitpid
       on Windows. Callers needing a race-free wait should use Lwt_process,
       which caches the process handle from CreateProcess. */
    handle = OpenProcess(SYNCHRONIZE | PROCESS_QUERY_INFORMATION, FALSE, (DWORD)pid);
    if (handle == NULL) {
        win32_maperr(GetLastError());
        uerror("waitpid", Nothing);
    }

    {
        LWT_UNIX_INIT_JOB(job, waitpid, 0);
        job->handle = handle;
        job->exit_code = 0;
        job->error_code = ERROR_SUCCESS;
        job->wait_result = 0;
        job->pid = pid;
        job->use_wnohang = use_wnohang;
        CAMLreturn(lwt_unix_alloc_job(&(job->job)));
    }
}

#endif
