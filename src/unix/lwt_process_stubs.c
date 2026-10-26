/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if defined(LWT_ON_WINDOWS)

#include "lwt_unix.h"

#if OCAML_VERSION < 41300
#define CAML_INTERNALS
#endif

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/osdeps.h>

static HANDLE get_handle(value opt) {
  value fd;
  if (Is_some(opt)) {
    fd = Some_val(opt);
    if (Descr_kind_val(fd) == KIND_SOCKET) {
      win32_maperr(ERROR_INVALID_HANDLE);
      uerror("CreateProcess", Nothing);
      return NULL;
    } else
      return Handle_val(fd);
  } else
    return INVALID_HANDLE_VALUE;
}

/* Ensures the handle [h] is inheritable. Returns the handle for the
   child process in [hStd] and in [to_close] if it needs to be closed
   after CreateProcess. */
static int ensure_inheritable(HANDLE h /* in */,
                              HANDLE * hStd /* out */,
                              HANDLE * to_close /* out */)
{
  DWORD flags;
  HANDLE hp;

  if (h == INVALID_HANDLE_VALUE || h == NULL)
    return 1;
  if (! GetHandleInformation(h, &flags))
    return 0;
  hp = GetCurrentProcess();
  if (! (flags & HANDLE_FLAG_INHERIT)) {
    if (! DuplicateHandle(hp, h, hp, hStd, 0, TRUE, DUPLICATE_SAME_ACCESS))
      return 0;
    *to_close = *hStd;
  } else {
    *hStd = h;
  }
  return 1;
}

CAMLprim value lwt_process_create_process(value prog, value cmdline, value env,
                                          value cwd, value fds) {
  CAMLparam5(prog, cmdline, env, cwd, fds);
  CAMLlocal1(result);

  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  DWORD flags = 0, err;
  HANDLE fd0, fd1, fd2;
  HANDLE to_close0 = INVALID_HANDLE_VALUE, to_close1 = INVALID_HANDLE_VALUE,
    to_close2 = INVALID_HANDLE_VALUE;

  fd0 = get_handle(Field(fds, 0));
  fd1 = get_handle(Field(fds, 1));
  fd2 = get_handle(Field(fds, 2));

  err = ERROR_SUCCESS;
  ZeroMemory(&si, sizeof(si));
  ZeroMemory(&pi, sizeof(pi));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;

  /* If needed, duplicate the handles fd1, fd2, fd3 to make sure they
     are inheritable. */
  if (! ensure_inheritable(fd0, &si.hStdInput, &to_close0) ||
      ! ensure_inheritable(fd1, &si.hStdOutput, &to_close1) ||
      ! ensure_inheritable(fd2, &si.hStdError, &to_close2)) {
    err = GetLastError(); goto ret;
  }


#define string_option(opt) \
  (Is_block(opt) ? caml_stat_strdup_to_os(String_val(Field(opt, 0))) : NULL)

  char_os
    *progs = string_option(prog),
    *cmdlines = caml_stat_strdup_to_os(String_val(cmdline)),
    *envs = string_option(env),
    *cwds = string_option(cwd);

#undef string_option

  flags |= CREATE_UNICODE_ENVIRONMENT;
  if (! CreateProcess(progs, cmdlines, NULL, NULL, TRUE, flags,
                      envs, cwds, &si, &pi)) {
    err = GetLastError();
  }

  caml_stat_free(progs);
  caml_stat_free(cmdlines);
  caml_stat_free(envs);
  caml_stat_free(cwds);

ret:
/* Close the handles if we duplicated them above. */
  if (to_close0 != INVALID_HANDLE_VALUE) CloseHandle(to_close0);
  if (to_close1 != INVALID_HANDLE_VALUE) CloseHandle(to_close1);
  if (to_close2 != INVALID_HANDLE_VALUE) CloseHandle(to_close2);

  if (err != ERROR_SUCCESS) {
    win32_maperr(err);
    uerror("CreateProcess", Nothing);
  }

  CloseHandle(pi.hThread);

  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(pi.dwProcessId));
  Store_field(result, 1, win_alloc_handle(pi.hProcess));
  CAMLreturn(result);
}

struct job_wait {
  struct lwt_unix_job job;
  HANDLE handle;
};

static void worker_wait(struct job_wait *job) {
  WaitForSingleObject(job->handle, INFINITE);
}

static value result_wait(struct job_wait *job) {
  DWORD code, error;
  if (!GetExitCodeProcess(job->handle, &code)) {
    error = GetLastError();
    CloseHandle(job->handle);
    lwt_unix_free_job(&job->job);
    win32_maperr(error);
    uerror("GetExitCodeProcess", Nothing);
  }
  CloseHandle(job->handle);
  lwt_unix_free_job(&job->job);
  return Val_int(code);
}

CAMLprim value lwt_process_wait_job(value handle) {
  LWT_UNIX_INIT_JOB(job, wait, 0);
  job->handle = Handle_val(handle);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_process_terminate_process(value handle, value code) {
  if (!TerminateProcess(Handle_val(handle), Int_val(code))) {
    win32_maperr(GetLastError());
    uerror("TerminateProcess", Nothing);
  }
  return Val_unit;
}

#else /* defined(LWT_ON_WINDOWS) */

/* This is used to suppress a warning from ranlib about the object file having
   no symbols. */
void lwt_process_dummy_symbol(void) {}

#endif /* defined(LWT_ON_WINDOWS) */

#include <caml/fail.h>
#include <caml/mlvalues.h>

#if !defined(LWT_ON_WINDOWS) && defined(HAVE_POSIX_SPAWN)

#include <fcntl.h>
#include <spawn.h>
#include <sys/types.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

extern char **environ;

CAMLprim value lwt_process_posix_spawn(value prog, value args, value env,
                                       value redirections) {
  CAMLparam4(prog, args, env, redirections);
  char **argv;
  char **envp;
  const char *path;
  pid_t pid;
  posix_spawn_file_actions_t actions;
  int actions_initialized = 0;
  int r;

  caml_unix_check_path(prog, "posix_spawnp");
  path = String_val(prog);
  argv = cstringvect(args, "posix_spawnp");
  if (Is_some(env))
    envp = cstringvect(Some_val(env), "posix_spawnp");
  else
    envp = environ;

  r = posix_spawn_file_actions_init(&actions);
  if (r != 0) goto cleanup;
  actions_initialized = 1;

  /* Mirrors [unix_redirect] in lwt_process.cppo.ml. [redirections] holds
     stdin, stdout and stderr, in that order, as [unix_spawn_redirection] values:
     - Spawn_keep (0), Spawn_dev_null (1), Spawn_close (2) are immediates;
     - Spawn_dup fd (tag 0), Spawn_move fd (tag 1) are blocks.
     File actions run in order, so later redirections see the effect of earlier
     ones, as with the fork path. */
  for (int dst = 0; dst < 3; dst++) {
    value redirection = Field(redirections, dst);
    if (Is_block(redirection)) {
      int src = Int_val(Field(redirection, 0));
      r = posix_spawn_file_actions_adddup2(&actions, src, dst);
      if (r == 0 && Tag_val(redirection) == 1)
        r = posix_spawn_file_actions_addclose(&actions, src);
    } else {
      switch (Long_val(redirection)) {
        case 0:
          r = 0;
          break;
        case 1:
          r = posix_spawn_file_actions_addopen(
            &actions, dst, "/dev/null", O_RDWR, 0666);
          break;
        default:
          r = posix_spawn_file_actions_addclose(&actions, dst);
          break;
      }
    }
    if (r != 0) goto cleanup;
  }

  r = posix_spawnp(&pid, path, &actions, NULL, argv, envp);

cleanup:
  if (actions_initialized) posix_spawn_file_actions_destroy(&actions);
  cstringvect_free(argv);
  if (Is_some(env)) cstringvect_free(envp);
  if (r != 0) unix_error(r, "posix_spawnp", prog);
  CAMLreturn(Val_long(pid));
}

#else

CAMLprim value lwt_process_posix_spawn(value prog, value args, value env,
                                       value redirections) {
  (void)prog;
  (void)args;
  (void)env;
  (void)redirections;
  caml_failwith("posix_spawnp is unavailable");
}

#endif
