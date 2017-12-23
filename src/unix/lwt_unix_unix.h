/* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2009-2010 Jérémie Dimino
 *               2009 Mauricio Fernandez
 *               2010 Pierre Chambart
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

/* Unix (non-Windows) version of Lwt C stubs.

   Implementing an Lwt C stub can be a bit challenging. See lwt_unix_getcwd_job
   (search for it in your text editor) for a well-documented "model"
   function, including conceptual documentation, practical considerations,
   common pitfalls, etc. */


#include <caml/unixsupport.h>
#include <caml/version.h>
#include <dirent.h>
#include <poll.h>
#include <sys/resource.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <unistd.h>

/* bind */

struct job_bind {
    struct lwt_unix_job job;
    int fd;
    union sock_addr_union addr;
    socklen_param_type addr_len;
    int result;
    int error_code;
};

static void worker_bind(struct job_bind *job)
{
    job->result = bind(job->fd, &job->addr.s_gen, job->addr_len);
    job->error_code = errno;
}

static value result_bind(struct job_bind *job)
{
    LWT_UNIX_CHECK_JOB(job, job->result != 0, "bind");
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_bind_job(value fd, value address)
{
    LWT_UNIX_INIT_JOB(job, bind, 0);
    job->fd = Int_val(fd);
    get_sockaddr(address, &job->addr, &job->addr_len);

    return lwt_unix_alloc_job(&job->job);
}

/** getcwd

    Lwt C stubs come in two varieties, depending on whether the underlying C
    call is blocking or non-blocking. In all cases, the Lwt wrapper around the C
    call must be made to appear *non*-blocking.

    1. The simple case is when the underlying C call is already non-blocking. An
       example of this is `lwt_unix_read`, which is used by Lwt to perform reads
       from file descriptors that are in non-blocking mode. This stub is a
       simple wrapper around `read(2)`. It converts its arguments from OCaml
       runtime representation to normal C, machine representation, passes them
       to `read(2)`, converts the result back to OCaml, and returns.

    2. In case the underlying C call is blocking, as `getcwd(3)` is, Lwt
       "converts" it to a non-blocking call by running it inside a worker
       thread. The rest of this comment is concerned with such blocking calls.

    For background on writing C stubs in OCaml, see

      http://caml.inria.fr/pub/docs/manual-ocaml/intfc.html


    Each Lwt stub for a blocking C call defines a *job* for the Lwt worker
    thread pool. The actual thread pool is implemented in `lwt_unix_stubs.c` and
    is beyond the scope of this comment. It is not necessary to understand it to
    implement Lwt jobs (indeed, the author currently doesn't remember exactly
    how it works!).

    The thread pool expects jobs in a fixed format (the `struct` below) and
    accompanying functions with fixed names. You *MUST* follow this naming
    conventions. Specifically, for a job "`FOO`", you must define

    - the struct `struct job_FOO`
    - the function `lwt_unix_FOO_job`
    - the function `worker_FOO`
    - the function `result_FOO`

    The struct `struct job_FOO`: This is the representation of the job that will
    be manipulated by the thread pool. It has several purposes:

    - Store the pointers to `worker_FOO` and `result_FOO`, so the thread pool is
      able to run them.
    - Store the C call arguments, or references to them, so they can be accessed
      by `worker_FOO` when it runs in the worker thread.
    - Store the C call results, so they can be accessed by `result_FOO` in the
      main thread.
    - Be something that can be placed in queues, or manipulated otherwise.

    The function `lwt_unix_FOO_job` allocates the job's struct.

    The function `worker_FOO` is later called in a worker thread to actually run
    the job.

    The function `result_FOO` is, even later, called in the main thread to return
    the result of the job to OCaml, and deallocate the job.

    It is also possible to define additional helper functions and/or types, as
    needed.


    Many stubs are defined on Unix-like systems, but not Windows, and vice
    versa. However, Lwt's OCaml code lacks conditional compilation for this, and
    expects all the C symbols to be available on all platforms during linking.
    It just doesn't call the ones that don't have a real implementation.

    The unimplemented symbols are defined using the `LWT_NOT_AVAILABLEx` macros.
    The `getcwd` job currently takes one argument, and is not implemented on
    Windows. For this reason, `lwt_unix_windows.h` has
    `LWT_NOT_AVAILABLE1(unix_getcwd_job)`. The `lwt_` prefix is left off.

    In case this macro is forgotten, Lwt's CI builds should detect that when you
    open a PR against the Lwt repo. Don't worry if this happens – it's a typical
    oversight for all contributors and maintainers.


    See inline comments in the implementation of the `getcwd` job below for
    other details. */

/** The first field of a job `struct` must always be `struct lwt_unix_job job`:

    - The `struct lwt_unix_job` contains the data the thread pool needs to
      manage the job: function pointers, the total size of the job `struct`,
      etc. Placing it at the start of each job `struct` type ensures that the
      offsets to these fields are the same between all kinds of jobs.
    - The `struct lwt_unix_job` must be called `job`, because that is what the
      job helper macros expect.

    The job `struct` should also contain a field `error_code`. This is a
    snapshot of `errno` from the worker thread, right after the C call ran.
    `errno` is a notorious source of pitfalls; see comments in `worker_getcwd`
    and `result_getcwd`.

    The rest of the `struct` is free-form, but typically it contains

    - One field per argument to the C call.
    - One field for the return value of the C call. */
struct job_getcwd {
    struct lwt_unix_job job;
    char buf[4096];
    char *result;
    int error_code;
};
/*
In the OCaml sources, getcwd's buffer size is set as either
- 4096 (in asmrun/spacetime.c, byterun/sys.c)
- PATH_MAX, MAXPATHLEN, or 512 (in otherlibs/unix/getcwd.c)
*/


/* Runs in the worker thread. This function is `static` (not visible outside
   this C file) because it is called only through the function pointer that is
   stored inside `job_getcwd::job` when the job is allocated. `static` is why
   the name is not prefixed with `lwt_unix_`. */
static void worker_getcwd(struct job_getcwd *job)
{
    /* Run the C call. We don't perform any checking in the worker thread,
       because we typically don't want to take any other action here – we want
       to take action in the main thread. In more complex calls, pre-checks on
       the arguments are done in the `lwt_unix_FOO_job` job-allocating
       function, and post-checks on the results are done in the `result_FOO`
       function. */
    job->result = getcwd(job->buf, sizeof(job->buf));

    /* Store the current value of `errno`. Note that if the C call succeeded, it
       did not reset `errno` to zero. In that case, `errno` still contains the
       error code from the last C call to fail in this worker thread. This means
       that `errno`/`job->error_code` *cannot* be used to determine whether the
       C call succeeded or not. */
    job->error_code = errno;
}

/* Runs in the main thread. This function is `static` for the same reason as
   `worker_getcwd`. */
static value result_getcwd(struct job_getcwd *job)
{
    /* This macro is defined in `lwt_unix.h`. The arguments are used as follows:

       - The first argument is the name of the job variable.
       - If the check in the second argument *succeds*, the C call, and job,
         failed (confusing!). Note that this check must *not* be based solely on
         `job->error_code`; see comment in `worker_getcwd` above.
       - The last argument is the name of the C call, used in a
         `Unix.Unix_error` exception raised if the job failed.

       If the check succeeds/job failed, this macro deallocates the job, raises
       the exception, and does *not* "return" to the rest of `result_getcwd`.
       Otherwise, if the job succeeded, the job is *not* deallocated, and
       execution continues in the rest of `result_getcwd`.

       `job->error_code` is used internally by the macro in creating the
       `Unix.Unix_error`. If this is incorrect (i.e., some job does not set
       `errno` on failure), it is necessary to replace the macro by its
       expansion, and modify the behavior. */
    LWT_UNIX_CHECK_JOB(job, job->result == NULL, "getcwd");

    /* Convert the job result to an OCaml value.

       In this case, create an OCaml string from the temporary buffer into
       which `getcwd(3)` wrote the current directory. This copies the string.

       Throughout Lwt, blocking C calls that run in worker threads can't write
       directly into OCaml strings, because the OCaml garbage collector might
       move the strings after the pointer has already been passed to the call,
       but while the call is still blocked. Bigarrays don't have this problem,
       so pointers into them are passed to blocking C calls, avoiding a copy.

       In addition to worker threads not being able to write into OCaml strings,
       they typically cannot *allocate* any OCaml strings (or other values)
       either, because the worker threads do not try to take OCaml's global
       runtime lock. This sometimes results in extra data copies. For an
       example, see the implementation of `readdir_n`. At the time of this
       writing, that implementation copied each string returned by `readdir`
       twice.

       For jobs that return integers or other kinds of values, it is necessary
       to use the various `Int_val`, `Long_val` macros, etc. See

         http://caml.inria.fr/pub/docs/manual-ocaml/intfc.html#sec415 */
    value result = caml_copy_string(job->result);

    /* Have to free the job manually! */
    lwt_unix_free_job(&job->job);

    return result;
}

/* In the case of `Lwt_unix.getcwd`, the argument is `()`, which is represented
   in C by one argument, which we conventually call `unit`. OCaml always passes
   the same value for this argument, and we don't use it. */
CAMLprim value lwt_unix_getcwd_job(value unit)
{
    /* Allocate the `job_getcwd` on the OCaml heap. Inside it, store its size,
       and pointers to `worker_getcwd` and `result_getcwd`. Arguments must be
       stored manually after the macro is called, but in the case of `getcwd`,
       there are no arguments to initialize.

       For an example of a job that has arguments, see `lwt_unix_read_job`.

       The first argument is the name of the variable to be created to store
       the pointer to the job `struct`, i.e.

         struct job_getcwd *job = ...

       The last argument is the number of bytes of storage to reserve in memory
       immediately following the `struct`. This is for fields such as
       `char data[]` at the end of the struct. It is typically zero. For an
       example where it is not zero, see `lwt_unix_read_job` again.

       If the additional data is stored inline in the job struct, it is
       deallocated with `lwt_unix_free_job`. If the additional data is for
       pointers to additional structure, you must remember to deallocate it
       yourself. For an example of this, see `readdir_n`.*/
    LWT_UNIX_INIT_JOB(job, getcwd, 0);

    /* Allocate a corresponding object in the OCaml heap. `&job->job` is the
       same numeric address as `job`, but has type `struct lwt_unix_job`. */
    return lwt_unix_alloc_job(&job->job);
}

/* +-----------------------------------------------------------------+
   | Termios conversion                                              |
   +-----------------------------------------------------------------+ */

/* TODO: make it reentrant. */

enum { Bool, Enum, Speed, Char, End };

enum { Input, Output };

enum { Iflags, Oflags, Cflags, Lflags };

/* Number of fields in the terminal_io record field. Cf. unix.mli */

#define NFIELDS 38

/* Structure of the terminal_io record. Cf. unix.mli */

static long terminal_io_descr[] = {
    /* Input modes */
    Bool, Iflags, IGNBRK, Bool, Iflags, BRKINT, Bool, Iflags, IGNPAR, Bool,
    Iflags, PARMRK, Bool, Iflags, INPCK, Bool, Iflags, ISTRIP, Bool, Iflags,
    INLCR, Bool, Iflags, IGNCR, Bool, Iflags, ICRNL, Bool, Iflags, IXON, Bool,
    Iflags, IXOFF,
    /* Output modes */
    Bool, Oflags, OPOST,
    /* Control modes */
    Speed, Output, Speed, Input, Enum, Cflags, 5, 4, CSIZE, CS5, CS6, CS7, CS8,
    Enum, Cflags, 1, 2, CSTOPB, 0, CSTOPB, Bool, Cflags, CREAD, Bool, Cflags,
    PARENB, Bool, Cflags, PARODD, Bool, Cflags, HUPCL, Bool, Cflags, CLOCAL,
    /* Local modes */
    Bool, Lflags, ISIG, Bool, Lflags, ICANON, Bool, Lflags, NOFLSH, Bool,
    Lflags, ECHO, Bool, Lflags, ECHOE, Bool, Lflags, ECHOK, Bool, Lflags,
    ECHONL,
    /* Control characters */
    Char, VINTR, Char, VQUIT, Char, VERASE, Char, VKILL, Char, VEOF, Char, VEOL,
    Char, VMIN, Char, VTIME, Char, VSTART, Char, VSTOP, End};

static struct {
    speed_t speed;
    int baud;
} speedtable[] = {{B50, 50},
                  {B75, 75},
                  {B110, 110},
                  {B134, 134},
                  {B150, 150},
                  {B300, 300},
                  {B600, 600},
                  {B1200, 1200},
                  {B1800, 1800},
                  {B2400, 2400},
                  {B4800, 4800},
                  {B9600, 9600},
                  {B19200, 19200},
                  {B38400, 38400},
#ifdef B57600
                  {B57600, 57600},
#endif
#ifdef B115200
                  {B115200, 115200},
#endif
#ifdef B230400
                  {B230400, 230400},
#endif
                  {B0, 0}};

#define NSPEEDS (sizeof(speedtable) / sizeof(speedtable[0]))

static tcflag_t *choose_field(struct termios *terminal_status, long field)
{
    switch (field) {
        case Iflags:
            return &terminal_status->c_iflag;
        case Oflags:
            return &terminal_status->c_oflag;
        case Cflags:
            return &terminal_status->c_cflag;
        case Lflags:
            return &terminal_status->c_lflag;
        default:
            return 0;
    }
}

static void encode_terminal_status(struct termios *terminal_status, value *dst)
{
    long *pc;
    int i;

    for (pc = terminal_io_descr; *pc != End; dst++) {
        switch (*pc++) {
            case Bool: {
                tcflag_t *src = choose_field(terminal_status, *pc++);
                tcflag_t msk = *pc++;
                *dst = Val_bool(*src & msk);
                break;
            }
            case Enum: {
                tcflag_t *src = choose_field(terminal_status, *pc++);
                int ofs = *pc++;
                int num = *pc++;
                tcflag_t msk = *pc++;
                for (i = 0; i < num; i++) {
                    if ((*src & msk) == pc[i]) {
                        *dst = Val_int(i + ofs);
                        break;
                    }
                }
                pc += num;
                break;
            }
            case Speed: {
                int which = *pc++;
                speed_t speed = 0;
                *dst =
                    Val_int(9600); /* in case no speed in speedtable matches */
                switch (which) {
                    case Output:
                        speed = cfgetospeed(terminal_status);
                        break;
                    case Input:
                        speed = cfgetispeed(terminal_status);
                        break;
                }
                for (i = 0; i < NSPEEDS; i++) {
                    if (speed == speedtable[i].speed) {
                        *dst = Val_int(speedtable[i].baud);
                        break;
                    }
                }
                break;
            }
            case Char: {
                int which = *pc++;
                *dst = Val_int(terminal_status->c_cc[which]);
                break;
            }
        }
    }
}

static void decode_terminal_status(struct termios *terminal_status, value *src)
{
    long *pc;
    int i;

    for (pc = terminal_io_descr; *pc != End; src++) {
        switch (*pc++) {
            case Bool: {
                tcflag_t *dst = choose_field(terminal_status, *pc++);
                tcflag_t msk = *pc++;
                if (Bool_val(*src))
                    *dst |= msk;
                else
                    *dst &= ~msk;
                break;
            }
            case Enum: {
                tcflag_t *dst = choose_field(terminal_status, *pc++);
                int ofs = *pc++;
                int num = *pc++;
                tcflag_t msk = *pc++;
                i = Int_val(*src) - ofs;
                if (i >= 0 && i < num) {
                    *dst = (*dst & ~msk) | pc[i];
                } else {
                    unix_error(EINVAL, "tcsetattr", Nothing);
                }
                pc += num;
                break;
            }
            case Speed: {
                int which = *pc++;
                int baud = Int_val(*src);
                int res = 0;
                for (i = 0; i < NSPEEDS; i++) {
                    if (baud == speedtable[i].baud) {
                        switch (which) {
                            case Output:
                                res = cfsetospeed(terminal_status,
                                                  speedtable[i].speed);
                                break;
                            case Input:
                                res = cfsetispeed(terminal_status,
                                                  speedtable[i].speed);
                                break;
                        }
                        if (res == -1) uerror("tcsetattr", Nothing);
                        goto ok;
                    }
                }
                unix_error(EINVAL, "tcsetattr", Nothing);
            ok:
                break;
            }
            case Char: {
                int which = *pc++;
                terminal_status->c_cc[which] = Int_val(*src);
                break;
            }
        }
    }
}

/* +-----------------------------------------------------------------+
   | JOB: tcgetattr                                                  |
   +-----------------------------------------------------------------+ */

struct job_tcgetattr {
    struct lwt_unix_job job;
    int fd;
    struct termios termios;
    int result;
    int error_code;
};

static void worker_tcgetattr(struct job_tcgetattr *job)
{
    job->result = tcgetattr(job->fd, &job->termios);
    job->error_code = errno;
}

static value result_tcgetattr(struct job_tcgetattr *job)
{
    LWT_UNIX_CHECK_JOB(job, job->result < 0, "tcgetattr");
    value res = caml_alloc_tuple(NFIELDS);
    encode_terminal_status(&job->termios, &Field(res, 0));
    lwt_unix_free_job(&job->job);
    return res;
}

CAMLprim value lwt_unix_tcgetattr_job(value fd)
{
    LWT_UNIX_INIT_JOB(job, tcgetattr, 0);
    job->fd = Int_val(fd);
    return lwt_unix_alloc_job(&job->job);
}

/* +-----------------------------------------------------------------+
   | JOB: tcsetattr                                                  |
   +-----------------------------------------------------------------+ */

struct job_tcsetattr {
    struct lwt_unix_job job;
    int fd;
    int when;
    /* This array contains only non-allocated values. */
    value termios[NFIELDS];
    int result;
    int error_code;
};

static int when_flag_table[] = {TCSANOW, TCSADRAIN, TCSAFLUSH};

static void worker_tcsetattr(struct job_tcsetattr *job)
{
    struct termios termios;
    int result = tcgetattr(job->fd, &termios);
    if (result < 0) {
        job->result = result;
        job->error_code = errno;
    } else {
        decode_terminal_status(&termios, &(job->termios[0]));
        job->result = tcsetattr(job->fd, job->when, &termios);
        job->error_code = errno;
    }
}

static value result_tcsetattr(struct job_tcsetattr *job)
{
    LWT_UNIX_CHECK_JOB(job, job->result < 0, "tcsetattr");
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_tcsetattr_job(value fd, value when, value termios)
{
    LWT_UNIX_INIT_JOB(job, tcsetattr, 0);
    job->fd = Int_val(fd);
    job->when = when_flag_table[Int_val(when)];
    memcpy(&job->termios, &Field(termios, 0), NFIELDS * sizeof(value));
    return lwt_unix_alloc_job(&job->job);
}

/* +-----------------------------------------------------------------+
   | Unavailable primitives                                          |
   +-----------------------------------------------------------------+ */

LWT_NOT_AVAILABLE1(unix_is_socket)
LWT_NOT_AVAILABLE1(unix_socketpair_stub)
LWT_NOT_AVAILABLE1(unix_system_job)
LWT_NOT_AVAILABLE4(process_create_process)
LWT_NOT_AVAILABLE1(process_wait_job)
LWT_NOT_AVAILABLE2(process_terminate_process)
