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

#define ARGS(args...) args

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

#include "unix_recv_send_utils.h"
/* +-----------------------------------------------------------------+
   | wait4                                                           |
   +-----------------------------------------------------------------+ */

/* Some code duplicated from OCaml's otherlibs/unix/wait.c */

CAMLextern int caml_convert_signal_number(int);
CAMLextern int caml_rev_convert_signal_number(int);

#if !defined(__ANDROID__)

#if !(defined(WIFEXITED) && defined(WEXITSTATUS) && defined(WIFSTOPPED) && \
      defined(WSTOPSIG) && defined(WTERMSIG))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status)&0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#define WIFSTOPPED(status) (((status)&0xFF) == 0xFF)
#define WSTOPSIG(status) (((status) >> 8) & 0xFF)
#define WTERMSIG(status) ((status)&0x3F)
#endif

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

static value alloc_process_status(int status)
{
    value st;

    if (WIFEXITED(status)) {
        st = caml_alloc_small(1, TAG_WEXITED);
        Field(st, 0) = Val_int(WEXITSTATUS(status));
    } else if (WIFSTOPPED(status)) {
        st = caml_alloc_small(1, TAG_WSTOPPED);
        Field(st, 0) =
            Val_int(caml_rev_convert_signal_number(WSTOPSIG(status)));
    } else {
        st = caml_alloc_small(1, TAG_WSIGNALED);
        Field(st, 0) =
            Val_int(caml_rev_convert_signal_number(WTERMSIG(status)));
    }
    return st;
}

static int wait_flag_table[] = {WNOHANG, WUNTRACED};

value lwt_unix_wait4(value flags, value pid_req)
{
    CAMLparam1(flags);
    CAMLlocal2(times, res);

    int pid, status, cv_flags;
    cv_flags = caml_convert_flag_list(flags, wait_flag_table);

    struct rusage ru;

    caml_enter_blocking_section();
    pid = wait4(Int_val(pid_req), &status, cv_flags, &ru);
    caml_leave_blocking_section();
    if (pid == -1) uerror("wait4", Nothing);

    times = caml_alloc_small(2 * Double_wosize, Double_array_tag);
    Store_double_field(times, 0,
                       ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6);
    Store_double_field(times, 1,
                       ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);

    res = caml_alloc_tuple(3);
    Store_field(res, 0, Val_int(pid));
    Store_field(res, 1, alloc_process_status(status));
    Store_field(res, 2, times);
    CAMLreturn(res);
}

#else

LWT_NOT_AVAILABLE2(unix_wait4)

#endif

/* +-----------------------------------------------------------------+
   | CPUs                                                            |
   +-----------------------------------------------------------------+ */

#if defined(HAVE_GETCPU)

CAMLprim value lwt_unix_get_cpu(value Unit)
{
    int cpu = sched_getcpu();
    if (cpu < 0) uerror("sched_getcpu", Nothing);
    return Val_int(cpu);
}

#else

LWT_NOT_AVAILABLE1(unix_get_cpu)

#endif

#if defined(HAVE_AFFINITY)

CAMLprim value lwt_unix_get_affinity(value val_pid)
{
    CAMLparam1(val_pid);
    CAMLlocal2(list, node);
    cpu_set_t cpus;
    if (sched_getaffinity(Int_val(val_pid), sizeof(cpu_set_t), &cpus) < 0)
        uerror("sched_getaffinity", Nothing);
    int i;
    list = Val_int(0);
    for (i = sizeof(cpu_set_t) * 8 - 1; i >= 0; i--) {
        if (CPU_ISSET(i, &cpus)) {
            node = caml_alloc_tuple(2);
            Field(node, 0) = Val_int(i);
            Field(node, 1) = list;
            list = node;
        }
    }
    CAMLreturn(list);
}

CAMLprim value lwt_unix_set_affinity(value val_pid, value val_cpus)
{
    cpu_set_t cpus;
    CPU_ZERO(&cpus);
    for (; Is_block(val_cpus); val_cpus = Field(val_cpus, 1))
        CPU_SET(Int_val(Field(val_cpus, 0)), &cpus);
    if (sched_setaffinity(Int_val(val_pid), sizeof(cpu_set_t), &cpus) < 0)
        uerror("sched_setaffinity", Nothing);
    return Val_unit;
}

#else

LWT_NOT_AVAILABLE1(unix_get_affinity)
LWT_NOT_AVAILABLE2(unix_set_affinity)

#endif

/* +-----------------------------------------------------------------+
   | JOB: guess_blocking                                             |
   +-----------------------------------------------------------------+ */

struct job_guess_blocking {
    struct lwt_unix_job job;
    int fd;
    int result;
};

static void worker_guess_blocking(struct job_guess_blocking *job)
{
    struct stat stat;
    if (fstat(job->fd, &stat) == 0)
        job->result = !(S_ISFIFO(stat.st_mode) || S_ISSOCK(stat.st_mode));
    else
        job->result = 1;
}

static value result_guess_blocking(struct job_guess_blocking *job)
{
    value result = Val_bool(job->result);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_guess_blocking_job(value val_fd)
{
    LWT_UNIX_INIT_JOB(job, guess_blocking, 0);
    job->fd = Int_val(val_fd);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: wait_mincore                                               |
   +-----------------------------------------------------------------+ */

#ifdef __CYGWIN__
LWT_NOT_AVAILABLE2(unix_wait_mincore_job)
#else
struct job_wait_mincore {
    struct lwt_unix_job job;
    char *ptr;
};

static void worker_wait_mincore(struct job_wait_mincore *job)
{
    /* Read the byte to force the kernel to fetch the page: */
    char dummy;
    memcpy(&dummy, job->ptr, 1);
}

static value result_wait_mincore(struct job_wait_mincore *job)
{
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_wait_mincore_job(value val_buffer, value val_offset)
{
    LWT_UNIX_INIT_JOB(job, wait_mincore, 0);
    job->ptr = (char *)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
    return lwt_unix_alloc_job(&(job->job));
}
#endif

/* +-----------------------------------------------------------------+
   | JOB: open                                                       |
   +-----------------------------------------------------------------+ */

#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif
#ifndef O_DSYNC
#define O_DSYNC 0
#endif
#ifndef O_SYNC
#define O_SYNC 0
#endif
#ifndef O_RSYNC
#define O_RSYNC 0
#endif

static int open_flag_table[] = {
    O_RDONLY, O_WRONLY, O_RDWR,  O_NONBLOCK, O_APPEND, O_CREAT, O_TRUNC,
    O_EXCL,   O_NOCTTY, O_DSYNC, O_SYNC,     O_RSYNC,  0, /* O_SHARE_DELETE,
                                                             Windows-only */
    0, /* O_CLOEXEC, treated specially */
    0  /* O_KEEPEXEC, treated specially */
};

enum { CLOEXEC = 1, KEEPEXEC = 2 };

static int open_cloexec_table[15] = {0, 0, 0, 0, 0, 0,       0,       0,
                                     0, 0, 0, 0, 0, CLOEXEC, KEEPEXEC};

struct job_open {
    struct lwt_unix_job job;
    int flags;
    int perms;
    int fd; /* will have value CLOEXEC or KEEPEXEC on entry to worker_open */
    int blocking;
    int error_code;
    char *name;
    char data[];
};

static void worker_open(struct job_open *job)
{
    int fd;
    int cloexec;

    if (job->fd & CLOEXEC)
        cloexec = 1;
    else if (job->fd & KEEPEXEC)
        cloexec = 0;
    else
#if OCAML_VERSION_MAJOR >= 4 && OCAML_VERSION_MINOR >= 5
        cloexec = unix_cloexec_default;
#else
        cloexec = 0;
#endif

#if defined(O_CLOEXEC)
    if (cloexec) job->flags |= O_CLOEXEC;
#endif

    fd = open(job->name, job->flags, job->perms);
#if !defined(O_CLOEXEC) && defined(FD_CLOEXEC)
    if (fd >= 0 && cloexec) {
        int flags = fcntl(fd, F_GETFD, 0);

        if (flags == -1 || fcntl(fd, F_SETFD, flags | FD_CLOEXEC) == -1) {
            int serrno = errno;
            close(fd);
            errno = serrno;
            fd = -1;
        }
    }
#endif
    job->fd = fd;
    job->error_code = errno;
    if (fd >= 0) {
        struct stat stat;
        if (fstat(fd, &stat) < 0)
            job->blocking = 1;
        else
            job->blocking = !(S_ISFIFO(stat.st_mode) || S_ISSOCK(stat.st_mode));
    }
}

static value result_open(struct job_open *job)
{
    int fd = job->fd;
    LWT_UNIX_CHECK_JOB_ARG(job, fd < 0, "open", job->name);
    value result = caml_alloc_tuple(2);
    Field(result, 0) = Val_int(fd);
    Field(result, 1) = Val_bool(job->blocking);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_open_job(value name, value flags, value perms)
{
    LWT_UNIX_INIT_JOB_STRING(job, open, 0, name);
    job->fd = caml_convert_flag_list(flags, open_cloexec_table);
    job->flags = caml_convert_flag_list(flags, open_flag_table);
    job->perms = Int_val(perms);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: read                                                       |
   +-----------------------------------------------------------------+ */

struct job_read {
    struct lwt_unix_job job;
    /* The file descriptor. */
    int fd;
    /* The amount of data to read. */
    long length;
    /* The OCaml string. */
    value string;
    /* The offset in the string. */
    long offset;
    /* The result of the read syscall. */
    long result;
    /* The value of errno. */
    int error_code;
    /* The temporary buffer. */
    char buffer[];
};

static void worker_read(struct job_read *job)
{
    job->result = read(job->fd, job->buffer, job->length);
    job->error_code = errno;
}

static value result_read(struct job_read *job)
{
    long result = job->result;
    if (result < 0) {
        int error_code = job->error_code;
        caml_remove_generational_global_root(&(job->string));
        lwt_unix_free_job(&job->job);
        unix_error(error_code, "read", Nothing);
    } else {
        memcpy(String_val(job->string) + job->offset, job->buffer, result);
        caml_remove_generational_global_root(&(job->string));
        lwt_unix_free_job(&job->job);
        return Val_long(result);
    }
}

CAMLprim value lwt_unix_read_job(value val_fd, value val_buffer,
                                 value val_offset, value val_length)
{
    long length = Long_val(val_length);
    LWT_UNIX_INIT_JOB(job, read, length);
    job->fd = Int_val(val_fd);
    job->length = length;
    job->string = val_buffer;
    job->offset = Long_val(val_offset);
    caml_register_generational_global_root(&(job->string));
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: bytes_read                                                 |
   +-----------------------------------------------------------------+ */

struct job_bytes_read {
    struct lwt_unix_job job;
    /* The file descriptor. */
    int fd;
    /* The destination buffer. */
    char *buffer;
    /* The offset in the string. */
    long offset;
    /* The amount of data to read. */
    long length;
    /* The result of the read syscall. */
    long result;
    /* The value of errno. */
    int error_code;
};

static void worker_bytes_read(struct job_bytes_read *job)
{
    job->result = read(job->fd, job->buffer, job->length);
    job->error_code = errno;
}

static value result_bytes_read(struct job_bytes_read *job)
{
    long result = job->result;
    LWT_UNIX_CHECK_JOB(job, result < 0, "read");
    lwt_unix_free_job(&job->job);
    return Val_long(result);
}

CAMLprim value lwt_unix_bytes_read_job(value val_fd, value val_buf,
                                       value val_ofs, value val_len)
{
    LWT_UNIX_INIT_JOB(job, bytes_read, 0);
    job->fd = Int_val(val_fd);
    job->buffer = (char *)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
    job->length = Long_val(val_len);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: write                                                      |
   +-----------------------------------------------------------------+ */

struct job_write {
    struct lwt_unix_job job;
    int fd;
    long length;
    long result;
    int error_code;
    char buffer[];
};

static void worker_write(struct job_write *job)
{
    job->result = write(job->fd, job->buffer, job->length);
    job->error_code = errno;
}

static value result_write(struct job_write *job)
{
    long result = job->result;
    LWT_UNIX_CHECK_JOB(job, result < 0, "write");
    lwt_unix_free_job(&job->job);
    return Val_long(result);
}

CAMLprim value lwt_unix_write_job(value val_fd, value val_string,
                                  value val_offset, value val_length)
{
    long length = Long_val(val_length);
    LWT_UNIX_INIT_JOB(job, write, length);
    job->fd = Int_val(val_fd);
    job->length = length;
    memcpy(job->buffer, String_val(val_string) + Long_val(val_offset), length);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: bytes_write                                                |
   +-----------------------------------------------------------------+ */

struct job_bytes_write {
    struct lwt_unix_job job;
    int fd;
    char *buffer;
    long length;
    long result;
    int error_code;
};

static void worker_bytes_write(struct job_bytes_write *job)
{
    job->result = write(job->fd, job->buffer, job->length);
    job->error_code = errno;
}

static value result_bytes_write(struct job_bytes_write *job)
{
    long result = job->result;
    LWT_UNIX_CHECK_JOB(job, result < 0, "write");
    lwt_unix_free_job(&job->job);
    return Val_long(result);
}

CAMLprim value lwt_unix_bytes_write_job(value val_fd, value val_buffer,
                                        value val_offset, value val_length)
{
    LWT_UNIX_INIT_JOB(job, bytes_write, 0);
    job->fd = Int_val(val_fd);
    job->buffer = (char *)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
    job->length = Long_val(val_length);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: stat                                                       |
   +-----------------------------------------------------------------+ */

struct job_stat {
    struct lwt_unix_job job;
    struct stat stat;
    int result;
    int error_code;
    char *name;
    char data[];
};

static value copy_stat(int use_64, struct stat *buf)
{
    CAMLparam0();
    CAMLlocal5(atime, mtime, ctime, offset, v);

    atime = caml_copy_double((double)buf->st_atime +
                             (NANOSEC(buf, a) / 1000000000.0));
    mtime = caml_copy_double((double)buf->st_mtime +
                             (NANOSEC(buf, m) / 1000000000.0));
    ctime = caml_copy_double((double)buf->st_ctime +
                             (NANOSEC(buf, c) / 1000000000.0));
    offset = use_64 ? caml_copy_int64(buf->st_size) : Val_int(buf->st_size);
    v = caml_alloc_small(12, 0);
    Field(v, 0) = Val_int(buf->st_dev);
    Field(v, 1) = Val_int(buf->st_ino);
    switch (buf->st_mode & S_IFMT) {
        case S_IFREG:
            Field(v, 2) = Val_int(0);
            break;
        case S_IFDIR:
            Field(v, 2) = Val_int(1);
            break;
        case S_IFCHR:
            Field(v, 2) = Val_int(2);
            break;
        case S_IFBLK:
            Field(v, 2) = Val_int(3);
            break;
        case S_IFLNK:
            Field(v, 2) = Val_int(4);
            break;
        case S_IFIFO:
            Field(v, 2) = Val_int(5);
            break;
        case S_IFSOCK:
            Field(v, 2) = Val_int(6);
            break;
        default:
            Field(v, 2) = Val_int(0);
            break;
    }
    Field(v, 3) = Val_int(buf->st_mode & 07777);
    Field(v, 4) = Val_int(buf->st_nlink);
    Field(v, 5) = Val_int(buf->st_uid);
    Field(v, 6) = Val_int(buf->st_gid);
    Field(v, 7) = Val_int(buf->st_rdev);
    Field(v, 8) = offset;
    Field(v, 9) = atime;
    Field(v, 10) = mtime;
    Field(v, 11) = ctime;
    CAMLreturn(v);
}

static void worker_stat(struct job_stat *job)
{
    job->result = stat(job->name, &job->stat);
    job->error_code = errno;
}

static value result_stat(struct job_stat *job)
{
    LWT_UNIX_CHECK_JOB_ARG(job, job->result < 0, "stat", job->name);
    value result = copy_stat(0, &job->stat);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_stat_job(value name)
{
    LWT_UNIX_INIT_JOB_STRING(job, stat, 0, name);
    return lwt_unix_alloc_job(&(job->job));
}

static value result_stat_64(struct job_stat *job)
{
    LWT_UNIX_CHECK_JOB_ARG(job, job->result < 0, "stat", job->name);
    value result = copy_stat(1, &job->stat);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_stat_64_job(value name)
{
    LWT_UNIX_INIT_JOB_STRING(job, stat, 0, name);
    job->job.result = (lwt_unix_job_result)result_stat_64;
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: lstat                                                      |
   +-----------------------------------------------------------------+ */

struct job_lstat {
    struct lwt_unix_job job;
    struct stat lstat;
    int result;
    int error_code;
    char *name;
    char data[];
};

static void worker_lstat(struct job_lstat *job)
{
    job->result = lstat(job->name, &job->lstat);
    job->error_code = errno;
}

static value result_lstat(struct job_lstat *job)
{
    LWT_UNIX_CHECK_JOB_ARG(job, job->result < 0, "lstat", job->name);
    value result = copy_stat(0, &(job->lstat));
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_lstat_job(value name)
{
    LWT_UNIX_INIT_JOB_STRING(job, lstat, 0, name);
    return lwt_unix_alloc_job(&(job->job));
}

static value result_lstat_64(struct job_lstat *job)
{
    LWT_UNIX_CHECK_JOB_ARG(job, job->result < 0, "lstat", job->name);
    value result = copy_stat(1, &(job->lstat));
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_lstat_64_job(value name)
{
    LWT_UNIX_INIT_JOB_STRING(job, lstat, 0, name);
    job->job.result = (lwt_unix_job_result)result_lstat_64;
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: fstat                                                      |
   +-----------------------------------------------------------------+ */

struct job_fstat {
    struct lwt_unix_job job;
    int fd;
    struct stat fstat;
    int result;
    int error_code;
};

static void worker_fstat(struct job_fstat *job)
{
    job->result = fstat(job->fd, &(job->fstat));
    job->error_code = errno;
}

static value result_fstat(struct job_fstat *job)
{
    LWT_UNIX_CHECK_JOB(job, job->result < 0, "fstat");
    value result = copy_stat(0, &(job->fstat));
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_fstat_job(value val_fd)
{
    LWT_UNIX_INIT_JOB(job, fstat, 0);
    job->fd = Int_val(val_fd);
    return lwt_unix_alloc_job(&(job->job));
}

static value result_fstat_64(struct job_fstat *job)
{
    LWT_UNIX_CHECK_JOB(job, job->result < 0, "fstat");
    value result = copy_stat(1, &(job->fstat));
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_fstat_64_job(value val_fd)
{
    LWT_UNIX_INIT_JOB(job, fstat, 0);
    job->job.result = (lwt_unix_job_result)result_fstat_64;
    job->fd = Int_val(val_fd);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: utimes                                                     |
   +-----------------------------------------------------------------+ */

struct job_utimes {
    struct lwt_unix_job job;
    char *path;
    const struct timeval *times_pointer;
    struct timeval times[2];
    int result;
    int error_code;
    char data[];
};

static void worker_utimes(struct job_utimes *job)
{
    job->result = utimes(job->path, job->times_pointer);
    job->error_code = errno;
}

static value result_utimes(struct job_utimes *job)
{
    LWT_UNIX_CHECK_JOB_ARG(job, job->result != 0, "utimes", job->path);
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_utimes_job(value path, value val_atime, value val_mtime)
{
    LWT_UNIX_INIT_JOB_STRING(job, utimes, 0, path);

    double atime = Double_val(val_atime);
    double mtime = Double_val(val_mtime);

    if (atime == 0.0 && mtime == 0.0)
        job->times_pointer = NULL;
    else {
        job->times[0].tv_sec = atime;
        job->times[0].tv_usec = (atime - job->times[0].tv_sec) * 1000000;

        job->times[1].tv_sec = mtime;
        job->times[1].tv_usec = (mtime - job->times[1].tv_sec) * 1000000;

        job->times_pointer = job->times;
    }

    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: isatty                                                     |
   +-----------------------------------------------------------------+ */

struct job_isatty {
    struct lwt_unix_job job;
    int fd;
    int result;
};

static void worker_isatty(struct job_isatty *job)
{
    job->result = isatty(job->fd);
}

static value result_isatty(struct job_isatty *job)
{
    value result = Val_bool(job->result);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_isatty_job(value val_fd)
{
    LWT_UNIX_INIT_JOB(job, isatty, 0);
    job->fd = Int_val(val_fd);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: opendir                                                    |
   +-----------------------------------------------------------------+ */

struct job_opendir {
    struct lwt_unix_job job;
    DIR *result;
    int error_code;
    char *path;
    char data[];
};

static void worker_opendir(struct job_opendir *job)
{
    job->result = opendir(job->path);
    job->error_code = errno;
}

static value result_opendir(struct job_opendir *job)
{
    LWT_UNIX_CHECK_JOB_ARG(job, job->result == NULL, "opendir", job->path);
    value result = caml_alloc_small(1, Abstract_tag);
    DIR_Val(result) = job->result;
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_opendir_job(value path)
{
    LWT_UNIX_INIT_JOB_STRING(job, opendir, 0, path);
    return lwt_unix_alloc_job(&job->job);
}

/* +-----------------------------------------------------------------+
   | JOB: closedir                                                   |
   +-----------------------------------------------------------------+ */

struct job_closedir {
    struct lwt_unix_job job;
    int result;
    int error_code;
    DIR *dir;
};

static void worker_closedir(struct job_closedir *job)
{
    job->result = closedir(job->dir);
    job->error_code = errno;
}

static value result_closedir(struct job_closedir *job)
{
    LWT_UNIX_CHECK_JOB(job, job->dir < 0, "closedir");
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_closedir_job(value dir)
{
    LWT_UNIX_INIT_JOB(job, closedir, 0);
    job->dir = DIR_Val(dir);
    return lwt_unix_alloc_job(&job->job);
}

/* Directory handle validity. */

CAMLprim value lwt_unix_valid_dir(value dir)
{
    CAMLparam1(dir);
    int result = DIR_Val(dir) == NULL ? 0 : 1;
    CAMLreturn(Val_int(result));
}

CAMLprim value lwt_unix_invalidate_dir(value dir)
{
    CAMLparam1(dir);
    DIR_Val(dir) = NULL;
    CAMLreturn(Val_unit);
}

/* +-----------------------------------------------------------------+
   | JOB: rewinddir                                                  |
   +-----------------------------------------------------------------+ */

struct job_rewinddir {
    struct lwt_unix_job job;
    DIR *dir;
};

static void worker_rewinddir(struct job_rewinddir *job) { rewinddir(job->dir); }
static value result_rewinddir(struct job_rewinddir *job)
{
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_rewinddir_job(value dir)
{
    LWT_UNIX_INIT_JOB(job, rewinddir, 0);
    job->dir = DIR_Val(dir);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: readdir                                                    |
   +-----------------------------------------------------------------+ */

struct job_readdir {
    struct lwt_unix_job job;
    DIR *dir;
    struct dirent *entry;
    int error_code;
};

static void worker_readdir(struct job_readdir *job)
{
    // From the man page of readdir
    // If the end of the directory stream is reached, NULL is returned and
    // errno is not changed. If an error occurs, NULL is returned and errno
    // is set appropriately. To distinguish end of stream and from an error,
    // set errno to zero before calling readdir() and then check the value of
    // errno if NULL is returned.
    errno = 0;
    job->entry = readdir(job->dir);
    job->error_code = errno;
}

static value result_readdir(struct job_readdir *job)
{
    LWT_UNIX_CHECK_JOB(job, job->entry == NULL && job->error_code != 0,
                       "readdir");
    if (job->entry == NULL) {
        // From the man page
        // On success, readdir() returns a pointer to a dirent structure.
        // (This structure may be statically allocated; do not attempt to
        // free(3) it.)
        lwt_unix_free_job(&job->job);
        caml_raise_end_of_file();
    } else {
        value name = caml_copy_string(job->entry->d_name);
        // see above about not freeing dirent
        lwt_unix_free_job(&job->job);
        return name;
    }
}

CAMLprim value lwt_unix_readdir_job(value val_dir)
{
    LWT_UNIX_INIT_JOB(job, readdir, 0);
    job->dir = DIR_Val(val_dir);
    return lwt_unix_alloc_job(&job->job);
}

/* +-----------------------------------------------------------------+
   | JOB: readdir_n                                                  |
   +-----------------------------------------------------------------+ */

struct job_readdir_n {
    struct lwt_unix_job job;
    DIR *dir;
    /* count serves two purpose:
       1. Transmit the maximum number of entries requested by the programmer.
          See `readdir_n`'s OCaml side documentation.
       2. Transmit the number of actually read entries from the `worker` to
          the result parser.
          See below `worker_readdir_n` and `result_readdir_n`
    */
    long count;
    int error_code;
    char *entries[];
};

static void worker_readdir_n(struct job_readdir_n *job)
{
    long i;
    for (i = 0; i < job->count; i++) {
        errno = 0;
        struct dirent *entry = readdir(job->dir);

        /* An error happened. */
        if (entry == NULL && errno != 0) {
            job->count = i;
            job->error_code = errno;
            return;
        }

        /* End of directory reached */
        if (entry == NULL && errno == 0) break;

        /* readdir is good */
        char *name = strdup(entry->d_name);
        if (name == NULL) {
            job->count = i;
            job->error_code = errno;
            return;
        }

        /* All is good */
        job->entries[i] = name;
    }
    job->count = i;
    job->error_code = 0;
}

static value result_readdir_n(struct job_readdir_n *job)
{
    CAMLparam0();
    CAMLlocal1(result);
    int error_code = job->error_code;
    if (error_code) {
        long i;
        for (i = 0; i < job->count; i++) free(job->entries[i]);
        lwt_unix_free_job(&job->job);
        unix_error(error_code, "readdir", Nothing);
    } else {
        result = caml_alloc(job->count, 0);
        long i;
        for (i = 0; i < job->count; i++) {
            Store_field(result, i, caml_copy_string(job->entries[i]));
        }
        for (i = 0; i < job->count; i++) free(job->entries[i]);
        lwt_unix_free_job(&job->job);
        CAMLreturn(result);
    }
}

CAMLprim value lwt_unix_readdir_n_job(value val_dir, value val_count)
{
    long count = Long_val(val_count);
    DIR *dir = DIR_Val(val_dir);

    LWT_UNIX_INIT_JOB(job, readdir_n, sizeof(char *) * count);
    job->dir = dir;
    job->count = count;

    return lwt_unix_alloc_job(&job->job);
}

/* +-----------------------------------------------------------------+
   | JOB: readlink                                                   |
   +-----------------------------------------------------------------+ */

struct job_readlink {
    struct lwt_unix_job job;
    char *buffer;
    ssize_t result;
    int error_code;
    char *name;
    char data[];
};

static void worker_readlink(struct job_readlink *job)
{
    ssize_t buffer_size = 1024;
    ssize_t link_length;

    for (;;) {
        job->buffer = lwt_unix_malloc(buffer_size + 1);
        link_length = readlink(job->name, job->buffer, buffer_size);

        if (link_length < 0) {
            free(job->buffer);
            job->result = -1;
            job->error_code = errno;
            return;
        }
        if (link_length < buffer_size) {
            job->buffer[link_length] = 0;
            job->result = link_length;
            return;
        } else {
            free(job->buffer);
            buffer_size *= 2;
        }
    }
}

static value result_readlink(struct job_readlink *job)
{
    LWT_UNIX_CHECK_JOB_ARG(job, job->result < 0, "readlink", job->name);
    value result = caml_copy_string(job->buffer);
    free(job->buffer);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_readlink_job(value name)
{
    LWT_UNIX_INIT_JOB_STRING(job, readlink, 0, name);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: lockf                                                      |
   +-----------------------------------------------------------------+ */

struct job_lockf {
    struct lwt_unix_job job;
    int fd;
    int command;
    long length;
    int result;
    int error_code;
};

#if defined(F_GETLK) && defined(F_SETLK) && defined(F_SETLKW)

static void worker_lockf(struct job_lockf *job)
{
    struct flock l;

    l.l_whence = 1;
    if (job->length < 0) {
        l.l_start = job->length;
        l.l_len = -job->length;
    } else {
        l.l_start = 0L;
        l.l_len = job->length;
    }
    switch (job->command) {
        case 0: /* F_ULOCK */
            l.l_type = F_UNLCK;
            job->result = fcntl(job->fd, F_SETLK, &l);
            job->error_code = errno;
            break;
        case 1: /* F_LOCK */
            l.l_type = F_WRLCK;
            job->result = fcntl(job->fd, F_SETLKW, &l);
            job->error_code = errno;
            break;
        case 2: /* F_TLOCK */
            l.l_type = F_WRLCK;
            job->result = fcntl(job->fd, F_SETLK, &l);
            job->error_code = errno;
            break;
        case 3: /* F_TEST */
            l.l_type = F_WRLCK;
            job->result = fcntl(job->fd, F_GETLK, &l);
            if (job->result != -1) {
                if (l.l_type == F_UNLCK) {
                    job->result = 0;
                } else {
                    job->result = -1;
                    job->error_code = EACCES;
                }
            }
            break;
        case 4: /* F_RLOCK */
            l.l_type = F_RDLCK;
            job->result = fcntl(job->fd, F_SETLKW, &l);
            job->error_code = errno;
            break;
        case 5: /* F_TRLOCK */
            l.l_type = F_RDLCK;
            job->result = fcntl(job->fd, F_SETLK, &l);
            job->error_code = errno;
            break;
        default:
            job->result = -1;
            job->error_code = EINVAL;
    }
}

#else

static int lock_command_table[] = {F_ULOCK, F_LOCK, F_TLOCK,
                                   F_TEST,  F_LOCK, F_TLOCK};

static void worker_lockf(struct job_lockf *job)
{
    job->result = lockf(job->fd, lock_command_table[job->command], job->length);
    job->error_code = errno;
}

#endif

static value result_lockf(struct job_lockf *job)
{
    LWT_UNIX_CHECK_JOB(job, job->result < 0, "lockf");
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_lockf_job(value val_fd, value val_command,
                                  value val_length)
{
    LWT_UNIX_INIT_JOB(job, lockf, 0);
    job->fd = Int_val(val_fd);
    job->command = Int_val(val_command);
    job->length = Long_val(val_length);
    return lwt_unix_alloc_job(&job->job);
}

/* +-----------------------------------------------------------------+
   | JOB: getlogin                                                   |
   +-----------------------------------------------------------------+ */

#if !defined(__ANDROID__)

struct job_getlogin {
    struct lwt_unix_job job;
    char buffer[1024];
    int result;
};

static void worker_getlogin(struct job_getlogin *job)
{
    job->result = getlogin_r(job->buffer, 1024);
}

static value result_getlogin(struct job_getlogin *job)
{
    int result = job->result;
    if (result) {
        lwt_unix_free_job(&job->job);
        unix_error(result, "getlogin", Nothing);
    } else {
        value v = caml_copy_string(job->buffer);
        lwt_unix_free_job(&job->job);
        return v;
    }
}

CAMLprim value lwt_unix_getlogin_job(value Unit)
{
    LWT_UNIX_INIT_JOB(job, getlogin, 0);
    return lwt_unix_alloc_job(&job->job);
}

#else

LWT_NOT_AVAILABLE1(unix_getlogin_job)

#endif

/* +-----------------------------------------------------------------+
   | JOBs: get{pw,gr}{nam,uid}                                       |
   +-----------------------------------------------------------------+ */

#if !defined(__ANDROID__)

static value alloc_passwd_entry(struct passwd *entry)
{
    value res;
    value name = Val_unit, passwd = Val_unit, gecos = Val_unit;
    value dir = Val_unit, shell = Val_unit;

    Begin_roots5(name, passwd, gecos, dir, shell);
    name = caml_copy_string(entry->pw_name);
    passwd = caml_copy_string(entry->pw_passwd);
#if !defined(__BEOS__)
    gecos = caml_copy_string(entry->pw_gecos);
#else
    gecos = caml_copy_string("");
#endif
    dir = caml_copy_string(entry->pw_dir);
    shell = caml_copy_string(entry->pw_shell);
    res = caml_alloc_small(7, 0);
    Field(res, 0) = name;
    Field(res, 1) = passwd;
    Field(res, 2) = Val_int(entry->pw_uid);
    Field(res, 3) = Val_int(entry->pw_gid);
    Field(res, 4) = gecos;
    Field(res, 5) = dir;
    Field(res, 6) = shell;
    End_roots();
    return res;
}

static value alloc_group_entry(struct group *entry)
{
    value res;
    value name = Val_unit, pass = Val_unit, mem = Val_unit;

    Begin_roots3(name, pass, mem);
    name = caml_copy_string(entry->gr_name);
    pass = caml_copy_string(entry->gr_passwd);
    mem = caml_copy_string_array((const char **)entry->gr_mem);
    res = caml_alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = pass;
    Field(res, 2) = Val_int(entry->gr_gid);
    Field(res, 3) = mem;
    End_roots();
    return res;
}

#define JOB_GET_ENTRY(INIT, FUNC, CONF, TYPE, ARG, ARG_DECL, FAIL_ARG) \
    struct job_##FUNC {                                                \
        struct lwt_unix_job job;                                       \
        struct TYPE entry;                                             \
        struct TYPE *ptr;                                              \
        char *buffer;                                                  \
        int result;                                                    \
        ARG_DECL;                                                      \
    };                                                                 \
                                                                       \
    static void worker_##FUNC(struct job_##FUNC *job)                  \
    {                                                                  \
        size_t buffer_size = sysconf(_SC_##CONF##_R_SIZE_MAX);         \
        if (buffer_size == (size_t)-1) buffer_size = 16384;            \
        job->buffer = (char *)lwt_unix_malloc(buffer_size);            \
        job->result = FUNC##_r(job->ARG, &job->entry, job->buffer,     \
                               buffer_size, &job->ptr);                \
    }                                                                  \
                                                                       \
    static value result_##FUNC(struct job_##FUNC *job)                 \
    {                                                                  \
        int result = job->result;                                      \
        if (result) {                                                  \
            value arg = FAIL_ARG;                                      \
            free(job->buffer);                                         \
            lwt_unix_free_job(&job->job);                              \
            unix_error(result, #FUNC, arg);                            \
        } else if (job->ptr == NULL) {                                 \
            free(job->buffer);                                         \
            lwt_unix_free_job(&job->job);                              \
            caml_raise_not_found();                                    \
        } else {                                                       \
            value entry = alloc_##TYPE##_entry(&job->entry);           \
            free(job->buffer);                                         \
            lwt_unix_free_job(&job->job);                              \
            return entry;                                              \
        }                                                              \
    }                                                                  \
                                                                       \
    CAMLprim value lwt_unix_##FUNC##_job(value ARG)                    \
    {                                                                  \
        INIT;                                                          \
        return lwt_unix_alloc_job(&job->job);                          \
    }

JOB_GET_ENTRY(LWT_UNIX_INIT_JOB_STRING(job, getpwnam, 0, name), getpwnam, GETPW,
              passwd, name, char *name;
              char data[], caml_copy_string(job->name))
JOB_GET_ENTRY(LWT_UNIX_INIT_JOB_STRING(job, getgrnam, 0, name), getgrnam, GETGR,
              group, name, char *name;
              char data[], caml_copy_string(job->name))
JOB_GET_ENTRY(LWT_UNIX_INIT_JOB(job, getpwuid, 0); job->uid = Int_val(uid),
                                                   getpwuid, GETPW, passwd, uid,
                                                   int uid, Nothing)
JOB_GET_ENTRY(LWT_UNIX_INIT_JOB(job, getgrgid, 0); job->gid = Int_val(gid),
                                                   getgrgid, GETGR, group, gid,
                                                   int gid, Nothing)

#else

LWT_NOT_AVAILABLE1(unix_getpwnam_job)
LWT_NOT_AVAILABLE1(unix_getgrnam_job)
LWT_NOT_AVAILABLE1(unix_getpwuid_job)
LWT_NOT_AVAILABLE1(unix_getgrgid_job)

#endif

/* Helper functions for not re-entrant functions */

/* keep test in sync with discover.ml */
#if !defined(HAS_GETHOSTBYADDR_R) || \
    (HAS_GETHOSTBYADDR_R != 7 && HAS_GETHOSTBYADDR_R != 8)
#define NON_R_GETHOSTBYADDR 1
#endif

/* keep test in sync with discover.ml */
#if !defined(HAS_GETHOSTBYNAME_R) || \
    (HAS_GETHOSTBYNAME_R != 5 && HAS_GETHOSTBYNAME_R != 6)
#define NON_R_GETHOSTBYNAME 1
#endif

#if defined(NON_R_GETHOSTBYADDR) || defined(NON_R_GETHOSTBYNAME)
static char **c_copy_addr_array(char **src, int addr_len)
{
    if (src == NULL) {
        return NULL;
    }
    char **p = src;
    size_t i = 0;
    while (*p) {
        i++;
        p++;
    }
    const size_t ar_len = i;
    p = malloc((ar_len + 1) * sizeof(char *));
    if (p == NULL) {
        return NULL;
    }
    for (i = 0; i < ar_len; ++i) {
        p[i] = malloc(addr_len);
        if (p[i] == NULL) {
            size_t j;
            for (j = 0; j < i; j++) {
                free(p[j]);
            }
            free(p);
            return NULL;
        }
        memcpy(p[i], src[i], addr_len);
    }
    p[ar_len] = NULL;
    return p;
}
#endif
#if !defined(HAVE_NETDB_REENTRANT) || defined(NON_R_GETHOSTBYADDR) || \
    defined(NON_R_GETHOSTBYNAME)
static char **c_copy_string_array(char **src)
{
    char **p = src;
    size_t i = 0;
    size_t len;
    if (src == NULL) {
        return NULL;
    }
    while (*p) {
        i++;
        p++;
    }
    len = i;
    p = malloc((len + 1) * sizeof(char *));
    if (p == NULL) {
        return NULL;
    }
    for (i = 0; i < len; ++i) {
        p[i] = strdup(src[i]);
        if (p[i] == NULL) {
            size_t j;
            for (j = 0; j < i; j++) {
                free(p[j]);
            }
            free(p);
            return NULL;
        }
    }
    p[len] = NULL;
    return p;
}

static void c_free_string_array(char **src)
{
    if (src) {
        char **p = src;
        while (*p) {
            free(*p);
            ++p;
        }
        free(src);
    }
}

static inline char *s_strdup(const char *s)
{
    return (strdup(s == NULL ? "" : s));
}
#endif

/* +-----------------------------------------------------------------+
   | JOB: gethostname                                                |
   +-----------------------------------------------------------------+ */

struct job_gethostname {
    struct lwt_unix_job job;
    char *buffer;
    int result;
    int error_code;
};

static void worker_gethostname(struct job_gethostname *job)
{
    int buffer_size = 64;
    int err;

    for (;;) {
        job->buffer = lwt_unix_malloc(buffer_size + 1);

        err = gethostname(job->buffer, buffer_size);

        if (err == -1 && errno == ENAMETOOLONG) {
            free(job->buffer);
            buffer_size *= 2;
        } else if (err == -1) {
            free(job->buffer);
            job->result = -1;
            job->error_code = errno;
            return;
        } else {
            job->buffer[buffer_size] = 0;
            job->result = 0;
            return;
        }
    }
}

static value result_gethostname(struct job_gethostname *job)
{
    LWT_UNIX_CHECK_JOB(job, job->result < 0, "gethostname");
    value result = caml_copy_string(job->buffer);
    free(job->buffer);
    lwt_unix_free_job(&job->job);
    return result;
}

CAMLprim value lwt_unix_gethostname_job(value Unit)
{
    LWT_UNIX_INIT_JOB(job, gethostname, 0);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: gethostbyname                                              |
   +-----------------------------------------------------------------+ */

#define NETDB_BUFFER_SIZE 10000

struct job_gethostbyname {
    struct lwt_unix_job job;
    struct hostent entry;
    struct hostent *ptr;
#ifndef NON_R_GETHOSTBYNAME
    char buffer[NETDB_BUFFER_SIZE];
#endif
    char *name;
    char data[];
};

CAMLexport value alloc_inet_addr(struct in_addr *inaddr);
CAMLexport value alloc_inet6_addr(struct in6_addr *inaddr);

static value alloc_one_addr(char const *a)
{
    struct in_addr addr;
    memmove(&addr, a, 4);
    return alloc_inet_addr(&addr);
}

static value alloc_one_addr6(char const *a)
{
    struct in6_addr addr;
    memmove(&addr, a, 16);
    return alloc_inet6_addr(&addr);
}

static value alloc_host_entry(struct hostent *entry)
{
    value res;
    value name = Val_unit, aliases = Val_unit;
    value addr_list = Val_unit, adr = Val_unit;

    Begin_roots4(name, aliases, addr_list, adr);
    name = caml_copy_string((char *)(entry->h_name));
    /* PR#4043: protect against buggy implementations of gethostbynamee()
       that return a NULL pointer in h_aliases */
    if (entry->h_aliases)
        aliases = caml_copy_string_array((const char **)entry->h_aliases);
    else
        aliases = Atom(0);
    if (entry->h_length == 16)
        addr_list = caml_alloc_array(alloc_one_addr6,
                                     (const char **)entry->h_addr_list);
    else
        addr_list =
            caml_alloc_array(alloc_one_addr, (const char **)entry->h_addr_list);
    res = caml_alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = aliases;
    switch (entry->h_addrtype) {
        case PF_UNIX:
            Field(res, 2) = Val_int(0);
            break;
        case PF_INET:
            Field(res, 2) = Val_int(1);
            break;
        default: /*PF_INET6 */
            Field(res, 2) = Val_int(2);
            break;
    }
    Field(res, 3) = addr_list;
    End_roots();
    return res;
}

#if defined(NON_R_GETHOSTBYADDR) || defined(NON_R_GETHOSTBYNAME)
static struct hostent *hostent_dup(struct hostent *orig)
{
    if (orig == NULL) {
        return NULL;
    }
    struct hostent *h = malloc(sizeof *h);
    if (h == NULL) {
        return NULL;
    }
    h->h_name = s_strdup(orig->h_name);
    if (!h->h_name) {
        goto nomem1;
    }
    if (!orig->h_aliases) {
        h->h_aliases = NULL;
    } else {
        h->h_aliases = c_copy_string_array(orig->h_aliases);
        if (!h->h_aliases) {
            goto nomem2;
        }
    }
    if (!orig->h_addr_list) {
        h->h_addr_list = NULL;
    } else {
        h->h_addr_list = c_copy_addr_array(orig->h_addr_list, orig->h_length);
        if (!h->h_addr_list) {
            goto nomem3;
        }
    }
    h->h_addrtype = orig->h_addrtype;
    h->h_length = orig->h_length;
    return h;
nomem3:
    c_free_string_array(h->h_aliases);
nomem2:
    free((char *)h->h_name);
nomem1:
    free(h);
    return NULL;
}

static void hostent_free(struct hostent *h)
{
    if (h) {
        c_free_string_array(h->h_addr_list);
        c_free_string_array(h->h_aliases);
        free((char *)h->h_name);
        free(h);
    }
}
#endif

static void worker_gethostbyname(struct job_gethostbyname *job)
{
#if HAS_GETHOSTBYNAME_R == 5
    int h_errno;
    job->ptr = gethostbyname_r(job->name, &job->entry, job->buffer,
                               NETDB_BUFFER_SIZE, &h_errno);
#elif HAS_GETHOSTBYNAME_R == 6
    int h_errno;
    if (gethostbyname_r(job->name, &job->entry, job->buffer, NETDB_BUFFER_SIZE,
                        &(job->ptr), &h_errno) != 0)
        job->ptr = NULL;
#else
    job->ptr = gethostbyname(job->name);
    if (job->ptr) {
        job->ptr = hostent_dup(job->ptr);
        if (job->ptr) {
            job->entry = *job->ptr;
        }
    }
#endif
}

static value result_gethostbyname(struct job_gethostbyname *job)
{
    if (job->ptr == NULL) {
        lwt_unix_free_job(&job->job);
        caml_raise_not_found();
    } else {
        value entry = alloc_host_entry(&job->entry);
#ifdef NON_R_GETHOSTBYNAME
        hostent_free(job->ptr);
#endif
        lwt_unix_free_job(&job->job);
        return entry;
    }
}

CAMLprim value lwt_unix_gethostbyname_job(value name)
{
    LWT_UNIX_INIT_JOB_STRING(job, gethostbyname, 0, name);
    return lwt_unix_alloc_job(&(job->job));
}

/* +-----------------------------------------------------------------+
   | JOB: gethostbyaddr                                              |
   +-----------------------------------------------------------------+ */

struct job_gethostbyaddr {
    struct lwt_unix_job job;
    struct in_addr addr;
    struct hostent entry;
    struct hostent *ptr;
#ifndef NON_R_GETHOSTBYADDR
    char buffer[NETDB_BUFFER_SIZE];
#endif
};

static void worker_gethostbyaddr(struct job_gethostbyaddr *job)
{
#if HAS_GETHOSTBYADDR_R == 7
    int h_errno;
    job->ptr = gethostbyaddr_r(&job->addr, 4, AF_INET, &job->entry, job->buffer,
                               NETDB_BUFFER_SIZE, &h_errno);
#elif HAS_GETHOSTBYADDR_R == 8
    int h_errno;
    if (gethostbyaddr_r(&job->addr, 4, AF_INET, &job->entry, job->buffer,
                        NETDB_BUFFER_SIZE, &job->ptr, &h_errno) != 0)
        job->ptr = NULL;
#else
    job->ptr = gethostbyaddr(&job->addr, 4, AF_INET);
    if (job->ptr) {
        job->ptr = hostent_dup(job->ptr);
        if (job->ptr) {
            job->entry = *job->ptr;
        }
    }
#endif
}

static value result_gethostbyaddr(struct job_gethostbyaddr *job)
{
    if (job->ptr == NULL) {
        lwt_unix_free_job(&job->job);
        caml_raise_not_found();
    } else {
        value entry = alloc_host_entry(&job->entry);
#ifdef NON_R_GETHOSTBYADDR
        hostent_free(job->ptr);
#endif
        lwt_unix_free_job(&job->job);
        return entry;
    }
}

CAMLprim value lwt_unix_gethostbyaddr_job(value val_addr)
{
    LWT_UNIX_INIT_JOB(job, gethostbyaddr, 0);
    job->addr = GET_INET_ADDR(val_addr);
    return lwt_unix_alloc_job(&job->job);
}

/* +-----------------------------------------------------------------+
   | JOBs: getprotoby{name,number}, getservby{name,port}             |
   +-----------------------------------------------------------------+ */

static value alloc_protoent(struct protoent *entry)
{
    value res;
    value name = Val_unit, aliases = Val_unit;

    Begin_roots2(name, aliases);
    name = caml_copy_string(entry->p_name);
    aliases = caml_copy_string_array((const char **)entry->p_aliases);
    res = caml_alloc_small(3, 0);
    Field(res, 0) = name;
    Field(res, 1) = aliases;
    Field(res, 2) = Val_int(entry->p_proto);
    End_roots();
    return res;
}

static value alloc_servent(struct servent *entry)
{
    value res;
    value name = Val_unit, aliases = Val_unit, proto = Val_unit;

    Begin_roots3(name, aliases, proto);
    name = caml_copy_string(entry->s_name);
    aliases = caml_copy_string_array((const char **)entry->s_aliases);
    proto = caml_copy_string(entry->s_proto);
    res = caml_alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = aliases;
    Field(res, 2) = Val_int(ntohs(entry->s_port));
    Field(res, 3) = proto;
    End_roots();
    return res;
}

#if defined(HAVE_NETDB_REENTRANT)

#define JOB_GET_ENTRY2(INIT, FUNC, TYPE, ARGS_VAL, ARGS_DECL, ARGS_CALL)     \
    struct job_##FUNC {                                                      \
        struct lwt_unix_job job;                                             \
        struct TYPE entry;                                                   \
        struct TYPE *ptr;                                                    \
        char *buffer;                                                        \
        ARGS_DECL;                                                           \
    };                                                                       \
                                                                             \
    static void worker_##FUNC(struct job_##FUNC *job)                        \
    {                                                                        \
        size_t size = 1024;                                                  \
        for (;;) {                                                           \
            job->buffer = (char *)lwt_unix_malloc(size);                     \
                                                                             \
            int result = FUNC##_r(ARGS_CALL, &job->entry, job->buffer, size, \
                                  &job->ptr);                                \
                                                                             \
            switch (result) {                                                \
                case 0:                                                      \
                    return;                                                  \
                case ERANGE:                                                 \
                    free(job->buffer);                                       \
                    size += 1024;                                            \
                    break;                                                   \
                case ENOENT:                                                 \
                default:                                                     \
                    job->ptr = NULL;                                         \
                    return;                                                  \
            }                                                                \
        }                                                                    \
    }                                                                        \
                                                                             \
    static value result_##FUNC(struct job_##FUNC *job)                       \
    {                                                                        \
        if (job->ptr == NULL) {                                              \
            free(job->buffer);                                               \
            lwt_unix_free_job(&job->job);                                    \
            caml_raise_not_found();                                          \
        } else {                                                             \
            value res = alloc_##TYPE(&job->entry);                           \
            free(job->buffer);                                               \
            lwt_unix_free_job(&job->job);                                    \
            return res;                                                      \
        }                                                                    \
    }                                                                        \
                                                                             \
    CAMLprim value lwt_unix_##FUNC##_job(ARGS_VAL)                           \
    {                                                                        \
        INIT;                                                                \
        return lwt_unix_alloc_job(&(job->job));                              \
    }

#else /* defined(HAVE_NETDB_REENTRANT) */

static struct servent *servent_dup(const struct servent *serv)
{
    struct servent *s;
    if (!serv) {
        return NULL;
    }
    s = malloc(sizeof *s);
    if (s == NULL) {
        goto nomem1;
    }
    s->s_name = s_strdup(serv->s_name);
    if (s->s_name == NULL) {
        goto nomem2;
    }
    s->s_proto = s_strdup(serv->s_proto);
    if (s->s_proto == NULL) {
        goto nomem3;
    }
    s->s_aliases = c_copy_string_array(serv->s_aliases);
    if (s->s_aliases == NULL && serv->s_aliases != NULL) {
        goto nomem4;
    }
    s->s_port = serv->s_port;
    return s;
nomem4:
    free(s->s_proto);
nomem3:
    free(s->s_name);
nomem2:
    free(s);
nomem1:
    return NULL;
}

static void servent_free(struct servent *s)
{
    if (!s) {
        return;
    }
    free(s->s_proto);
    free(s->s_name);
    c_free_string_array(s->s_aliases);
    free(s);
}

static struct protoent *protoent_dup(const struct protoent *proto)
{
    if (!proto) {
        return NULL;
    }
    struct protoent *p = malloc(sizeof *p);
    if (p == NULL) {
        return NULL;
    }
    p->p_name = s_strdup(proto->p_name);
    if (p->p_name == NULL) {
        goto nomem1;
    }
    p->p_aliases = c_copy_string_array(proto->p_aliases);
    if (p->p_aliases == NULL && proto->p_aliases != NULL) {
        goto nomem2;
    }
    p->p_proto = proto->p_proto;
    return p;
nomem2:
    free(p->p_name);
nomem1:
    free(p);
    return NULL;
}

static void protoent_free(struct protoent *p)
{
    if (p) {
        free(p->p_name);
        c_free_string_array(p->p_aliases);
        free(p);
    }
}

#define JOB_GET_ENTRY2(INIT, FUNC, TYPE, ARGS_VAL, ARGS_DECL, ARGS_CALL) \
    struct job_##FUNC {                                                  \
        struct lwt_unix_job job;                                         \
        struct TYPE *entry;                                              \
        ARGS_DECL;                                                       \
    };                                                                   \
                                                                         \
    static void worker_##FUNC(struct job_##FUNC *job)                    \
    {                                                                    \
        job->entry = FUNC(ARGS_CALL);                                    \
        if (job->entry) {                                                \
            job->entry = TYPE##_dup(job->entry);                         \
            if (!job->entry) {                                           \
            }                                                            \
        }                                                                \
    }                                                                    \
                                                                         \
    static value result_##FUNC(struct job_##FUNC *job)                   \
    {                                                                    \
        if (job->entry == NULL) {                                        \
            lwt_unix_free_job(&job->job);                                \
            caml_raise_not_found();                                      \
        } else {                                                         \
            value res = alloc_##TYPE(job->entry);                        \
            TYPE##_free(job->entry);                                     \
            lwt_unix_free_job(&job->job);                                \
            return res;                                                  \
        }                                                                \
    }                                                                    \
                                                                         \
    CAMLprim value lwt_unix_##FUNC##_job(ARGS_VAL)                       \
    {                                                                    \
        INIT;                                                            \
        return lwt_unix_alloc_job(&(job->job));                          \
    }

#endif /* defined(HAVE_NETDB_REENTRANT) */

JOB_GET_ENTRY2(LWT_UNIX_INIT_JOB_STRING(job, getprotobyname, 0, name),
               getprotobyname, protoent, value name, char *name;
               char data[], job->name)
JOB_GET_ENTRY2(LWT_UNIX_INIT_JOB(job, getprotobynumber, 0);
               job->num = Int_val(num), getprotobynumber, protoent, value num,
               int num, job->num)
JOB_GET_ENTRY2(LWT_UNIX_INIT_JOB_STRING2(job, getservbyname, 0, name, proto),
               getservbyname, servent, ARGS(value name, value proto),
               char *name;
               char *proto; char data[], ARGS(job->name, job->proto))
JOB_GET_ENTRY2(LWT_UNIX_INIT_JOB_STRING(job, getservbyport, 0, proto);
               job->port = htons(Int_val(port)), getservbyport, servent,
               ARGS(value port, value proto), int port;
               char *proto; char data[], ARGS(job->port, job->proto))

/* +-----------------------------------------------------------------+
   | JOB: getaddrinfo                                                |
   +-----------------------------------------------------------------+ */

struct job_getaddrinfo {
    struct lwt_unix_job job;
    char *node;
    char *service;
    struct addrinfo hints;
    struct addrinfo *info;
    int result;
    char data[];
};

static value cst_to_constr(int n, int *tbl, int size, int deflt)
{
    int i;
    for (i = 0; i < size; i++)
        if (n == tbl[i]) return Val_int(i);
    return Val_int(deflt);
}
// TODO remember to include unix_recv_send_utils.h for socket_domain_table
static value convert_addrinfo(struct addrinfo *a)
{
    CAMLparam0();
    CAMLlocal3(vres, vaddr, vcanonname);
    union sock_addr_union sa;
    socklen_t len;

    len = a->ai_addrlen;
    if (len > sizeof(sa)) len = sizeof(sa);
    memcpy(&sa.s_gen, a->ai_addr, len);
    vaddr = alloc_sockaddr(&sa, len, -1);
    vcanonname =
        caml_copy_string(a->ai_canonname == NULL ? "" : a->ai_canonname);
    vres = caml_alloc_small(5, 0);
    Field(vres, 0) = cst_to_constr(a->ai_family, socket_domain_table, 3, 0);
    Field(vres, 1) = cst_to_constr(a->ai_socktype, socket_type_table, 4, 0);
    Field(vres, 2) = Val_int(a->ai_protocol);
    Field(vres, 3) = vaddr;
    Field(vres, 4) = vcanonname;
    CAMLreturn(vres);
}

static void worker_getaddrinfo(struct job_getaddrinfo *job)
{
    job->result = getaddrinfo(job->node[0] ? job->node : NULL,
                              job->service[0] ? job->service : NULL,
                              &job->hints, &job->info);
}

static value result_getaddrinfo(struct job_getaddrinfo *job)
{
    CAMLparam0();
    CAMLlocal3(vres, e, v);
    vres = Val_int(0);
    if (job->result == 0) {
        struct addrinfo *r;
        for (r = job->info; r; r = r->ai_next) {
            e = convert_addrinfo(r);
            v = caml_alloc_small(2, 0);
            Field(v, 0) = e;
            Field(v, 1) = vres;
            vres = v;
        }
    }
    if (job->info != NULL) freeaddrinfo(job->info);
    lwt_unix_free_job(&job->job);
    CAMLreturn(vres);
}
// TODO remember to include unix_recv_send_utils.h for socket_domain_table

CAMLprim value lwt_unix_getaddrinfo_job(value node, value service, value hints)
{
    LWT_UNIX_INIT_JOB_STRING2(job, getaddrinfo, 0, node, service);
    job->info = NULL;
    memset(&job->hints, 0, sizeof(struct addrinfo));
    job->hints.ai_family = PF_UNSPEC;
    for (/*nothing*/; Is_block(hints); hints = Field(hints, 1)) {
        value v = Field(hints, 0);
        if (Is_block(v)) switch (Tag_val(v)) {
                case 0: /* AI_FAMILY of socket_domain */
                    job->hints.ai_family =
                        socket_domain_table[Int_val(Field(v, 0))];
                    break;
                case 1: /* AI_SOCKTYPE of socket_type */
                    job->hints.ai_socktype =
                        socket_type_table[Int_val(Field(v, 0))];
                    break;
                case 2: /* AI_PROTOCOL of int */
                    job->hints.ai_protocol = Int_val(Field(v, 0));
                    break;
            }
        else
            switch (Int_val(v)) {
                case 0: /* AI_NUMERICHOST */
                    job->hints.ai_flags |= AI_NUMERICHOST;
                    break;
                case 1: /* AI_CANONNAME */
                    job->hints.ai_flags |= AI_CANONNAME;
                    break;
                case 2: /* AI_PASSIVE */
                    job->hints.ai_flags |= AI_PASSIVE;
                    break;
            }
    }
    return lwt_unix_alloc_job(&job->job);
}

/* +-----------------------------------------------------------------+
   | JOB: getnameinfo                                                |
   +-----------------------------------------------------------------+ */

struct job_getnameinfo {
    struct lwt_unix_job job;
    union sock_addr_union addr;
    socklen_t addr_len;
    int opts;
    char host[4096];
    char serv[1024];
    int result;
};

static int getnameinfo_flag_table[] = {NI_NOFQDN, NI_NUMERICHOST, NI_NAMEREQD,
                                       NI_NUMERICSERV, NI_DGRAM};

static void worker_getnameinfo(struct job_getnameinfo *job)
{
    job->result = getnameinfo((const struct sockaddr *)&job->addr.s_gen,
                              job->addr_len, job->host, sizeof(job->host),
                              job->serv, sizeof(job->serv), job->opts);
}

static value result_getnameinfo(struct job_getnameinfo *job)
{
    CAMLparam0();
    CAMLlocal3(vres, vhost, vserv);
    if (job->result) {
        lwt_unix_free_job(&job->job);
        caml_raise_not_found();
    } else {
        vhost = caml_copy_string(job->host);
        vserv = caml_copy_string(job->serv);
        vres = caml_alloc_small(2, 0);
        Field(vres, 0) = vhost;
        Field(vres, 1) = vserv;
        lwt_unix_free_job(&job->job);
        CAMLreturn(vres);
    }
}

CAMLprim value lwt_unix_getnameinfo_job(value sockaddr, value opts)
{
    LWT_UNIX_INIT_JOB(job, getnameinfo, 0);
    get_sockaddr(sockaddr, &job->addr, &job->addr_len);
    job->opts = caml_convert_flag_list(opts, getnameinfo_flag_table);
    return lwt_unix_alloc_job(&job->job);
}

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
