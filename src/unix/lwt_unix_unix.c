/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_unix
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

/* Unix (non windows) version of stubs. */

/* +-----------------------------------------------------------------+
   | Test for readability/writability                                |
   +-----------------------------------------------------------------+ */

#include <poll.h>

CAMLprim value lwt_unix_readable(value fd)
{
  struct pollfd pollfd;
  pollfd.fd = Int_val(fd);
  pollfd.events = POLLIN;
  pollfd.revents = 0;
  if (poll(&pollfd, 1, 0) < 0)
    uerror("readable", Nothing);
  return (Val_bool(pollfd.revents & POLLIN));
}

CAMLprim value lwt_unix_writable(value fd)
{
  struct pollfd pollfd;
  pollfd.fd = Int_val(fd);
  pollfd.events = POLLOUT;
  pollfd.revents = 0;
  if (poll(&pollfd, 1, 0) < 0)
    uerror("readable", Nothing);
  return (Val_bool(pollfd.revents & POLLOUT));
}

/* +-----------------------------------------------------------------+
   | Memory mapped files                                             |
   +-----------------------------------------------------------------+ */

static int advise_table[] = {
  MADV_NORMAL,
  MADV_RANDOM,
  MADV_SEQUENTIAL,
  MADV_WILLNEED,
  MADV_DONTNEED,
};

CAMLprim value lwt_unix_madvise (value val_buffer, value val_offset, value val_length, value val_advice)
{
  int ret = madvise((char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset),
                    Long_val(val_length),
                    advise_table[Int_val(val_advice)]);
  if (ret == -1)  uerror("madvise", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_get_page_size()
{
  long page_size = sysconf(_SC_PAGESIZE);
  if (page_size < 0) page_size = 4096;
  return Val_long(page_size);
}

CAMLprim value lwt_unix_mincore(value val_buffer, value val_offset, value val_length, value val_states)
{
  long len = Wosize_val(val_states);
  unsigned char vec[len];
  mincore((char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset), Long_val(val_length), vec);
  long i;
  for (i = 0; i < len; i++)
    Field(val_states, i) = Val_bool(vec[i] & 1);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | read/write                                                      |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_unix_read(value val_fd, value val_buf, value val_ofs, value val_len)
{
  int ret;
  ret = read(Int_val(val_fd), &Byte(String_val(val_buf), Long_val(val_ofs)), Long_val(val_len));
  if (ret == -1) uerror("read", Nothing);
  return Val_int(ret);
}

CAMLprim value lwt_unix_bytes_read(value val_fd, value val_buf, value val_ofs, value val_len)
{
  int ret;
  ret = read(Int_val(val_fd), (char*)Caml_ba_array_val(val_buf)->data + Long_val(val_ofs), Long_val(val_len));
  if (ret == -1) uerror("read", Nothing);
  return Val_int(ret);
}

CAMLprim value lwt_unix_write(value val_fd, value val_buf, value val_ofs, value val_len)
{
  int ret;
  ret = write(Int_val(val_fd), &Byte(String_val(val_buf), Long_val(val_ofs)), Long_val(val_len));
  if (ret == -1) uerror("write", Nothing);
  return Val_int(ret);
}

CAMLprim value lwt_unix_bytes_write(value val_fd, value val_buf, value val_ofs, value val_len)
{
  int ret;
  ret = write(Int_val(val_fd), (char*)Caml_ba_array_val(val_buf)->data + Long_val(val_ofs), Long_val(val_len));
  if (ret == -1) uerror("write", Nothing);
  return Val_int(ret);
}

/* +-----------------------------------------------------------------+
   | recv/send                                                       |
   +-----------------------------------------------------------------+ */

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

value lwt_unix_recv(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = recv(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len),
             convert_flag_list(flags, msg_flag_table));
  if (ret == -1) uerror("recv", Nothing);
  return Val_int(ret);
}

value lwt_unix_bytes_recv(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = recv(Int_val(fd), (char*)Caml_ba_array_val(buf)->data + Long_val(ofs), Long_val(len),
             convert_flag_list(flags, msg_flag_table));
  if (ret == -1) uerror("recv", Nothing);
  return Val_int(ret);
}

value lwt_unix_send(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = send(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len),
             convert_flag_list(flags, msg_flag_table));
  if (ret == -1) uerror("send", Nothing);
  return Val_int(ret);
}

value lwt_unix_bytes_send(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = send(Int_val(fd), (char*)Caml_ba_array_val(buf)->data + Long_val(ofs), Long_val(len),
             convert_flag_list(flags, msg_flag_table));
  if (ret == -1) uerror("send", Nothing);
  return Val_int(ret);
}

/* +-----------------------------------------------------------------+
   | recvfrom/sendto                                                 |
   +-----------------------------------------------------------------+ */

extern int socket_domain_table[];
extern int socket_type_table[];

union sock_addr_union {
  struct sockaddr s_gen;
  struct sockaddr_un s_unix;
  struct sockaddr_in s_inet;
  struct sockaddr_in6 s_inet6;
};

CAMLexport value alloc_sockaddr (union sock_addr_union * addr /*in*/,
                                 socklen_t addr_len, int close_on_error);

value lwt_unix_recvfrom(value fd, value buf, value ofs, value len, value flags)
{
  CAMLparam5(fd, buf, ofs, len, flags);
  CAMLlocal2(result, address);
  int ret;
  union sock_addr_union addr;
  socklen_t addr_len;
  addr_len = sizeof(addr);
  ret = recvfrom(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len),
                 convert_flag_list(flags, msg_flag_table),
                 &addr.s_gen, &addr_len);
  if (ret == -1) uerror("recvfrom", Nothing);
  address = alloc_sockaddr(&addr, addr_len, -1);
  result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(ret);
  Field(result, 1) = address;
  CAMLreturn(result);
}

value lwt_unix_bytes_recvfrom(value fd, value buf, value ofs, value len, value flags)
{
  CAMLparam5(fd, buf, ofs, len, flags);
  CAMLlocal2(result, address);
  int ret;
  union sock_addr_union addr;
  socklen_t addr_len;
  addr_len = sizeof(addr);
  ret = recvfrom(Int_val(fd), (char*)Caml_ba_data_val(buf) + Long_val(ofs), Long_val(len),
                 convert_flag_list(flags, msg_flag_table),
                 &addr.s_gen, &addr_len);
  if (ret == -1) uerror("recvfrom", Nothing);
  address = alloc_sockaddr(&addr, addr_len, -1);
  result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(ret);
  Field(result, 1) = address;
  CAMLreturn(result);
}

extern void get_sockaddr (value mladdr,
                          union sock_addr_union * addr /*out*/,
                          socklen_t * addr_len /*out*/);

value lwt_unix_sendto(value fd, value buf, value ofs, value len, value flags, value dest)
{
  union sock_addr_union addr;
  socklen_t addr_len;
  int ret;
  get_sockaddr(dest, &addr, &addr_len);
  ret = sendto(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len),
               convert_flag_list(flags, msg_flag_table),
               &addr.s_gen, addr_len);
  if (ret == -1) uerror("send", Nothing);
  return Val_int(ret);
}

CAMLprim value lwt_unix_sendto_byte(value *argv, int argc)
{
  return lwt_unix_sendto(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value lwt_unix_bytes_sendto(value fd, value buf, value ofs, value len, value flags, value dest)
{
  union sock_addr_union addr;
  socklen_t addr_len;
  int ret;
  get_sockaddr(dest, &addr, &addr_len);
  ret = sendto(Int_val(fd), (char*)Caml_ba_data_val(buf) + Long_val(ofs), Long_val(len),
               convert_flag_list(flags, msg_flag_table),
               &addr.s_gen, addr_len);
  if (ret == -1) uerror("send", Nothing);
  return Val_int(ret);
}

CAMLprim value lwt_unix_bytes_sendto_byte(value *argv, int argc)
{
  return lwt_unix_bytes_sendto(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

/* +-----------------------------------------------------------------+
   | {recv/send}_msg                                                 |
   +-----------------------------------------------------------------+ */

/* Convert a caml list of io-vectors into a C array io io-vector
   structures */
static void store_iovs(struct iovec *iovs, value iovs_val)
{
  CAMLparam0();
  CAMLlocal2(list, x);
  for(list = iovs_val; Is_block(list); list = Field(list, 1), iovs++) {
    x = Field(list, 0);
    iovs->iov_base = &Byte(String_val(Field(x, 0)), Long_val(Field(x, 1)));
    iovs->iov_len = Long_val(Field(x, 2));
  }
  CAMLreturn0;
}

static void bytes_store_iovs(struct iovec *iovs, value iovs_val)
{
  CAMLparam0();
  CAMLlocal2(list, x);
  for(list = iovs_val; Is_block(list); list = Field(list, 1), iovs++) {
    x = Field(list, 0);
    iovs->iov_base = (char*)Caml_ba_data_val(Field(x, 0)) + Long_val(Field(x, 1));
    iovs->iov_len = Long_val(Field(x, 2));
  }
  CAMLreturn0;
}

static value wrapper_recv_msg(int fd, int n_iovs, struct iovec *iovs)
{
  CAMLparam0();
  CAMLlocal3(list, result, x);

  struct msghdr msg;
  memset(&msg, 0, sizeof(msg));
  msg.msg_iov = iovs;
  msg.msg_iovlen = n_iovs;
#if defined(HAVE_FD_PASSING)
  msg.msg_controllen = CMSG_SPACE(256 * sizeof(int));
  msg.msg_control = alloca(msg.msg_controllen);
  memset(msg.msg_control, 0, msg.msg_controllen);
#endif

  int ret = recvmsg(fd, &msg, 0);
  if (ret == -1) uerror("recv_msg", Nothing);

  list = Val_int(0);
#if defined(HAVE_FD_PASSING)
  struct cmsghdr *cm;
  for (cm = CMSG_FIRSTHDR(&msg); cm; cm = CMSG_NXTHDR(&msg, cm))
    if (cm->cmsg_level == SOL_SOCKET && cm->cmsg_type == SCM_RIGHTS) {
      int *fds = (int*)CMSG_DATA(cm);
      int nfds = (cm->cmsg_len - CMSG_LEN(0)) / sizeof(int);
      int i;
      for(i = nfds - 1; i >= 0; i--) {
        x = caml_alloc_tuple(2);
        Store_field(x, 0, Val_int(fds[i]));
        Store_field(x, 1, list);
        list = x;
      };
      break;
    };
#endif

  result = caml_alloc_tuple(2);
  Store_field(result, 0, Val_int(ret));
  Store_field(result, 1, list);
  CAMLreturn(result);
}

CAMLprim value lwt_unix_recv_msg(value val_fd, value val_n_iovs, value val_iovs)
{
  int n_iovs = Int_val(val_n_iovs);
  struct iovec iovs[n_iovs];
  store_iovs(iovs, val_iovs);
  return wrapper_recv_msg(Int_val(val_fd), n_iovs, iovs);
}

CAMLprim value lwt_unix_bytes_recv_msg(value val_fd, value val_n_iovs, value val_iovs)
{
  int n_iovs = Int_val(val_n_iovs);
  struct iovec iovs[n_iovs];
  bytes_store_iovs(iovs, val_iovs);
  return wrapper_recv_msg(Int_val(val_fd), n_iovs, iovs);
}

static value wrapper_send_msg(int fd, int n_iovs, struct iovec *iovs, value val_n_fds, value val_fds)
{
  CAMLparam2(val_n_fds, val_fds);

  struct msghdr msg;
  memset(&msg, 0, sizeof(msg));
  msg.msg_iov = iovs;
  msg.msg_iovlen = n_iovs;

  int n_fds = Int_val(val_n_fds);
#if defined(HAVE_FD_PASSING)
  if (n_fds > 0) {
    msg.msg_controllen = CMSG_SPACE(n_fds * sizeof(int));
    msg.msg_control = alloca(msg.msg_controllen);
    memset(msg.msg_control, 0, msg.msg_controllen);

    struct cmsghdr *cm;
    cm = CMSG_FIRSTHDR(&msg);
    cm->cmsg_level = SOL_SOCKET;
    cm->cmsg_type = SCM_RIGHTS;
    cm->cmsg_len = CMSG_LEN(n_fds * sizeof(int));

    int *fds = (int*)CMSG_DATA(cm);
    for(; Is_block(val_fds); val_fds = Field(val_fds, 1), fds++)
      *fds = Int_val(Field(val_fds, 0));
  };
#else
  if (n_fds > 0) lwt_unix_not_available("fd_passing");
#endif

  int ret = sendmsg(fd, &msg, 0);
  if (ret == -1) uerror("send_msg", Nothing);
  CAMLreturn(Val_int(ret));
}

CAMLprim value lwt_unix_send_msg(value val_fd, value val_n_iovs, value val_iovs, value val_n_fds, value val_fds)
{
  int n_iovs = Int_val(val_n_iovs);
  struct iovec iovs[n_iovs];
  store_iovs(iovs, val_iovs);
  return wrapper_send_msg(Int_val(val_fd), n_iovs, iovs, val_n_fds, val_fds);
}

CAMLprim value lwt_unix_bytes_send_msg(value val_fd, value val_n_iovs, value val_iovs, value val_n_fds, value val_fds)
{
  int n_iovs = Int_val(val_n_iovs);
  struct iovec iovs[n_iovs];
  bytes_store_iovs(iovs, val_iovs);
  return wrapper_send_msg(Int_val(val_fd), n_iovs, iovs, val_n_fds, val_fds);
}

/* +-----------------------------------------------------------------+
   | Credentials                                                     |
   +-----------------------------------------------------------------+ */

#if defined(HAVE_GET_CREDENTIALS)

#include <sys/un.h>

CAMLprim value lwt_unix_get_credentials(value fd)
{
    CAMLparam1(fd);
    CAMLlocal1(res);
    struct ucred cred;
    socklen_t cred_len = sizeof(cred);

    if (getsockopt(Int_val(fd), SOL_SOCKET, SO_PEERCRED, &cred, &cred_len) == -1)
      uerror("get_credentials", Nothing);

    res = caml_alloc_tuple(3);
    Store_field(res, 0, Val_int(cred.pid));
    Store_field(res, 1, Val_int(cred.uid));
    Store_field(res, 2, Val_int(cred.gid));
    CAMLreturn(res);
}

#endif

/* +-----------------------------------------------------------------+
   | wait4                                                           |
   +-----------------------------------------------------------------+ */

/* Some code duplicated from OCaml's otherlibs/unix/wait.c */

#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>

CAMLextern int caml_convert_signal_number (int);
CAMLextern int caml_rev_convert_signal_number (int);

#if !(defined(WIFEXITED) && defined(WEXITSTATUS) && defined(WIFSTOPPED) && \
      defined(WSTOPSIG) && defined(WTERMSIG))
/* Assume old-style V7 status word */
#define WIFEXITED(status) (((status) & 0xFF) == 0)
#define WEXITSTATUS(status) (((status) >> 8) & 0xFF)
#define WIFSTOPPED(status) (((status) & 0xFF) == 0xFF)
#define WSTOPSIG(status) (((status) >> 8) & 0xFF)
#define WTERMSIG(status) ((status) & 0x3F)
#endif

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

static value alloc_process_status(int status)
{
  value st;

  if (WIFEXITED(status)) {
    st = alloc_small(1, TAG_WEXITED);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  }
  else if (WIFSTOPPED(status)) {
    st = alloc_small(1, TAG_WSTOPPED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WSTOPSIG(status)));
  }
  else {
    st = alloc_small(1, TAG_WSIGNALED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WTERMSIG(status)));
  }
  return st;
}

static int wait_flag_table[] = {
  WNOHANG, WUNTRACED
};

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

  times = alloc_small(2 * Double_wosize, Double_array_tag);
  Store_double_field(times, 0, ru.ru_utime.tv_sec + ru.ru_utime.tv_usec / 1e6);
  Store_double_field(times, 1, ru.ru_stime.tv_sec + ru.ru_stime.tv_usec / 1e6);

  res = caml_alloc_tuple(3);
  Store_field(res, 0, Val_int(pid));
  Store_field(res, 1, alloc_process_status(status));
  Store_field(res, 2, times);
  CAMLreturn(res);
}

value lwt_unix_has_wait4(value unit)
{
  return Val_int(1);
}

/* +-----------------------------------------------------------------+
   | CPUs                                                            |
   +-----------------------------------------------------------------+ */

#if defined(HAVE_GETCPU)

CAMLprim value lwt_unix_get_cpu()
{
  int cpu = sched_getcpu();
  if (cpu < 0) uerror("sched_getcpu", Nothing);
  return Val_int(cpu);
}

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

#endif

/* +-----------------------------------------------------------------+
   | JOB: guess_blocking                                             |
   +-----------------------------------------------------------------+ */

struct job_guess_blocking {
  struct lwt_unix_job job;
  int fd;
  int result;
};

#define Job_guess_blocking_val(v) *(struct job_guess_blocking**)Data_custom_val(v)

static void worker_guess_blocking(struct job_guess_blocking *job)
{
  struct stat stat;
  if (fstat(job->fd, &stat) == 0)
    job->result = !(S_ISFIFO(stat.st_mode) || S_ISSOCK(stat.st_mode));
  else
    job->result = 1;
}

CAMLprim value lwt_unix_guess_blocking_job(value val_fd)
{
  struct job_guess_blocking *job = lwt_unix_new(struct job_guess_blocking);
  job->job.worker = (lwt_unix_job_worker)worker_guess_blocking;
  job->fd = Int_val(val_fd);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_guess_blocking_result(value val_job)
{
  struct job_guess_blocking *job = Job_guess_blocking_val(val_job);
  return Bool_val(job->result);
}

CAMLprim value lwt_unix_guess_blocking_free(value val_job)
{
  struct job_guess_blocking *job = Job_guess_blocking_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: wait_mincore                                               |
   +-----------------------------------------------------------------+ */

struct job_wait_mincore {
  struct lwt_unix_job job;
  char *ptr;
};

#define Job_wait_mincore_val(v) *(struct job_wait_mincore**)Data_custom_val(v)

static void worker_wait_mincore(struct job_wait_mincore *job)
{
  /* Read the byte to force the kernel to fetch the page: */
  char dummy = *(job->ptr);
  /* Make the compiler happy: */
  dummy = 0;
}

CAMLprim value lwt_unix_wait_mincore_job(value val_buffer, value val_offset)
{
  struct job_wait_mincore *job = lwt_unix_new(struct job_wait_mincore);
  job->job.worker = (lwt_unix_job_worker)worker_wait_mincore;
  job->ptr = (char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_wait_mincore_free(value val_job)
{
  struct job_wait_mincore *job = Job_wait_mincore_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

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
  O_RDONLY,
  O_WRONLY,
  O_RDWR,
  O_NONBLOCK,
  O_APPEND,
  O_CREAT,
  O_TRUNC,
  O_EXCL,
  O_NOCTTY,
  O_DSYNC,
  O_SYNC,
  O_RSYNC
};

struct job_open {
  struct lwt_unix_job job;
  char *path;
  int flags;
  int perms;
  int fd;
  int blocking;
  int error_code;
};

#define Job_open_val(v) *(struct job_open**)Data_custom_val(v)

static void worker_open(struct job_open *job)
{
  int fd;
  fd = open(job->path, job->flags, job->perms);
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

CAMLprim value lwt_unix_open_job(value val_path, value val_flags, value val_perms)
{
  struct job_open *job = lwt_unix_new(struct job_open);
  job->job.worker = (lwt_unix_job_worker)worker_open;
  job->path = lwt_unix_strdup(String_val(val_path));
  job->flags = convert_flag_list(val_flags, open_flag_table);
  job->perms = Int_val(val_perms);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_open_result(value val_job)
{
  struct job_open *job = Job_open_val(val_job);
  int fd = job->fd;
  if (fd < 0) unix_error(job->error_code, "open", Nothing);
  value result = caml_alloc_tuple(2);
  Field(result, 0) = Val_int(fd);
  Field(result, 1) = Val_bool(job->blocking);
  return result;
}

CAMLprim value lwt_unix_open_free(value val_job)
{
  struct job_open *job = Job_open_val(val_job);
  free(job->path);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: close                                                      |
   +-----------------------------------------------------------------+ */

struct job_close {
  struct lwt_unix_job job;
  int fd;
  int result;
  int error_code;
};

#define Job_close_val(v) *(struct job_close**)Data_custom_val(v)

static void worker_close(struct job_close *job)
{
  job->result = close(job->fd);
  job->error_code = errno;
}

CAMLprim value lwt_unix_close_job(value val_fd)
{
  struct job_close *job = lwt_unix_new(struct job_close);
  job->job.worker = (lwt_unix_job_worker)worker_close;
  job->fd = Int_val(val_fd);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_close_result(value val_job)
{
  struct job_close *job = Job_close_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "close", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_close_free(value val_job)
{
  lwt_unix_free_job(&(Job_close_val(val_job))->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: read                                                       |
   +-----------------------------------------------------------------+ */

struct job_read {
  struct lwt_unix_job job;
  int fd;
  char *buffer;
  int length;
  int result;
  int error_code;
};

#define Job_read_val(v) *(struct job_read**)Data_custom_val(v)

static void worker_read(struct job_read *job)
{
  job->result = read(job->fd, job->buffer, job->length);
  job->error_code = errno;
}

CAMLprim value lwt_unix_read_job(value val_fd, value val_length)
{
  struct job_read *job = lwt_unix_new(struct job_read);
  long length = Long_val(val_length);
  job->job.worker = (lwt_unix_job_worker)worker_read;
  job->fd = Int_val(val_fd);
  job->buffer = (char*)lwt_unix_malloc(length);
  job->length = length;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_read_result(value val_job, value val_string, value val_offset)
{
  struct job_read *job = Job_read_val(val_job);
  int result = job->result;
  if (result < 0) unix_error(job->error_code, "read", Nothing);
  memcpy(String_val(val_string) + Long_val(val_offset), job->buffer, result);
  return Val_long(result);
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
  int fd;
  char *buffer;
  int length;
  int result;
  int error_code;
};

#define Job_bytes_read_val(v) *(struct job_bytes_read**)Data_custom_val(v)

static void worker_bytes_read(struct job_bytes_read *job)
{
  job->result = read(job->fd, job->buffer, job->length);
  job->error_code = errno;
}

CAMLprim value lwt_unix_bytes_read_job(value val_fd, value val_buf, value val_ofs, value val_len)
{
  struct job_bytes_read *job = lwt_unix_new(struct job_bytes_read);
  job->job.worker = (lwt_unix_job_worker)worker_bytes_read;
  job->fd = Int_val(val_fd);
  job->buffer = (char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
  job->length = Long_val(val_len);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_bytes_read_result(value val_job)
{
  struct job_bytes_read *job = Job_bytes_read_val(val_job);
  int result = job->result;
  if (result < 0) unix_error(job->error_code, "read", Nothing);
  return Val_long(result);
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
  int fd;
  char *buffer;
  int length;
  int result;
  int error_code;
};

#define Job_write_val(v) *(struct job_write**)Data_custom_val(v)

static void worker_write(struct job_write *job)
{
  job->result = write(job->fd, job->buffer, job->length);
  job->error_code = errno;
}

CAMLprim value lwt_unix_write_job(value val_fd, value val_string, value val_offset, value val_length)
{
  struct job_write *job = lwt_unix_new(struct job_write);
  long length = Long_val(val_length);
  job->job.worker = (lwt_unix_job_worker)worker_write;
  job->fd = Int_val(val_fd);
  job->buffer = (char*)lwt_unix_malloc(length);
  memcpy(job->buffer, String_val(val_string) + Long_val(val_offset), length);
  job->length = length;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_write_result(value val_job)
{
  struct job_write *job = Job_write_val(val_job);
  int result = job->result;
  if (result < 0) unix_error(job->error_code, "write", Nothing);
  return Val_long(result);
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
  int fd;
  char *buffer;
  int length;
  int result;
  int error_code;
};

#define Job_bytes_write_val(v) *(struct job_bytes_write**)Data_custom_val(v)

static void worker_bytes_write(struct job_bytes_write *job)
{
  job->result = write(job->fd, job->buffer, job->length);
  job->error_code = errno;
}

CAMLprim value lwt_unix_bytes_write_job(value val_fd, value val_buffer, value val_offset, value val_length)
{
  struct job_bytes_write *job = lwt_unix_new(struct job_bytes_write);
  job->job.worker = (lwt_unix_job_worker)worker_bytes_write;
  job->fd = Int_val(val_fd);
  job->buffer = (char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
  job->length = Long_val(val_length);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_bytes_write_result(value val_job)
{
  struct job_bytes_write *job = Job_bytes_write_val(val_job);
  int result = job->result;
  if (result < 0) unix_error(job->error_code, "write", Nothing);
  return Val_long(result);
}

CAMLprim value lwt_unix_bytes_write_free(value val_job)
{
  struct job_bytes_write *job = Job_bytes_write_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: lseek                                                      |
   +-----------------------------------------------------------------+ */

struct job_lseek {
  struct lwt_unix_job job;
  int fd;
  off_t offset;
  int command;
  off_t result;
  int error_code;
};

#define Job_lseek_val(v) *(struct job_lseek**)Data_custom_val(v)

static int seek_command_table[] = {
  SEEK_SET, SEEK_CUR, SEEK_END
};

static void worker_lseek(struct job_lseek *job)
{
  job->result = lseek(job->fd, job->offset, job->command);
  job->error_code = errno;
}

CAMLprim value lwt_unix_lseek_job(value val_fd, value val_offset, value val_command)
{
  struct job_lseek *job = lwt_unix_new(struct job_lseek);
  job->job.worker = (lwt_unix_job_worker)worker_lseek;
  job->fd = Int_val(val_fd);
  job->offset = Long_val(val_offset);
  job->command = seek_command_table[Int_val(val_command)];
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_lseek_result(value val_job)
{
  struct job_lseek *job = Job_lseek_val(val_job);
  off_t result = job->result;
  if (result < 0) unix_error(job->error_code, "lseek", Nothing);
  return Val_long(result);
}

CAMLprim value lwt_unix_lseek_free(value val_job)
{
  struct job_lseek *job = Job_lseek_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

CAMLprim value lwt_unix_lseek_64_job(value val_fd, value val_offset, value val_command)
{
  struct job_lseek *job = lwt_unix_new(struct job_lseek);
  job->job.worker = (lwt_unix_job_worker)worker_lseek;
  job->fd = Int_val(val_fd);
  job->offset = Int64_val(val_offset);
  job->command = seek_command_table[Int_val(val_command)];
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_lseek_64_result(value val_job)
{
  struct job_lseek *job = Job_lseek_val(val_job);
  off_t result = job->result;
  if (result < 0) unix_error(job->error_code, "lseek", Nothing);
  return caml_copy_int64(result);
}

CAMLprim value lwt_unix_lseek_64_free(value val_job)
{
  struct job_lseek *job = Job_lseek_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: truncate                                                   |
   +-----------------------------------------------------------------+ */

struct job_truncate {
  struct lwt_unix_job job;
  char *name;
  off_t offset;
  int result;
  int error_code;
};

#define Job_truncate_val(v) *(struct job_truncate**)Data_custom_val(v)

static void worker_truncate(struct job_truncate *job)
{
  job->result = truncate(job->name, job->offset);
  job->error_code = errno;
}

CAMLprim value lwt_unix_truncate_job(value val_name, value val_offset)
{
  struct job_truncate *job = lwt_unix_new(struct job_truncate);
  job->job.worker = (lwt_unix_job_worker)worker_truncate;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->offset = Long_val(val_offset);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_truncate_result(value val_job)
{
  struct job_truncate *job = Job_truncate_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "truncate", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_truncate_free(value val_job)
{
  struct job_truncate *job = Job_truncate_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

CAMLprim value lwt_unix_truncate_64_job(value val_name, value val_offset)
{
  struct job_truncate *job = lwt_unix_new(struct job_truncate);
  job->job.worker = (lwt_unix_job_worker)worker_truncate;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->offset = Int64_val(val_offset);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_truncate_64_result(value val_job)
{
  struct job_truncate *job = Job_truncate_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "truncate", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_truncate_64_free(value val_job)
{
  struct job_truncate *job = Job_truncate_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: ftruncate                                                  |
   +-----------------------------------------------------------------+ */

struct job_ftruncate {
  struct lwt_unix_job job;
  int fd;
  off_t offset;
  int result;
  int error_code;
};

#define Job_ftruncate_val(v) *(struct job_ftruncate**)Data_custom_val(v)

static void worker_ftruncate(struct job_ftruncate *job)
{
  job->result = ftruncate(job->fd, job->offset);
  job->error_code = errno;
}

CAMLprim value lwt_unix_ftruncate_job(value val_fd, value val_offset)
{
  struct job_ftruncate *job = lwt_unix_new(struct job_ftruncate);
  job->job.worker = (lwt_unix_job_worker)worker_ftruncate;
  job->fd = Int_val(val_fd);
  job->offset = Long_val(val_offset);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_ftruncate_result(value val_job)
{
  struct job_ftruncate *job = Job_ftruncate_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "ftruncate", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_ftruncate_free(value val_job)
{
  struct job_ftruncate *job = Job_ftruncate_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

CAMLprim value lwt_unix_ftruncate_64_job(value val_fd, value val_offset)
{
  struct job_ftruncate *job = lwt_unix_new(struct job_ftruncate);
  job->job.worker = (lwt_unix_job_worker)worker_ftruncate;
  job->fd = Int_val(val_fd);
  job->offset = Int64_val(val_offset);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_ftruncate_64_result(value val_job)
{
  struct job_ftruncate *job = Job_ftruncate_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "ftruncate", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_ftruncate_64_free(value val_job)
{
  struct job_ftruncate *job = Job_ftruncate_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: fsync                                                      |
   +-----------------------------------------------------------------+ */

struct job_fsync {
  struct lwt_unix_job job;
  int fd;
  int result;
  int error_code;
};

#define Job_fsync_val(v) *(struct job_fsync**)Data_custom_val(v)

static void worker_fsync(struct job_fsync *job)
{
  job->result = fsync(job->fd);
  job->error_code = errno;
}

CAMLprim value lwt_unix_fsync_job(value val_fd)
{
  struct job_fsync *job = lwt_unix_new(struct job_fsync);
  job->job.worker = (lwt_unix_job_worker)worker_fsync;
  job->fd = Int_val(val_fd);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_fsync_result(value val_job)
{
  struct job_fsync *job = Job_fsync_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "fsync", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_fsync_free(value val_job)
{
  struct job_fsync *job = Job_fsync_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

#if defined(HAVE_FDATASYNC)

/* +-----------------------------------------------------------------+
   | JOB: fdatasync                                                  |
   +-----------------------------------------------------------------+ */

struct job_fdatasync {
  struct lwt_unix_job job;
  int fd;
  int result;
  int error_code;
};

#define Job_fdatasync_val(v) *(struct job_fdatasync**)Data_custom_val(v)

static void worker_fdatasync(struct job_fdatasync *job)
{
  job->result = fdatasync(job->fd);
  job->error_code = errno;
}

CAMLprim value lwt_unix_fdatasync_job(value val_fd)
{
  struct job_fdatasync *job = lwt_unix_new(struct job_fdatasync);
  job->job.worker = (lwt_unix_job_worker)worker_fdatasync;
  job->fd = Int_val(val_fd);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_fdatasync_result(value val_job)
{
  struct job_fdatasync *job = Job_fdatasync_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "fdatasync", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_fdatasync_free(value val_job)
{
  struct job_fdatasync *job = Job_fdatasync_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

#endif

/* +-----------------------------------------------------------------+
   | JOB: stat                                                       |
   +-----------------------------------------------------------------+ */

struct job_stat {
  struct lwt_unix_job job;
  char *name;
  struct stat stat;
  int result;
  int error_code;
};

#define Job_stat_val(v) *(struct job_stat**)Data_custom_val(v)

static value copy_stat(int use_64, struct stat *buf)
{
  CAMLparam0();
  CAMLlocal5(atime, mtime, ctime, offset, v);

  atime = copy_double((double) buf->st_atime);
  mtime = copy_double((double) buf->st_mtime);
  ctime = copy_double((double) buf->st_ctime);
  offset = use_64 ? caml_copy_int64(buf->st_size) : Val_int(buf->st_size);
  v = alloc_small(12, 0);
  Field(v, 0) = Val_int (buf->st_dev);
  Field(v, 1) = Val_int (buf->st_ino);
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
  job->result = stat(job->name, &(job->stat));
  job->error_code = errno;
}

CAMLprim value lwt_unix_stat_job(value val_name)
{
  struct job_stat *job = lwt_unix_new(struct job_stat);
  job->job.worker = (lwt_unix_job_worker)worker_stat;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_stat_result(value val_job)
{
  struct job_stat *job = Job_stat_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "stat", Nothing);
  return copy_stat(0, &(job->stat));
}

CAMLprim value lwt_unix_stat_free(value val_job)
{
  struct job_stat *job = Job_stat_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

CAMLprim value lwt_unix_stat_64_job(value val_name)
{
  struct job_stat *job = lwt_unix_new(struct job_stat);
  job->job.worker = (lwt_unix_job_worker)worker_stat;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_stat_64_result(value val_job)
{
  struct job_stat *job = Job_stat_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "stat", Nothing);
  return copy_stat(1, &(job->stat));
}

CAMLprim value lwt_unix_stat_64_free(value val_job)
{
  struct job_stat *job = Job_stat_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: lstat                                                      |
   +-----------------------------------------------------------------+ */

struct job_lstat {
  struct lwt_unix_job job;
  char *name;
  struct stat lstat;
  int result;
  int error_code;
};

#define Job_lstat_val(v) *(struct job_lstat**)Data_custom_val(v)

static void worker_lstat(struct job_lstat *job)
{
  job->result = lstat(job->name, &(job->lstat));
  job->error_code = errno;
}

CAMLprim value lwt_unix_lstat_job(value val_name)
{
  struct job_lstat *job = lwt_unix_new(struct job_lstat);
  job->job.worker = (lwt_unix_job_worker)worker_lstat;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_lstat_result(value val_job)
{
  struct job_lstat *job = Job_lstat_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "lstat", Nothing);
  return copy_stat(0, &(job->lstat));
}

CAMLprim value lwt_unix_lstat_free(value val_job)
{
  struct job_lstat *job = Job_lstat_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

CAMLprim value lwt_unix_lstat_64_job(value val_name)
{
  struct job_lstat *job = lwt_unix_new(struct job_lstat);
  job->job.worker = (lwt_unix_job_worker)worker_lstat;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_lstat_64_result(value val_job)
{
  struct job_lstat *job = Job_lstat_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "lstat", Nothing);
  return copy_stat(1, &(job->lstat));
}

CAMLprim value lwt_unix_lstat_64_free(value val_job)
{
  struct job_lstat *job = Job_lstat_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
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

#define Job_fstat_val(v) *(struct job_fstat**)Data_custom_val(v)

static void worker_fstat(struct job_fstat *job)
{
  job->result = fstat(job->fd, &(job->fstat));
  job->error_code = errno;
}

CAMLprim value lwt_unix_fstat_job(value val_fd)
{
  struct job_fstat *job = lwt_unix_new(struct job_fstat);
  job->job.worker = (lwt_unix_job_worker)worker_fstat;
  job->fd = Int_val(val_fd);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_fstat_result(value val_job)
{
  struct job_fstat *job = Job_fstat_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "fstat", Nothing);
  return copy_stat(0, &(job->fstat));
}

CAMLprim value lwt_unix_fstat_free(value val_job)
{
  struct job_fstat *job = Job_fstat_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

CAMLprim value lwt_unix_fstat_64_job(value val_fd)
{
  struct job_fstat *job = lwt_unix_new(struct job_fstat);
  job->job.worker = (lwt_unix_job_worker)worker_fstat;
  job->fd = Int_val(val_fd);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_fstat_64_result(value val_job)
{
  struct job_fstat *job = Job_fstat_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "fstat", Nothing);
  return copy_stat(1, &(job->fstat));
}

CAMLprim value lwt_unix_fstat_64_free(value val_job)
{
  struct job_fstat *job = Job_fstat_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: isatty                                                     |
   +-----------------------------------------------------------------+ */

struct job_isatty {
  struct lwt_unix_job job;
  int fd;
  int result;
};

#define Job_isatty_val(v) *(struct job_isatty**)Data_custom_val(v)

static void worker_isatty(struct job_isatty *job)
{
  job->result = isatty(job->fd);
}

CAMLprim value lwt_unix_isatty_job(value val_fd)
{
  struct job_isatty *job = lwt_unix_new(struct job_isatty);
  job->job.worker = (lwt_unix_job_worker)worker_isatty;
  job->fd = Int_val(val_fd);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_isatty_result(value val_job)
{
  struct job_isatty *job = Job_isatty_val(val_job);
  return Val_bool(job->result);
}

CAMLprim value lwt_unix_isatty_free(value val_job)
{
  struct job_isatty *job = Job_isatty_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: unlink                                                     |
   +-----------------------------------------------------------------+ */

struct job_unlink {
  struct lwt_unix_job job;
  char *name;
  int result;
  int error_code;
};

#define Job_unlink_val(v) *(struct job_unlink**)Data_custom_val(v)

static void worker_unlink(struct job_unlink *job)
{
  job->result = unlink(job->name);
  job->error_code = errno;
}

CAMLprim value lwt_unix_unlink_job(value val_name)
{
  struct job_unlink *job = lwt_unix_new(struct job_unlink);
  job->job.worker = (lwt_unix_job_worker)worker_unlink;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_unlink_result(value val_job)
{
  struct job_unlink *job = Job_unlink_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "unlink", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_unlink_free(value val_job)
{
  struct job_unlink *job = Job_unlink_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: rename                                                     |
   +-----------------------------------------------------------------+ */

struct job_rename {
  struct lwt_unix_job job;
  char *name1;
  char *name2;
  int result;
  int error_code;
};

#define Job_rename_val(v) *(struct job_rename**)Data_custom_val(v)

static void worker_rename(struct job_rename *job)
{
  job->result = rename(job->name1, job->name2);
  job->error_code = errno;
}

CAMLprim value lwt_unix_rename_job(value val_name1, value val_name2)
{
  struct job_rename *job = lwt_unix_new(struct job_rename);
  job->job.worker = (lwt_unix_job_worker)worker_rename;
  job->name1 = lwt_unix_strdup(String_val(val_name1));
  job->name2 = lwt_unix_strdup(String_val(val_name2));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_rename_result(value val_job)
{
  struct job_rename *job = Job_rename_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "rename", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_rename_free(value val_job)
{
  struct job_rename *job = Job_rename_val(val_job);
  free(job->name1);
  free(job->name2);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: link                                                       |
   +-----------------------------------------------------------------+ */

struct job_link {
  struct lwt_unix_job job;
  char *name1;
  char *name2;
  int result;
  int error_code;
};

#define Job_link_val(v) *(struct job_link**)Data_custom_val(v)

static void worker_link(struct job_link *job)
{
  job->result = link(job->name1, job->name2);
  job->error_code = errno;
}

CAMLprim value lwt_unix_link_job(value val_name1, value val_name2)
{
  struct job_link *job = lwt_unix_new(struct job_link);
  job->job.worker = (lwt_unix_job_worker)worker_link;
  job->name1 = lwt_unix_strdup(String_val(val_name1));
  job->name2 = lwt_unix_strdup(String_val(val_name2));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_link_result(value val_job)
{
  struct job_link *job = Job_link_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "link", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_link_free(value val_job)
{
  struct job_link *job = Job_link_val(val_job);
  free(job->name1);
  free(job->name2);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: chmod                                                      |
   +-----------------------------------------------------------------+ */

struct job_chmod {
  struct lwt_unix_job job;
  char *name;
  int perms;
  int result;
  int error_code;
};

#define Job_chmod_val(v) *(struct job_chmod**)Data_custom_val(v)

static void worker_chmod(struct job_chmod *job)
{
  job->result = chmod(job->name, job->perms);
  job->error_code = errno;
}

CAMLprim value lwt_unix_chmod_job(value val_name, value val_perms)
{
  struct job_chmod *job = lwt_unix_new(struct job_chmod);
  job->job.worker = (lwt_unix_job_worker)worker_chmod;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->perms = Int_val(val_perms);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_chmod_result(value val_job)
{
  struct job_chmod *job = Job_chmod_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "chmod", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_chmod_free(value val_job)
{
  struct job_chmod *job = Job_chmod_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: fchmod                                                     |
   +-----------------------------------------------------------------+ */

struct job_fchmod {
  struct lwt_unix_job job;
  int fd;
  int perms;
  int result;
  int error_code;
};

#define Job_fchmod_val(v) *(struct job_fchmod**)Data_custom_val(v)

static void worker_fchmod(struct job_fchmod *job)
{
  job->result = fchmod(job->fd, job->perms);
  job->error_code = errno;
}

CAMLprim value lwt_unix_fchmod_job(value val_fd, value val_perms)
{
  struct job_fchmod *job = lwt_unix_new(struct job_fchmod);
  job->job.worker = (lwt_unix_job_worker)worker_fchmod;
  job->fd = Int_val(val_fd);
  job->perms = Int_val(val_perms);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_fchmod_result(value val_job)
{
  struct job_fchmod *job = Job_fchmod_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "fchmod", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_fchmod_free(value val_job)
{
  struct job_fchmod *job = Job_fchmod_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: chown                                                      |
   +-----------------------------------------------------------------+ */

struct job_chown {
  struct lwt_unix_job job;
  char *name;
  int uid;
  int gid;
  int result;
  int error_code;
};

#define Job_chown_val(v) *(struct job_chown**)Data_custom_val(v)

static void worker_chown(struct job_chown *job)
{
  job->result = chown(job->name, job->uid, job->gid);
  job->error_code = errno;
}

CAMLprim value lwt_unix_chown_job(value val_name, value val_uid, value val_gid)
{
  struct job_chown *job = lwt_unix_new(struct job_chown);
  job->job.worker = (lwt_unix_job_worker)worker_chown;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->uid = Int_val(val_uid);
  job->gid = Int_val(val_gid);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_chown_result(value val_job)
{
  struct job_chown *job = Job_chown_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "chown", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_chown_free(value val_job)
{
  struct job_chown *job = Job_chown_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: fchown                                                     |
   +-----------------------------------------------------------------+ */

struct job_fchown {
  struct lwt_unix_job job;
  int fd;
  int uid;
  int gid;
  int result;
  int error_code;
};

#define Job_fchown_val(v) *(struct job_fchown**)Data_custom_val(v)

static void worker_fchown(struct job_fchown *job)
{
  job->result = fchown(job->fd, job->uid, job->gid);
  job->error_code = errno;
}

CAMLprim value lwt_unix_fchown_job(value val_fd, value val_uid, value val_gid)
{
  struct job_fchown *job = lwt_unix_new(struct job_fchown);
  job->job.worker = (lwt_unix_job_worker)worker_fchown;
  job->fd = Int_val(val_fd);
  job->uid = Int_val(val_uid);
  job->gid = Int_val(val_gid);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_fchown_result(value val_job)
{
  struct job_fchown *job = Job_fchown_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "fchown", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_fchown_free(value val_job)
{
  struct job_fchown *job = Job_fchown_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: access                                                     |
   +-----------------------------------------------------------------+ */

struct job_access {
  struct lwt_unix_job job;
  char *name;
  int mode;
  int result;
  int error_code;
};

#define Job_access_val(v) *(struct job_access**)Data_custom_val(v)

static int access_permission_table[] = {
  R_OK, W_OK, X_OK, F_OK
};

static void worker_access(struct job_access *job)
{
  job->result = access(job->name, job->mode);
  job->error_code = errno;
}

CAMLprim value lwt_unix_access_job(value val_name, value val_perms)
{
  struct job_access *job = lwt_unix_new(struct job_access);
  job->job.worker = (lwt_unix_job_worker)worker_access;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->mode = convert_flag_list(val_perms, access_permission_table);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_access_result(value val_job)
{
  struct job_access *job = Job_access_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "access", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_access_free(value val_job)
{
  struct job_access *job = Job_access_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: mkdir                                                      |
   +-----------------------------------------------------------------+ */

struct job_mkdir {
  struct lwt_unix_job job;
  char *name;
  int perms;
  int result;
  int error_code;
};

#define Job_mkdir_val(v) *(struct job_mkdir**)Data_custom_val(v)

static void worker_mkdir(struct job_mkdir *job)
{
  job->result = mkdir(job->name, job->perms);
  job->error_code = errno;
}

CAMLprim value lwt_unix_mkdir_job(value val_name, value val_perms)
{
  struct job_mkdir *job = lwt_unix_new(struct job_mkdir);
  job->job.worker = (lwt_unix_job_worker)worker_mkdir;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->perms = Int_val(val_perms);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_mkdir_result(value val_job)
{
  struct job_mkdir *job = Job_mkdir_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "mkdir", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_mkdir_free(value val_job)
{
  struct job_mkdir *job = Job_mkdir_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: rmdir                                                      |
   +-----------------------------------------------------------------+ */

struct job_rmdir {
  struct lwt_unix_job job;
  char *name;
  int result;
  int error_code;
};

#define Job_rmdir_val(v) *(struct job_rmdir**)Data_custom_val(v)

static void worker_rmdir(struct job_rmdir *job)
{
  job->result = rmdir(job->name);
  job->error_code = errno;
}

CAMLprim value lwt_unix_rmdir_job(value val_name)
{
  struct job_rmdir *job = lwt_unix_new(struct job_rmdir);
  job->job.worker = (lwt_unix_job_worker)worker_rmdir;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_rmdir_result(value val_job)
{
  struct job_rmdir *job = Job_rmdir_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "rmdir", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_rmdir_free(value val_job)
{
  struct job_rmdir *job = Job_rmdir_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: chdir                                                      |
   +-----------------------------------------------------------------+ */

struct job_chdir {
  struct lwt_unix_job job;
  char *name;
  int result;
  int error_code;
};

#define Job_chdir_val(v) *(struct job_chdir**)Data_custom_val(v)

static void worker_chdir(struct job_chdir *job)
{
  job->result = chdir(job->name);
  job->error_code = errno;
}

CAMLprim value lwt_unix_chdir_job(value val_name)
{
  struct job_chdir *job = lwt_unix_new(struct job_chdir);
  job->job.worker = (lwt_unix_job_worker)worker_chdir;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_chdir_result(value val_job)
{
  struct job_chdir *job = Job_chdir_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "chdir", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_chdir_free(value val_job)
{
  struct job_chdir *job = Job_chdir_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: chroot                                                     |
   +-----------------------------------------------------------------+ */

struct job_chroot {
  struct lwt_unix_job job;
  char *name;
  int result;
  int error_code;
};

#define Job_chroot_val(v) *(struct job_chroot**)Data_custom_val(v)

static void worker_chroot(struct job_chroot *job)
{
  job->result = chroot(job->name);
  job->error_code = errno;
}

CAMLprim value lwt_unix_chroot_job(value val_name)
{
  struct job_chroot *job = lwt_unix_new(struct job_chroot);
  job->job.worker = (lwt_unix_job_worker)worker_chroot;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_chroot_result(value val_job)
{
  struct job_chroot *job = Job_chroot_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "chroot", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_chroot_free(value val_job)
{
  struct job_chroot *job = Job_chroot_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: opendir                                                    |
   +-----------------------------------------------------------------+ */

struct job_opendir {
  struct lwt_unix_job job;
  char *name;
  DIR *result;
  int error_code;
};

#define Job_opendir_val(v) *(struct job_opendir**)Data_custom_val(v)

static void worker_opendir(struct job_opendir *job)
{
  job->result = opendir(job->name);
  job->error_code = errno;
}

CAMLprim value lwt_unix_opendir_job(value val_name)
{
  struct job_opendir *job = lwt_unix_new(struct job_opendir);
  job->job.worker = (lwt_unix_job_worker)worker_opendir;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_opendir_result(value val_job)
{
  struct job_opendir *job = Job_opendir_val(val_job);
  if (job->result == NULL) unix_error(job->error_code, "opendir", Nothing);
  value result = alloc_small(1, Abstract_tag);
  DIR_Val(result) = job->result;
  return result;
}

CAMLprim value lwt_unix_opendir_free(value val_job)
{
  struct job_opendir *job = Job_opendir_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: readdir                                                    |
   +-----------------------------------------------------------------+ */

struct job_readdir {
  struct lwt_unix_job job;
  DIR *dir;
  struct dirent *entry;
  struct dirent *ptr;
  int result;
};

#define Job_readdir_val(v) *(struct job_readdir**)Data_custom_val(v)

static void worker_readdir(struct job_readdir *job)
{
  job->entry = lwt_unix_malloc(offsetof(struct dirent, d_name) + fpathconf(dirfd(job->dir), _PC_NAME_MAX) + 1);
  job->result = readdir_r(job->dir, job->entry, &(job->ptr));
}

CAMLprim value lwt_unix_readdir_job(value val_dir)
{
  struct job_readdir *job = lwt_unix_new(struct job_readdir);
  job->job.worker = (lwt_unix_job_worker)worker_readdir;
  job->dir = DIR_Val(val_dir);
  job->entry = NULL;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_readdir_result(value val_job)
{
  struct job_readdir *job = Job_readdir_val(val_job);
  if (job->result != 0) unix_error(job->result, "readdir", Nothing);
  if (job->ptr == NULL) caml_raise_end_of_file();
  return caml_copy_string(job->entry->d_name);
}

CAMLprim value lwt_unix_readdir_free(value val_job)
{
  struct job_readdir *job = Job_readdir_val(val_job);
  if (job->entry != NULL) free(job->entry);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: readdir_n                                                  |
   +-----------------------------------------------------------------+ */

struct job_readdir_n {
  struct lwt_unix_job job;
  DIR *dir;
  int count;
  int error_code;
  struct dirent *entries[];
};

#define Job_readdir_n_val(v) *(struct job_readdir_n**)Data_custom_val(v)

static void worker_readdir_n(struct job_readdir_n *job)
{
  size_t size = offsetof(struct dirent, d_name) + fpathconf(dirfd(job->dir), _PC_NAME_MAX) + 1;
  int i;
  for(i = 0; i < job->count; i++) {
    struct dirent *ptr;
    struct dirent *entry = (struct dirent *)lwt_unix_malloc(size);

    int result = readdir_r(job->dir, entry, &ptr);

    /* An error happened. */
    if (result != 0) {
      /* Free already read entries. */
      free(entry);
      int j;
      for(j = 0; j < i; j++) free(job->entries[j]);
      /* Return an error. */
      job->error_code = result;
      return;
    }

    /* End of directory reached */
    if (ptr == NULL) {
      free(entry);
      break;
    }

    job->entries[i] = entry;
  }

  job->count = i;
  job->error_code = 0;
}

CAMLprim value lwt_unix_readdir_n_job(value val_dir, value val_count)
{
  int count = Int_val(val_count);
  struct job_readdir_n *job = (struct job_readdir_n *)lwt_unix_malloc(sizeof(struct job_readdir_n) + sizeof(struct dirent*) * count);
  job->job.worker = (lwt_unix_job_worker)worker_readdir_n;
  job->dir = DIR_Val(val_dir);
  job->count = count;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_readdir_n_result(value val_job)
{
  CAMLparam1(val_job);
  CAMLlocal1(result);
  struct job_readdir_n *job = Job_readdir_n_val(val_job);
  if (job->error_code != 0) unix_error(job->error_code, "readdir_n", Nothing);

  result = caml_alloc(job->count, 0);
  int i;
  for(i = 0; i < job->count; i++) {
    Store_field(result, i, caml_copy_string(job->entries[i]->d_name));
    free(job->entries[i]);
    job->entries[i] = NULL;
  }
  job->count = 0;
  CAMLreturn(result);
}

CAMLprim value lwt_unix_readdir_n_free(value val_job)
{
  struct job_readdir_n *job = Job_readdir_n_val(val_job);
  if (job->error_code == 0) {
    int i;
    for(i = 0; i < job->count; i++)
      if (job->entries[i] != NULL) free(job->entries[i]);
  }
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: rewinddir                                                  |
   +-----------------------------------------------------------------+ */

struct job_rewinddir {
  struct lwt_unix_job job;
  DIR *dir;
};

#define Job_rewinddir_val(v) *(struct job_rewinddir**)Data_custom_val(v)

static void worker_rewinddir(struct job_rewinddir *job)
{
  rewinddir(job->dir);
}

CAMLprim value lwt_unix_rewinddir_job(value val_dir)
{
  struct job_rewinddir *job = lwt_unix_new(struct job_rewinddir);
  job->job.worker = (lwt_unix_job_worker)worker_rewinddir;
  job->dir = DIR_Val(val_dir);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_rewinddir_result(value val_job)
{
  return Val_unit;
}

CAMLprim value lwt_unix_rewinddir_free(value val_job)
{
  struct job_rewinddir *job = Job_rewinddir_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: closedir                                                   |
   +-----------------------------------------------------------------+ */

struct job_closedir {
  struct lwt_unix_job job;
  DIR *dir;
  int result;
  int error_code;
};

#define Job_closedir_val(v) *(struct job_closedir**)Data_custom_val(v)

static void worker_closedir(struct job_closedir *job)
{
  job->result = closedir(job->dir);
  job->error_code = errno;
}

CAMLprim value lwt_unix_closedir_job(value val_dir)
{
  struct job_closedir *job = lwt_unix_new(struct job_closedir);
  job->job.worker = (lwt_unix_job_worker)worker_closedir;
  job->dir = DIR_Val(val_dir);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_closedir_result(value val_job)
{
  struct job_closedir *job = Job_closedir_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "closedir", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_closedir_free(value val_job)
{
  struct job_closedir *job = Job_closedir_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: mkfifo                                                     |
   +-----------------------------------------------------------------+ */

struct job_mkfifo {
  struct lwt_unix_job job;
  char *name;
  int perms;
  int result;
  int error_code;
};

#define Job_mkfifo_val(v) *(struct job_mkfifo**)Data_custom_val(v)

static void worker_mkfifo(struct job_mkfifo *job)
{
  job->result = mkfifo(job->name, job->perms);
  job->error_code = errno;
}

CAMLprim value lwt_unix_mkfifo_job(value val_name, value val_perms)
{
  struct job_mkfifo *job = lwt_unix_new(struct job_mkfifo);
  job->job.worker = (lwt_unix_job_worker)worker_mkfifo;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->perms = Int_val(val_perms);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_mkfifo_result(value val_job)
{
  struct job_mkfifo *job = Job_mkfifo_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "mkfifo", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_mkfifo_free(value val_job)
{
  struct job_mkfifo *job = Job_mkfifo_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: symlink                                                    |
   +-----------------------------------------------------------------+ */

struct job_symlink {
  struct lwt_unix_job job;
  char *name1;
  char *name2;
  int result;
  int error_code;
};

#define Job_symlink_val(v) *(struct job_symlink**)Data_custom_val(v)

static void worker_symlink(struct job_symlink *job)
{
  job->result = symlink(job->name1, job->name2);
  job->error_code = errno;
}

CAMLprim value lwt_unix_symlink_job(value val_name1, value val_name2)
{
  struct job_symlink *job = lwt_unix_new(struct job_symlink);
  job->job.worker = (lwt_unix_job_worker)worker_symlink;
  job->name1 = lwt_unix_strdup(String_val(val_name1));
  job->name2 = lwt_unix_strdup(String_val(val_name2));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_symlink_result(value val_job)
{
  struct job_symlink *job = Job_symlink_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "symlink", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_symlink_free(value val_job)
{
  struct job_symlink *job = Job_symlink_val(val_job);
  free(job->name1);
  free(job->name2);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: readlink                                                   |
   +-----------------------------------------------------------------+ */

struct job_readlink {
  struct lwt_unix_job job;
  char *name;
  char *buffer;
  ssize_t result;
  int error_code;
};

#define Job_readlink_val(v) *(struct job_readlink**)Data_custom_val(v)

static void worker_readlink(struct job_readlink *job)
{

  ssize_t buffer_size = 1024;
  ssize_t link_length;

  for (;;) {

    job->buffer = lwt_unix_malloc(buffer_size);

    link_length = readlink(job->name, job->buffer, buffer_size);

    if (link_length < buffer_size) {
      if (link_length >= 0) {
        job->buffer = realloc(job->buffer, link_length + 1);
        job->buffer[link_length] = '\0';
      } else {
        free (job->buffer);
        job->buffer = NULL;
      }
      job->result = link_length;
      job->error_code = errno;
      break;
    } else {
      free(job->buffer);
      buffer_size *= 2;
    }
  }
}

CAMLprim value lwt_unix_readlink_job(value val_name)
{
  struct job_readlink *job = lwt_unix_new(struct job_readlink);
  job->job.worker = (lwt_unix_job_worker)worker_readlink;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->buffer = NULL;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_readlink_result(value val_job)
{
  struct job_readlink *job = Job_readlink_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "readlink", Nothing);
  return caml_copy_string(job->buffer);
}

CAMLprim value lwt_unix_readlink_free(value val_job)
{
  struct job_readlink *job = Job_readlink_val(val_job);
  free(job->name);
  free(job->buffer);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: lockf                                                      |
   +-----------------------------------------------------------------+ */

struct job_lockf {
  struct lwt_unix_job job;
  int fd;
  int command;
  off_t length;
  int result;
  int error_code;
};

#define Job_lockf_val(v) *(struct job_lockf**)Data_custom_val(v)

static int lock_command_table[] = {
  F_ULOCK, F_LOCK, F_TLOCK, F_TEST, F_LOCK, F_TLOCK
};

static void worker_lockf(struct job_lockf *job)
{
  job->result = lockf(job->fd, job->command, job->length);
  job->error_code = errno;
}

CAMLprim value lwt_unix_lockf_job(value val_fd, value val_command, value val_length)
{
  struct job_lockf *job = lwt_unix_new(struct job_lockf);
  job->job.worker = (lwt_unix_job_worker)worker_lockf;
  job->fd = Int_val(val_fd);
  job->command = lock_command_table[Int_val(val_command)];
  job->length = Long_val(val_length);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_lockf_result(value val_job)
{
  struct job_lockf *job = Job_lockf_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "lockf", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_lockf_free(value val_job)
{
  struct job_lockf *job = Job_lockf_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: getlogin                                                   |
   +-----------------------------------------------------------------+ */

struct job_getlogin {
  struct lwt_unix_job job;
  char buffer[1024];
  int result;
};

#define Job_getlogin_val(v) *(struct job_getlogin**)Data_custom_val(v)

static void worker_getlogin(struct job_getlogin *job)
{
  job->result = getlogin_r(job->buffer, 1024);
}

CAMLprim value lwt_unix_getlogin_job()
{
  struct job_getlogin *job = lwt_unix_new(struct job_getlogin);
  job->job.worker = (lwt_unix_job_worker)worker_getlogin;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getlogin_result(value val_job)
{
  struct job_getlogin *job = Job_getlogin_val(val_job);
  if (job->result != 0) unix_error(job->result, "getlogin", Nothing);
  return caml_copy_string(job->buffer);
}

CAMLprim value lwt_unix_getlogin_free(value val_job)
{
  struct job_getlogin *job = Job_getlogin_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: getpwnam                                                   |
   +-----------------------------------------------------------------+ */

struct job_getpwnam {
  struct lwt_unix_job job;
  char *name;
  struct passwd pwd;
  struct passwd *ptr;
  char *buffer;
  int result;
};

#define Job_getpwnam_val(v) *(struct job_getpwnam**)Data_custom_val(v)

static value alloc_passwd_entry(struct passwd *entry)
{
  value res;
  value name = Val_unit, passwd = Val_unit, gecos = Val_unit;
  value dir = Val_unit, shell = Val_unit;

  Begin_roots5 (name, passwd, gecos, dir, shell);
    name = copy_string(entry->pw_name);
    passwd = copy_string(entry->pw_passwd);
#ifndef __BEOS__
    gecos = copy_string(entry->pw_gecos);
#else
    gecos = copy_string("");
#endif
    dir = copy_string(entry->pw_dir);
    shell = copy_string(entry->pw_shell);
    res = alloc_small(7, 0);
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

static void worker_getpwnam(struct job_getpwnam *job)
{
  size_t buffer_size = sysconf(_SC_GETPW_R_SIZE_MAX);
  if (buffer_size == (size_t) -1) buffer_size = 16384;
  job->buffer = (char*)lwt_unix_malloc(buffer_size);
  job->result = getpwnam_r(job->name, &(job->pwd), job->buffer, buffer_size, &(job->ptr));
}

CAMLprim value lwt_unix_getpwnam_job(value val_name)
{
  struct job_getpwnam *job = lwt_unix_new(struct job_getpwnam);
  job->job.worker = (lwt_unix_job_worker)worker_getpwnam;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->buffer = NULL;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getpwnam_result(value val_job)
{
  struct job_getpwnam *job = Job_getpwnam_val(val_job);
  if (job->result != 0) unix_error(job->result, "getpwnam", Nothing);
  if (job->ptr == NULL) caml_raise_not_found();
  return alloc_passwd_entry(&(job->pwd));
}

CAMLprim value lwt_unix_getpwnam_free(value val_job)
{
  struct job_getpwnam *job = Job_getpwnam_val(val_job);
  free(job->name);
  if (job->buffer != NULL) free(job->buffer);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: getgrnam                                                   |
   +-----------------------------------------------------------------+ */

struct job_getgrnam {
  struct lwt_unix_job job;
  char *name;
  struct group grp;
  struct group *ptr;
  char *buffer;
  int result;
};

#define Job_getgrnam_val(v) *(struct job_getgrnam**)Data_custom_val(v)

static value alloc_group_entry(struct group *entry)
{
  value res;
  value name = Val_unit, pass = Val_unit, mem = Val_unit;

  Begin_roots3 (name, pass, mem);
    name = copy_string(entry->gr_name);
    pass = copy_string(entry->gr_passwd);
    mem = copy_string_array((const char**)entry->gr_mem);
    res = alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = pass;
    Field(res, 2) = Val_int(entry->gr_gid);
    Field(res, 3) = mem;
  End_roots();
  return res;
}

static void worker_getgrnam(struct job_getgrnam *job)
{
  size_t buffer_size = sysconf(_SC_GETGR_R_SIZE_MAX);
  if (buffer_size == (size_t) -1) buffer_size = 16384;
  job->buffer = (char*)lwt_unix_malloc(buffer_size);
  job->result = getgrnam_r(job->name, &(job->grp), job->buffer, buffer_size, &(job->ptr));
}

CAMLprim value lwt_unix_getgrnam_job(value val_name)
{
  struct job_getgrnam *job = lwt_unix_new(struct job_getgrnam);
  job->job.worker = (lwt_unix_job_worker)worker_getgrnam;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->buffer = NULL;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getgrnam_result(value val_job)
{
  struct job_getgrnam *job = Job_getgrnam_val(val_job);
  if (job->result != 0) unix_error(job->result, "getgrnam", Nothing);
  if (job->ptr == NULL) caml_raise_not_found();
  return alloc_group_entry(&(job->grp));
}

CAMLprim value lwt_unix_getgrnam_free(value val_job)
{
  struct job_getgrnam *job = Job_getgrnam_val(val_job);
  free(job->name);
  if (job->buffer != NULL) free(job->buffer);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: getpwuid                                                   |
   +-----------------------------------------------------------------+ */

struct job_getpwuid {
  struct lwt_unix_job job;
  int uid;
  struct passwd pwd;
  struct passwd *ptr;
  char *buffer;
  int result;
};

#define Job_getpwuid_val(v) *(struct job_getpwuid**)Data_custom_val(v)

static void worker_getpwuid(struct job_getpwuid *job)
{
  size_t buffer_size = sysconf(_SC_GETPW_R_SIZE_MAX);
  if (buffer_size == (size_t) -1) buffer_size = 16384;
  job->buffer = (char*)lwt_unix_malloc(buffer_size);
  job->result = getpwuid_r(job->uid, &(job->pwd), job->buffer, buffer_size, &(job->ptr));
}

CAMLprim value lwt_unix_getpwuid_job(value val_uid)
{
  struct job_getpwuid *job = lwt_unix_new(struct job_getpwuid);
  job->job.worker = (lwt_unix_job_worker)worker_getpwuid;
  job->uid = Int_val(val_uid);
  job->buffer = NULL;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getpwuid_result(value val_job)
{
  struct job_getpwuid *job = Job_getpwuid_val(val_job);
  if (job->result != 0) unix_error(job->result, "getpwuid", Nothing);
  if (job->ptr == NULL) caml_raise_not_found();
  return alloc_passwd_entry(&(job->pwd));
}

CAMLprim value lwt_unix_getpwuid_free(value val_job)
{
  struct job_getpwuid *job = Job_getpwuid_val(val_job);
  if (job->buffer != NULL) free(job->buffer);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: getgrgid                                                   |
   +-----------------------------------------------------------------+ */

struct job_getgrgid {
  struct lwt_unix_job job;
  int gid;
  struct group grp;
  struct group *ptr;
  char *buffer;
  int result;
};

#define Job_getgrgid_val(v) *(struct job_getgrgid**)Data_custom_val(v)

static void worker_getgrgid(struct job_getgrgid *job)
{
  size_t buffer_size = sysconf(_SC_GETGR_R_SIZE_MAX);
  if (buffer_size == (size_t) -1) buffer_size = 16384;
  job->buffer = (char*)lwt_unix_malloc(buffer_size);
  job->result = getgrgid_r(job->gid, &(job->grp), job->buffer, buffer_size, &(job->ptr));
}

CAMLprim value lwt_unix_getgrgid_job(value val_gid)
{
  struct job_getgrgid *job = lwt_unix_new(struct job_getgrgid);
  job->job.worker = (lwt_unix_job_worker)worker_getgrgid;
  job->gid = Int_val(val_gid);
  job->buffer = NULL;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getgrgid_result(value val_job)
{
  struct job_getgrgid *job = Job_getgrgid_val(val_job);
  if (job->result != 0) unix_error(job->result, "getgrgid", Nothing);
  if (job->ptr == NULL) caml_raise_not_found();
  return alloc_group_entry(&(job->grp));
}

CAMLprim value lwt_unix_getgrgid_free(value val_job)
{
  struct job_getgrgid *job = Job_getgrgid_val(val_job);
  if (job->buffer != NULL) free(job->buffer);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: gethostname                                                |
   +-----------------------------------------------------------------+ */

struct job_gethostname {
  struct lwt_unix_job job;
  char *buffer;
  int result;
  int error_code;
};

#define Job_gethostname_val(v) *(struct job_gethostname**)Data_custom_val(v)

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
    } else {
      job->buffer[buffer_size] = '\0';
      job->result = err;
      job->error_code = errno;
      break;
    }
  }
}

CAMLprim value lwt_unix_gethostname_job()
{
  struct job_gethostname *job = lwt_unix_new(struct job_gethostname);
  job->job.worker = (lwt_unix_job_worker)worker_gethostname;
  job->buffer = NULL;
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_gethostname_result(value val_job)
{
  struct job_gethostname *job = Job_gethostname_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "gethostname", Nothing);
  return caml_copy_string(job->buffer);
}

CAMLprim value lwt_unix_gethostname_free(value val_job)
{
  struct job_gethostname *job = Job_gethostname_val(val_job);
  free(job->buffer);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: gethostbyname                                              |
   +-----------------------------------------------------------------+ */

#define NETDB_BUFFER_SIZE 10000

struct job_gethostbyname {
  struct lwt_unix_job job;
  char *name;
  struct hostent entry;
  struct hostent *ptr;
  char buffer[NETDB_BUFFER_SIZE];
};

#define Job_gethostbyname_val(v) *(struct job_gethostbyname**)Data_custom_val(v)

CAMLexport value alloc_inet_addr (struct in_addr * inaddr);
#define GET_INET_ADDR(v) (*((struct in_addr *) (v)))

CAMLexport value alloc_inet6_addr (struct in6_addr * inaddr);
#define GET_INET6_ADDR(v) (*((struct in6_addr *) (v)))

static value alloc_one_addr(char const *a)
{
  struct in_addr addr;
  memmove (&addr, a, 4);
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

  Begin_roots4 (name, aliases, addr_list, adr);
    name = copy_string((char *)(entry->h_name));
    /* PR#4043: protect against buggy implementations of gethostbynamee()
       that return a NULL pointer in h_aliases */
    if (entry->h_aliases)
      aliases = copy_string_array((const char**)entry->h_aliases);
    else
      aliases = Atom(0);
    if (entry->h_length == 16)
      addr_list = alloc_array(alloc_one_addr6, (const char**)entry->h_addr_list);
    else
      addr_list = alloc_array(alloc_one_addr, (const char**)entry->h_addr_list);
    res = alloc_small(4, 0);
    Field(res, 0) = name;
    Field(res, 1) = aliases;
    switch (entry->h_addrtype) {
    case PF_UNIX:          Field(res, 2) = Val_int(0); break;
    case PF_INET:          Field(res, 2) = Val_int(1); break;
    default: /*PF_INET6 */ Field(res, 2) = Val_int(2); break;
    }
    Field(res, 3) = addr_list;
  End_roots();
  return res;
}

static void worker_gethostbyname(struct job_gethostbyname *job)
{
  int h_errno;
#if HAS_GETHOSTBYNAME_R == 5
  job->ptr = gethostbyname_r(job->name, &(job->entry), job->buffer, NETDB_BUFFER_SIZE, &h_errno);
#elif HAS_GETHOSTBYNAME_R == 6
  if (gethostbyname_r(job->name, &(job->entry), job->buffer, NETDB_BUFFER_SIZE, &(job->ptr), &h_errno) != 0)
    job->ptr = NULL;
#else
  job->ptr = NULL;
#endif
}

CAMLprim value lwt_unix_gethostbyname_job(value val_name)
{
  struct job_gethostbyname *job = lwt_unix_new(struct job_gethostbyname);
  job->job.worker = (lwt_unix_job_worker)worker_gethostbyname;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_gethostbyname_result(value val_job)
{
  struct job_gethostbyname *job = Job_gethostbyname_val(val_job);
  if (job->ptr == NULL) caml_raise_not_found();
  return alloc_host_entry(&(job->entry));
}

CAMLprim value lwt_unix_gethostbyname_free(value val_job)
{
  struct job_gethostbyname *job = Job_gethostbyname_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: gethostbyaddr                                              |
   +-----------------------------------------------------------------+ */

struct job_gethostbyaddr {
  struct lwt_unix_job job;
  struct in_addr addr;
  struct hostent entry;
  struct hostent *ptr;
  char buffer[NETDB_BUFFER_SIZE];
};

#define Job_gethostbyaddr_val(v) *(struct job_gethostbyaddr**)Data_custom_val(v)

static void worker_gethostbyaddr(struct job_gethostbyaddr *job)
{
  int h_errno;
#if HAS_GETHOSTBYADDR_R == 7
  job->ptr = gethostbyaddr_r(&(job->addr), 4, AF_INET, &(job->entry), job->buffer, NETDB_BUFFER_SIZE, &h_errno);
#elif HAS_GETHOSTBYADDR_R == 8
  if (gethostbyaddr_r(&(job->addr), 4, AF_INET, &(job->entry), job->buffer, NETDB_BUFFER_SIZE, &(job->ptr), &h_errno) != 0)
    job->ptr = NULL;
#else
  job->ptr = NULL;
#endif
}

CAMLprim value lwt_unix_gethostbyaddr_job(value val_addr)
{
  struct job_gethostbyaddr *job = lwt_unix_new(struct job_gethostbyaddr);
  job->job.worker = (lwt_unix_job_worker)worker_gethostbyaddr;
  job->addr = GET_INET_ADDR(val_addr);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_gethostbyaddr_result(value val_job)
{
  struct job_gethostbyaddr *job = Job_gethostbyaddr_val(val_job);
  if (job->ptr == NULL) caml_raise_not_found();
  return alloc_host_entry(&(job->entry));
}

CAMLprim value lwt_unix_gethostbyaddr_free(value val_job)
{
  struct job_gethostbyaddr *job = Job_gethostbyaddr_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: getprotobyname                                             |
   +-----------------------------------------------------------------+ */

struct job_getprotobyname {
  struct lwt_unix_job job;
  char *name;
  struct protoent *result;
};

#define Job_getprotobyname_val(v) *(struct job_getprotobyname**)Data_custom_val(v)

static value alloc_proto_entry(struct protoent *entry)
{
  value res;
  value name = Val_unit, aliases = Val_unit;

  Begin_roots2 (name, aliases);
    name = copy_string(entry->p_name);
    aliases = copy_string_array((const char**)entry->p_aliases);
    res = alloc_small(3, 0);
    Field(res,0) = name;
    Field(res,1) = aliases;
    Field(res,2) = Val_int(entry->p_proto);
  End_roots();
  return res;
}

static void worker_getprotobyname(struct job_getprotobyname *job)
{
  job->result = getprotobyname(job->name);
}

CAMLprim value lwt_unix_getprotobyname_job(value val_name)
{
  struct job_getprotobyname *job = lwt_unix_new(struct job_getprotobyname);
  job->job.worker = (lwt_unix_job_worker)worker_getprotobyname;
  job->name = lwt_unix_strdup(String_val(val_name));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getprotobyname_result(value val_job)
{
  struct job_getprotobyname *job = Job_getprotobyname_val(val_job);
  if (job->result == NULL) caml_raise_not_found();
  return alloc_proto_entry(job->result);
}

CAMLprim value lwt_unix_getprotobyname_free(value val_job)
{
  struct job_getprotobyname *job = Job_getprotobyname_val(val_job);
  free(job->name);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: getprotobynumber                                           |
   +-----------------------------------------------------------------+ */

struct job_getprotobynumber {
  struct lwt_unix_job job;
  int number;
  struct protoent *result;
};

#define Job_getprotobynumber_val(v) *(struct job_getprotobynumber**)Data_custom_val(v)

static void worker_getprotobynumber(struct job_getprotobynumber *job)
{
  job->result = getprotobynumber(job->number);
}

CAMLprim value lwt_unix_getprotobynumber_job(value val_number)
{
  struct job_getprotobynumber *job = lwt_unix_new(struct job_getprotobynumber);
  job->job.worker = (lwt_unix_job_worker)worker_getprotobynumber;
  job->number = Int_val(val_number);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getprotobynumber_result(value val_job)
{
  struct job_getprotobynumber *job = Job_getprotobynumber_val(val_job);
  if (job->result == NULL) caml_raise_not_found();
  return alloc_proto_entry(job->result);
}

CAMLprim value lwt_unix_getprotobynumber_free(value val_job)
{
  struct job_getprotobynumber *job = Job_getprotobynumber_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: getservbyname                                              |
   +-----------------------------------------------------------------+ */

struct job_getservbyname {
  struct lwt_unix_job job;
  char *name;
  char *proto;
  struct servent *result;
};

#define Job_getservbyname_val(v) *(struct job_getservbyname**)Data_custom_val(v)

static value alloc_service_entry(struct servent *entry)
{
  value res;
  value name = Val_unit, aliases = Val_unit, proto = Val_unit;

  Begin_roots3 (name, aliases, proto);
    name = copy_string(entry->s_name);
    aliases = copy_string_array((const char**)entry->s_aliases);
    proto = copy_string(entry->s_proto);
    res = alloc_small(4, 0);
    Field(res,0) = name;
    Field(res,1) = aliases;
    Field(res,2) = Val_int(ntohs(entry->s_port));
    Field(res,3) = proto;
  End_roots();
  return res;
}

static void worker_getservbyname(struct job_getservbyname *job)
{
  job->result = getservbyname(job->name, job->proto);
}

CAMLprim value lwt_unix_getservbyname_job(value val_name, value val_proto)
{
  struct job_getservbyname *job = lwt_unix_new(struct job_getservbyname);
  job->job.worker = (lwt_unix_job_worker)worker_getservbyname;
  job->name = lwt_unix_strdup(String_val(val_name));
  job->proto = lwt_unix_strdup(String_val(val_proto));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getservbyname_result(value val_job)
{
  struct job_getservbyname *job = Job_getservbyname_val(val_job);
  if (job->result == NULL) caml_raise_not_found();
  return alloc_service_entry(job->result);
}

CAMLprim value lwt_unix_getservbyname_free(value val_job)
{
  struct job_getservbyname *job = Job_getservbyname_val(val_job);
  free(job->name);
  free(job->proto);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: getservbyport                                              |
   +-----------------------------------------------------------------+ */

struct job_getservbyport {
  struct lwt_unix_job job;
  int port;
  char *proto;
  struct servent *result;
};

#define Job_getservbyport_val(v) *(struct job_getservbyport**)Data_custom_val(v)

static void worker_getservbyport(struct job_getservbyport *job)
{
  job->result = getservbyport(job->port, job->proto);
}

CAMLprim value lwt_unix_getservbyport_job(value val_port, value val_proto)
{
  struct job_getservbyport *job = lwt_unix_new(struct job_getservbyport);
  job->job.worker = (lwt_unix_job_worker)worker_getservbyport;
  job->port = Int_val(val_port);
  job->proto = lwt_unix_strdup(String_val(val_proto));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getservbyport_result(value val_job)
{
  struct job_getservbyport *job = Job_getservbyport_val(val_job);
  if (job->result == NULL) caml_raise_not_found();
  return alloc_service_entry(job->result);
}

CAMLprim value lwt_unix_getservbyport_free(value val_job)
{
  struct job_getservbyport *job = Job_getservbyport_val(val_job);
  free(job->proto);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

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
};

#define Job_getaddrinfo_val(v) *(struct job_getaddrinfo**)Data_custom_val(v)

value cst_to_constr(int n, int *tbl, int size, int deflt)
{
  int i;
  for (i = 0; i < size; i++)
    if (n == tbl[i]) return Val_int(i);
  return Val_int(deflt);
}

static value convert_addrinfo(struct addrinfo * a)
{
  CAMLparam0();
  CAMLlocal3(vres,vaddr,vcanonname);
  union sock_addr_union sa;
  socklen_t len;

  len = a->ai_addrlen;
  if (len > sizeof(sa)) len = sizeof(sa);
  memcpy(&sa.s_gen, a->ai_addr, len);
  vaddr = alloc_sockaddr(&sa, len, -1);
  vcanonname = copy_string(a->ai_canonname == NULL ? "" : a->ai_canonname);
  vres = alloc_small(5, 0);
  Field(vres, 0) = cst_to_constr(a->ai_family, socket_domain_table, 3, 0);
  Field(vres, 1) = cst_to_constr(a->ai_socktype, socket_type_table, 4, 0);
  Field(vres, 2) = Val_int(a->ai_protocol);
  Field(vres, 3) = vaddr;
  Field(vres, 4) = vcanonname;
  CAMLreturn(vres);
}

static void worker_getaddrinfo(struct job_getaddrinfo *job)
{
  job->result = getaddrinfo(job->node, job->service, &(job->hints), &(job->info));
}

CAMLprim value lwt_unix_getaddrinfo_job(value val_node, value val_service, value val_hints)
{
  struct job_getaddrinfo *job = lwt_unix_new(struct job_getaddrinfo);
  job->job.worker = (lwt_unix_job_worker)worker_getaddrinfo;
  job->node = caml_string_length(val_node) == 0 ? NULL : lwt_unix_strdup(String_val(val_node));
  job->service = caml_string_length(val_service) == 0 ? NULL : lwt_unix_strdup(String_val(val_service));
  job->info = NULL;
  memset(&(job->hints), 0, sizeof(struct addrinfo));
  job->hints.ai_family = PF_UNSPEC;
  for (/*nothing*/; Is_block(val_hints); val_hints = Field(val_hints, 1)) {
    value v = Field(val_hints, 0);
    if (Is_block(v))
      switch (Tag_val(v)) {
      case 0:                   /* AI_FAMILY of socket_domain */
        job->hints.ai_family = socket_domain_table[Int_val(Field(v, 0))];
        break;
      case 1:                   /* AI_SOCKTYPE of socket_type */
        job->hints.ai_socktype = socket_type_table[Int_val(Field(v, 0))];
        break;
      case 2:                   /* AI_PROTOCOL of int */
        job->hints.ai_protocol = Int_val(Field(v, 0));
        break;
      }
    else
      switch (Int_val(v)) {
      case 0:                   /* AI_NUMERICHOST */
        job->hints.ai_flags |= AI_NUMERICHOST; break;
      case 1:                   /* AI_CANONNAME */
        job->hints.ai_flags |= AI_CANONNAME; break;
      case 2:                   /* AI_PASSIVE */
        job->hints.ai_flags |= AI_PASSIVE; break;
      }
  }
 return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getaddrinfo_result(value val_job)
{
  CAMLparam1(val_job);
  CAMLlocal3(vres, e, v);
  struct addrinfo *r;
  struct job_getaddrinfo *job = Job_getaddrinfo_val(val_job);
  if (job->result != 0) unix_error(job->result, "getaddrinfo", Nothing);
  vres = Val_int(0);
  for (r = job->info; r != NULL; r = r->ai_next) {
    e = convert_addrinfo(r);
    v = alloc_small(2, 0);
    Field(v, 0) = e;
    Field(v, 1) = vres;
    vres = v;
  }
  CAMLreturn(vres);
}

CAMLprim value lwt_unix_getaddrinfo_free(value val_job)
{
  struct job_getaddrinfo *job = Job_getaddrinfo_val(val_job);
  if (job->node != NULL) free(job->node);
  if (job->service != NULL) free(job->service);
  if (job->info != NULL) freeaddrinfo(job->info);
  lwt_unix_free_job(&job->job);
  return Val_unit;
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
  struct addrinfo *info;
  int result;
};

#define Job_getnameinfo_val(v) *(struct job_getnameinfo**)Data_custom_val(v)

static int getnameinfo_flag_table[] = {
  NI_NOFQDN, NI_NUMERICHOST, NI_NAMEREQD, NI_NUMERICSERV, NI_DGRAM
};

static void worker_getnameinfo(struct job_getnameinfo *job)
{
  job->result = getnameinfo((const struct sockaddr *)&(job->addr.s_gen), job->addr_len,
                            job->host, sizeof(job->host), job->serv, sizeof(job->serv),
                            job->opts);
}

CAMLprim value lwt_unix_getnameinfo_job(value val_sockaddr, value val_opts)
{
  struct job_getnameinfo *job = lwt_unix_new(struct job_getnameinfo);
  job->job.worker = (lwt_unix_job_worker)worker_getnameinfo;
  get_sockaddr(val_sockaddr, &(job->addr), &(job->addr_len));
  job->opts = convert_flag_list(val_opts, getnameinfo_flag_table);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_getnameinfo_result(value val_job)
{
  CAMLparam1(val_job);
  CAMLlocal3(vres, vhost, vserv);
  struct job_getnameinfo *job = Job_getnameinfo_val(val_job);
  if (job->result != 0) caml_raise_not_found();
  vhost = caml_copy_string(job->host);
  vserv = caml_copy_string(job->serv);
  vres = alloc_small(2, 0);
  Field(vres, 0) = vhost;
  Field(vres, 1) = vserv;
  CAMLreturn(vres);
}

CAMLprim value lwt_unix_getnameinfo_free(value val_job)
{
  struct job_getnameinfo *job = Job_getnameinfo_val(val_job);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Termios conversion                                              |
   +-----------------------------------------------------------------+ */

/* TODO: make it reentrant. */

static struct termios terminal_status;

enum { Bool, Enum, Speed, Char, End };

enum { Input, Output };

#define iflags ((long)(&terminal_status.c_iflag))
#define oflags ((long)(&terminal_status.c_oflag))
#define cflags ((long)(&terminal_status.c_cflag))
#define lflags ((long)(&terminal_status.c_lflag))

/* Number of fields in the terminal_io record field. Cf. unix.mli */

#define NFIELDS 38

/* Structure of the terminal_io record. Cf. unix.mli */

static long terminal_io_descr[] = {
  /* Input modes */
  Bool, iflags, IGNBRK,
  Bool, iflags, BRKINT,
  Bool, iflags, IGNPAR,
  Bool, iflags, PARMRK,
  Bool, iflags, INPCK,
  Bool, iflags, ISTRIP,
  Bool, iflags, INLCR,
  Bool, iflags, IGNCR,
  Bool, iflags, ICRNL,
  Bool, iflags, IXON,
  Bool, iflags, IXOFF,
  /* Output modes */
  Bool, oflags, OPOST,
  /* Control modes */
  Speed, Output,
  Speed, Input,
  Enum, cflags, 5, 4, CSIZE, CS5, CS6, CS7, CS8,
  Enum, cflags, 1, 2, CSTOPB, 0, CSTOPB,
  Bool, cflags, CREAD,
  Bool, cflags, PARENB,
  Bool, cflags, PARODD,
  Bool, cflags, HUPCL,
  Bool, cflags, CLOCAL,
  /* Local modes */
  Bool, lflags, ISIG,
  Bool, lflags, ICANON,
  Bool, lflags, NOFLSH,
  Bool, lflags, ECHO,
  Bool, lflags, ECHOE,
  Bool, lflags, ECHOK,
  Bool, lflags, ECHONL,
  /* Control characters */
  Char, VINTR,
  Char, VQUIT,
  Char, VERASE,
  Char, VKILL,
  Char, VEOF,
  Char, VEOL,
  Char, VMIN,
  Char, VTIME,
  Char, VSTART,
  Char, VSTOP,
  End
};

#undef iflags
#undef oflags
#undef cflags
#undef lflags

struct speedtable_entry ;

static struct {
  speed_t speed;
  int baud;
} speedtable[] = {
  {B50,      50},
  {B75,      75},
  {B110,     110},
  {B134,     134},
  {B150,     150},
  {B300,     300},
  {B600,     600},
  {B1200,    1200},
  {B1800,    1800},
  {B2400,    2400},
  {B4800,    4800},
  {B9600,    9600},
  {B19200,   19200},
  {B38400,   38400},
#ifdef B57600
  {B57600,   57600},
#endif
#ifdef B115200
  {B115200,  115200},
#endif
#ifdef B230400
  {B230400,  230400},
#endif
  {B0,       0}
};

#define NSPEEDS (sizeof(speedtable) / sizeof(speedtable[0]))

static void encode_terminal_status(value *dst)
{
  long * pc;
  int i;

  for(pc = terminal_io_descr; *pc != End; dst++) {
    switch(*pc++) {
    case Bool:
      { int * src = (int *) (*pc++);
        int msk = *pc++;
        *dst = Val_bool(*src & msk);
        break; }
    case Enum:
      { int * src = (int *) (*pc++);
        int ofs = *pc++;
        int num = *pc++;
        int msk = *pc++;
        for (i = 0; i < num; i++) {
          if ((*src & msk) == pc[i]) {
            *dst = Val_int(i + ofs);
            break;
          }
        }
        pc += num;
        break; }
    case Speed:
      { int which = *pc++;
        speed_t speed = 0;
        *dst = Val_int(9600);   /* in case no speed in speedtable matches */
        switch (which) {
        case Output:
          speed = cfgetospeed(&terminal_status); break;
        case Input:
          speed = cfgetispeed(&terminal_status); break;
        }
        for (i = 0; i < NSPEEDS; i++) {
          if (speed == speedtable[i].speed) {
            *dst = Val_int(speedtable[i].baud);
            break;
          }
        }
        break; }
    case Char:
      { int which = *pc++;
        *dst = Val_int(terminal_status.c_cc[which]);
        break; }
    }
  }
}

static void decode_terminal_status(value *src)
{
  long * pc;
  int i;

  for (pc = terminal_io_descr; *pc != End; src++) {
    switch(*pc++) {
    case Bool:
      { int * dst = (int *) (*pc++);
        int msk = *pc++;
        if (Bool_val(*src))
          *dst |= msk;
        else
          *dst &= ~msk;
        break; }
    case Enum:
      { int * dst = (int *) (*pc++);
        int ofs = *pc++;
        int num = *pc++;
        int msk = *pc++;
        i = Int_val(*src) - ofs;
        if (i >= 0 && i < num) {
          *dst = (*dst & ~msk) | pc[i];
        } else {
          unix_error(EINVAL, "tcsetattr", Nothing);
        }
        pc += num;
        break; }
    case Speed:
      { int which = *pc++;
        int baud = Int_val(*src);
        int res = 0;
        for (i = 0; i < NSPEEDS; i++) {
          if (baud == speedtable[i].baud) {
            switch (which) {
            case Output:
              res = cfsetospeed(&terminal_status, speedtable[i].speed); break;
            case Input:
              res = cfsetispeed(&terminal_status, speedtable[i].speed); break;
            }
            if (res == -1) uerror("tcsetattr", Nothing);
            goto ok;
          }
        }
        unix_error(EINVAL, "tcsetattr", Nothing);
      ok:
        break; }
    case Char:
      { int which = *pc++;
        terminal_status.c_cc[which] = Int_val(*src);
        break; }
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

#define Job_tcgetattr_val(v) *(struct job_tcgetattr**)Data_custom_val(v)

static void worker_tcgetattr(struct job_tcgetattr *job)
{
  job->result = tcgetattr(job->fd, &(job->termios));
  job->error_code = errno;
}

CAMLprim value lwt_unix_tcgetattr_job(value val_fd)
{
  struct job_tcgetattr *job = lwt_unix_new(struct job_tcgetattr);
  job->job.worker = (lwt_unix_job_worker)worker_tcgetattr;
  job->fd = Int_val(val_fd);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_tcgetattr_result(value val_job)
{
  struct job_tcgetattr *job = Job_tcgetattr_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "tcgetattr", Nothing);
  value res = alloc_tuple(NFIELDS);
  memcpy(&terminal_status, &(job->termios), sizeof(struct termios));
  encode_terminal_status(&Field(res, 0));
  return res;
}

CAMLprim value lwt_unix_tcgetattr_free(value val_job)
{
  lwt_unix_free_job(&(Job_tcgetattr_val(val_job))->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: tcsetattr                                                  |
   +-----------------------------------------------------------------+ */

struct job_tcsetattr {
  struct lwt_unix_job job;
  int fd;
  int when;
  struct termios termios;
  int result;
  int error_code;
};

#define Job_tcsetattr_val(v) *(struct job_tcsetattr**)Data_custom_val(v)

static int when_flag_table[] = {
  TCSANOW, TCSADRAIN, TCSAFLUSH
};

static void worker_tcsetattr(struct job_tcsetattr *job)
{
  job->result = tcsetattr(job->fd, job->when, &(job->termios));
  job->error_code = errno;
}

CAMLprim value lwt_unix_tcsetattr_job(value val_fd, value val_when, value val_termios)
{
  struct job_tcsetattr *job = lwt_unix_new(struct job_tcsetattr);
  job->job.worker = (lwt_unix_job_worker)worker_tcsetattr;
  job->fd = Int_val(val_fd);
  job->when = when_flag_table[Int_val(val_when)];
  decode_terminal_status(&Field(val_termios, 0));
  memcpy(&(job->termios), &terminal_status, sizeof(struct termios));
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_tcsetattr_result(value val_job)
{
  struct job_tcsetattr *job = Job_tcsetattr_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "tcsetattr", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_tcsetattr_free(value val_job)
{
  lwt_unix_free_job(&(Job_tcsetattr_val(val_job))->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: tcdrain                                                    |
   +-----------------------------------------------------------------+ */

struct job_tcdrain {
  struct lwt_unix_job job;
  int fd;
  int result;
  int error_code;
};

#define Job_tcdrain_val(v) *(struct job_tcdrain**)Data_custom_val(v)

static void worker_tcdrain(struct job_tcdrain *job)
{
  job->result = tcdrain(job->fd);
  job->error_code = errno;
}

CAMLprim value lwt_unix_tcdrain_job(value val_fd)
{
  struct job_tcdrain *job = lwt_unix_new(struct job_tcdrain);
  job->job.worker = (lwt_unix_job_worker)worker_tcdrain;
  job->fd = Int_val(val_fd);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_tcdrain_result(value val_job)
{
  struct job_tcdrain *job = Job_tcdrain_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "tcdrain", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_tcdrain_free(value val_job)
{
  lwt_unix_free_job(&(Job_tcdrain_val(val_job))->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: tcflush                                                    |
   +-----------------------------------------------------------------+ */

struct job_tcflush {
  struct lwt_unix_job job;
  int fd;
  int queue;
  int result;
  int error_code;
};

#define Job_tcflush_val(v) *(struct job_tcflush**)Data_custom_val(v)

static int queue_flag_table[] = {
  TCIFLUSH, TCOFLUSH, TCIOFLUSH
};

static void worker_tcflush(struct job_tcflush *job)
{
  job->result = tcflush(job->fd, job->queue);
  job->error_code = errno;
}

CAMLprim value lwt_unix_tcflush_job(value val_fd, value val_queue)
{
  struct job_tcflush *job = lwt_unix_new(struct job_tcflush);
  job->job.worker = (lwt_unix_job_worker)worker_tcflush;
  job->fd = Int_val(val_fd);
  job->queue = queue_flag_table[Int_val(val_queue)];
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_tcflush_result(value val_job)
{
  struct job_tcflush *job = Job_tcflush_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "tcflush", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_tcflush_free(value val_job)
{
  lwt_unix_free_job(&(Job_tcflush_val(val_job))->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: tcflow                                                     |
   +-----------------------------------------------------------------+ */

struct job_tcflow {
  struct lwt_unix_job job;
  int fd;
  int action;
  int result;
  int error_code;
};

#define Job_tcflow_val(v) *(struct job_tcflow**)Data_custom_val(v)

static int action_flag_table[] = {
  TCOOFF, TCOON, TCIOFF, TCION
};

static void worker_tcflow(struct job_tcflow *job)
{
  job->result = tcflow(job->fd, job->action);
  job->error_code = errno;
}

CAMLprim value lwt_unix_tcflow_job(value val_fd, value val_action)
{
  struct job_tcflow *job = lwt_unix_new(struct job_tcflow);
  job->job.worker = (lwt_unix_job_worker)worker_tcflow;
  job->fd = Int_val(val_fd);
  job->action = action_flag_table[Int_val(val_action)];
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_tcflow_result(value val_job)
{
  struct job_tcflow *job = Job_tcflow_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "tcflow", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_tcflow_free(value val_job)
{
  lwt_unix_free_job(&(Job_tcflow_val(val_job))->job);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | JOB: tcsendbreak                                                |
   +-----------------------------------------------------------------+ */

struct job_tcsendbreak {
  struct lwt_unix_job job;
  int fd;
  int delay;
  int result;
  int error_code;
};

#define Job_tcsendbreak_val(v) *(struct job_tcsendbreak**)Data_custom_val(v)

static void worker_tcsendbreak(struct job_tcsendbreak *job)
{
  job->result = tcsendbreak(job->fd, job->delay);
  job->error_code = errno;
}

CAMLprim value lwt_unix_tcsendbreak_job(value val_fd, value val_delay)
{
  struct job_tcsendbreak *job = lwt_unix_new(struct job_tcsendbreak);
  job->job.worker = (lwt_unix_job_worker)worker_tcsendbreak;
  job->fd = Int_val(val_fd);
  job->delay = Int_val(val_delay);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_tcsendbreak_result(value val_job)
{
  struct job_tcsendbreak *job = Job_tcsendbreak_val(val_job);
  if (job->result < 0) unix_error(job->error_code, "tcsendbreak", Nothing);
  return Val_unit;
}

CAMLprim value lwt_unix_tcsendbreak_free(value val_job)
{
  lwt_unix_free_job(&(Job_tcsendbreak_val(val_job))->job);
  return Val_unit;
}
