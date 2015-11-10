/* Lightweight thread library for OCaml
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

#define ARGS(args...) args

#include <sys/uio.h>
#include <sys/un.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <poll.h>

/* +-----------------------------------------------------------------+
   | Test for readability/writability                                |
   +-----------------------------------------------------------------+ */

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

CAMLprim value lwt_unix_get_page_size(value Unit)
{
  long page_size = sysconf(_SC_PAGESIZE);
  if (page_size < 0) page_size = 4096;
  return Val_long(page_size);
}

#ifdef __CYGWIN__
LWT_NOT_AVAILABLE4(unix_mincore)
#else
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
#endif

/* +-----------------------------------------------------------------+
   | read/write                                                      |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_unix_read(value val_fd, value val_buf, value val_ofs, value val_len)
{
  long ret;
  ret = read(Int_val(val_fd), &Byte(String_val(val_buf), Long_val(val_ofs)), Long_val(val_len));
  if (ret == -1) uerror("read", Nothing);
  return Val_long(ret);
}

CAMLprim value lwt_unix_bytes_read(value val_fd, value val_buf, value val_ofs, value val_len)
{
  long ret;
  ret = read(Int_val(val_fd), (char*)Caml_ba_array_val(val_buf)->data + Long_val(val_ofs), Long_val(val_len));
  if (ret == -1) uerror("read", Nothing);
  return Val_long(ret);
}

CAMLprim value lwt_unix_write(value val_fd, value val_buf, value val_ofs, value val_len)
{
  long ret;
  ret = write(Int_val(val_fd), &Byte(String_val(val_buf), Long_val(val_ofs)), Long_val(val_len));
  if (ret == -1) uerror("write", Nothing);
  return Val_long(ret);
}

CAMLprim value lwt_unix_bytes_write(value val_fd, value val_buf, value val_ofs, value val_len)
{
  long ret;
  ret = write(Int_val(val_fd), (char*)Caml_ba_array_val(val_buf)->data + Long_val(val_ofs), Long_val(val_len));
  if (ret == -1) uerror("write", Nothing);
  return Val_long(ret);
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

#if defined(HAVE_GET_CREDENTIALS_LINUX)
#  define CREDENTIALS_TYPE struct ucred
#  define CREDENTIALS_FIELD(id) id
#elif defined(HAVE_GET_CREDENTIALS_NETBSD)
#  define CREDENTIALS_TYPE struct sockcred
#  define CREDENTIALS_FIELD(id) sc_ ## id
#elif defined(HAVE_GET_CREDENTIALS_OPENBSD)
#  define CREDENTIALS_TYPE struct sockpeercred
#  define CREDENTIALS_FIELD(id) id
#elif defined(HAVE_GET_CREDENTIALS_FREEBSD)
#  define CREDENTIALS_TYPE struct cmsgcred
#  define CREDENTIALS_FIELD(id) cmsgcred_ ## id
#endif

#if defined(CREDENTIALS_TYPE)

CAMLprim value lwt_unix_get_credentials(value fd)
{
    CAMLparam1(fd);
    CAMLlocal1(res);
    CREDENTIALS_TYPE cred;
    socklen_t cred_len = sizeof(cred);

    if (getsockopt(Int_val(fd), SOL_SOCKET, SO_PEERCRED, &cred, &cred_len) == -1)
      uerror("get_credentials", Nothing);

    res = caml_alloc_tuple(3);
    Store_field(res, 0, Val_int(cred.CREDENTIALS_FIELD(pid)));
    Store_field(res, 1, Val_int(cred.CREDENTIALS_FIELD(uid)));
    Store_field(res, 2, Val_int(cred.CREDENTIALS_FIELD(gid)));
    CAMLreturn(res);
}

#elif defined(HAVE_GETPEEREID)

CAMLprim value lwt_unix_get_credentials(value fd)
{
    CAMLparam1(fd);
    CAMLlocal1(res);
    uid_t euid;
    gid_t egid;

    if (getpeereid(Int_val(fd), &euid, &egid) == -1)
      uerror("get_credentials", Nothing);

    res = caml_alloc_tuple(3);
    Store_field(res, 0, Val_int(-1));
    Store_field(res, 1, Val_int(euid));
    Store_field(res, 2, Val_int(egid));
    CAMLreturn(res);
}

#else

LWT_NOT_AVAILABLE1(unix_get_credentials)

#endif

/* +-----------------------------------------------------------------+
   | Multicast functions                                             |
   +-----------------------------------------------------------------+ */

static int socket_domain (int fd)
{
    /* Return the socket domain, PF_INET or PF_INET6. Fails for non-IP
       protos.
       fd must be a socket!
    */
    union sock_addr_union addr;
    socklen_t l;

    l = sizeof(addr);
    if (getsockname(fd, &addr.s_gen, &l) == -1)
        uerror("getsockname", Nothing);

    switch (addr.s_gen.sa_family) {
    case AF_INET:
        return PF_INET;
    case AF_INET6:
        return PF_INET6;
    default:
        invalid_argument("Not an Internet socket");
    }

    return 0;
}


CAMLprim value lwt_unix_mcast_set_loop (value fd, value flag)
{
    int t, r, f;

    t = socket_domain(Int_val(fd));
    f = Bool_val(flag);
    r = 0;

    switch (t) {
    case PF_INET:
        r = setsockopt (Int_val(fd), IPPROTO_IP, IP_MULTICAST_LOOP, (void *) &f, sizeof(f));
        break;
    default:
        invalid_argument("lwt_unix_mcast_set_loop");
    };

    if (r == -1)
        uerror("setsockopt",Nothing);

    return Val_unit;
}


CAMLprim value lwt_unix_mcast_set_ttl (value fd, value ttl)
{
    int t, r, v;
    int fd_sock;

    fd_sock = Int_val(fd);
    t = socket_domain(fd_sock);
    v = Int_val(ttl);
    r = 0;

    switch (t) {
    case PF_INET:
        r = setsockopt(fd_sock, IPPROTO_IP, IP_MULTICAST_TTL, (void *) &v, sizeof(v));
        break;
    default:
        invalid_argument("lwt_unix_mcast_set_ttl");
    };

    if (r == -1)
        uerror("setsockopt",Nothing);

    return Val_unit;
}

/* Keep this in sync with the type Lwt_unix.mcast_action */
#define VAL_MCAST_ACTION_ADD  (Val_int(0))
#define VAL_MCAST_ACTION_DROP (Val_int(1))

#define GET_INET_ADDR(v)      (*((struct in_addr *) (v)))

CAMLprim value lwt_unix_mcast_modify_membership (value fd, value v_action, value if_addr, value group_addr)
{
    int t, r;
    int fd_sock;
    int optname;

    fd_sock = Int_val(fd);
    t = socket_domain(fd_sock);
    r = 0;

    switch (t) {
    case PF_INET: {
        struct ip_mreq mreq;

        if (string_length(group_addr) != 4 || string_length(if_addr) != 4 )
            invalid_argument("lwt_unix_mcast_modify: Not an IPV4 address");

        memcpy(&mreq.imr_multiaddr, &GET_INET_ADDR(group_addr), 4);
        memcpy(&mreq.imr_interface, &GET_INET_ADDR(if_addr), 4);

        switch (v_action) {
        case VAL_MCAST_ACTION_ADD:
            optname = IP_ADD_MEMBERSHIP;
            break;

        default:
            optname = IP_DROP_MEMBERSHIP;
            break;
        }

        r = setsockopt(fd_sock, IPPROTO_IP, optname, (void *) &mreq, sizeof(mreq));
        break;
    }
    default:
        invalid_argument("lwt_unix_mcast_modify_membership");
    };

    if (r == -1)
        uerror("setsockopt", Nothing);

    return Val_unit;
}


/* +-----------------------------------------------------------------+
   | wait4                                                           |
   +-----------------------------------------------------------------+ */

/* Some code duplicated from OCaml's otherlibs/unix/wait.c */

CAMLextern int caml_convert_signal_number (int);
CAMLextern int caml_rev_convert_signal_number (int);

#if !defined(__ANDROID__)

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
  job->ptr = (char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
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
  O_RSYNC,
  0,
#ifdef O_CLOEXEC
  O_CLOEXEC
#else
#define NEED_CLOEXEC_EMULATION
  0
#endif
};

#ifdef NEED_CLOEXEC_EMULATION
static int open_cloexec_table[14] = {
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0,
  0,
  1
};
#endif

struct job_open {
  struct lwt_unix_job job;
  int flags;
  int perms;
  int fd;
  int blocking;
  int error_code;
  char *name;
  char data[];
};

static void worker_open(struct job_open *job)
{
  int fd;
  fd = open(job->name, job->flags, job->perms);
#if defined(NEED_CLOEXEC_EMULATION) && defined(FD_CLOEXEC)
  if (fd >= 0 && job->fd) {
    int flags = fcntl(fd, F_GETFD, 0);
    if (flags == -1 ||
        fcntl(fd, F_SETFD, flags | FD_CLOEXEC) == -1) {
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
#ifdef NEED_CLOEXEC_EMULATION
  job->fd = convert_flag_list(flags, open_cloexec_table) != 0;
#endif
  job->flags = convert_flag_list(flags, open_flag_table);
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

CAMLprim value lwt_unix_read_job(value val_fd, value val_buffer, value val_offset, value val_length)
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

CAMLprim value lwt_unix_bytes_read_job(value val_fd, value val_buf, value val_ofs, value val_len)
{
  LWT_UNIX_INIT_JOB(job, bytes_read, 0);
  job->fd = Int_val(val_fd);
  job->buffer = (char*)Caml_ba_data_val(val_buf) + Long_val(val_ofs);
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

CAMLprim value lwt_unix_write_job(value val_fd, value val_string, value val_offset, value val_length)
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

CAMLprim value lwt_unix_bytes_write_job(value val_fd, value val_buffer, value val_offset, value val_length)
{
  LWT_UNIX_INIT_JOB(job, bytes_write, 0);
  job->fd = Int_val(val_fd);
  job->buffer = (char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
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

  atime = copy_double((double) buf->st_atime + (NANOSEC(buf, a) / 1000000000.0));
  mtime = copy_double((double) buf->st_mtime + (NANOSEC(buf, m) / 1000000000.0));
  ctime = copy_double((double) buf->st_ctime + (NANOSEC(buf, c) / 1000000000.0));
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
  DIR* result;
  int error_code;
  char* path;
  char data[];
};

static void worker_opendir(struct job_opendir* job)
{
  job->result = opendir(job->path);
  job->error_code = errno;
}

static value result_opendir(struct job_opendir* job)
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
  DIR* dir;
};

static void worker_closedir(struct job_closedir* job)
{
  job->result = closedir(job->dir);
  job->error_code = errno;
}

static value result_closedir(struct job_closedir* job)
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

/* +-----------------------------------------------------------------+
   | JOB: rewinddir                                                  |
   +-----------------------------------------------------------------+ */

struct job_rewinddir {
  struct lwt_unix_job job;
  DIR* dir;
};

static void worker_rewinddir(struct job_rewinddir *job)
{
  rewinddir(job->dir);
}

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
  struct dirent *ptr;
  int result;
};

static void worker_readdir(struct job_readdir *job)
{
  job->entry = lwt_unix_malloc(offsetof(struct dirent, d_name) + fpathconf(dirfd(job->dir), _PC_NAME_MAX) + 1);
  job->result = readdir_r(job->dir, job->entry, &job->ptr);
}

static value result_readdir(struct job_readdir *job)
{
  int result = job->result;
  if (result) {
    free(job->entry);
    lwt_unix_free_job(&job->job);
    unix_error(result, "readdir", Nothing);
  } else if (job->ptr == NULL) {
    free(job->entry);
    lwt_unix_free_job(&job->job);
    caml_raise_end_of_file();
  } else {
    value name = caml_copy_string(job->entry->d_name);
    free(job->entry);
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
  long count;
  int error_code;
  struct dirent *entries[];
};

static void worker_readdir_n(struct job_readdir_n *job)
{
  size_t size = offsetof(struct dirent, d_name) + fpathconf(dirfd(job->dir), _PC_NAME_MAX) + 1;
  long i;
  for(i = 0; i < job->count; i++) {
    struct dirent *ptr;
    struct dirent *entry = (struct dirent *)lwt_unix_malloc(size);

    int result = readdir_r(job->dir, entry, &ptr);

    /* An error happened. */
    if (result != 0) {
      /* Free already read entries. */
      free(entry);
      long j;
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

static value result_readdir_n(struct job_readdir_n *job)
{
  CAMLparam0();
  CAMLlocal1(result);
  int error_code = job->error_code;
  if (error_code) {
    lwt_unix_free_job(&job->job);
    unix_error(error_code, "readdir", Nothing);
  } else {
    result = caml_alloc(job->count, 0);
    long i;
    for(i = 0; i < job->count; i++) {
      Store_field(result, i, caml_copy_string(job->entries[i]->d_name));
      free(job->entries[i]);
    }
    CAMLreturn(result);
  }
}

CAMLprim value lwt_unix_readdir_n_job(value val_dir, value val_count)
{
  long count = Long_val(val_count);
  LWT_UNIX_INIT_JOB(job, readdir_n, sizeof(struct dirent*) * count);
  job->dir = DIR_Val(val_dir);
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
    } if (link_length < buffer_size) {
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

static int lock_command_table[] = {
  F_ULOCK, F_LOCK, F_TLOCK, F_TEST, F_LOCK, F_TLOCK
};

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

CAMLprim value lwt_unix_lockf_job(value val_fd, value val_command, value val_length)
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

  Begin_roots5 (name, passwd, gecos, dir, shell);
    name = copy_string(entry->pw_name);
    passwd = copy_string(entry->pw_passwd);
#if !defined(__BEOS__)
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

#define JOB_GET_ENTRY(INIT, FUNC, CONF, TYPE, ARG, ARG_DECL, FAIL_ARG)  \
  struct job_##FUNC {                                                   \
    struct lwt_unix_job job;                                            \
    struct TYPE entry;                                                  \
    struct TYPE *ptr;                                                   \
    char *buffer;                                                       \
    int result;                                                         \
    ARG_DECL;                                                           \
  };                                                                    \
                                                                        \
  static void worker_##FUNC(struct job_##FUNC *job)                     \
  {                                                                     \
    size_t buffer_size = sysconf(_SC_##CONF##_R_SIZE_MAX);              \
    if (buffer_size == (size_t) -1) buffer_size = 16384;                \
    job->buffer = (char*)lwt_unix_malloc(buffer_size);                  \
    job->result = FUNC##_r(job->ARG, &job->entry, job->buffer, buffer_size, &job->ptr); \
  }                                                                     \
                                                                        \
  static value result_##FUNC(struct job_##FUNC *job)                    \
  {                                                                     \
    int result = job->result;                                           \
    if (result) {                                                       \
      value arg = FAIL_ARG;                                             \
      free(job->buffer);                                                \
      lwt_unix_free_job(&job->job);                                     \
      unix_error(result, #FUNC, arg);                                   \
    } else if (job->ptr == NULL) {                                      \
      free(job->buffer);                                                \
      lwt_unix_free_job(&job->job);                                     \
      caml_raise_not_found();                                           \
    } else {                                                            \
      value entry = alloc_##TYPE##_entry(&job->entry);                  \
      free(job->buffer);                                                \
      lwt_unix_free_job(&job->job);                                     \
      return entry;                                                     \
    }                                                                   \
  }                                                                     \
                                                                        \
  CAMLprim value lwt_unix_##FUNC##_job(value ARG)                       \
  {                                                                     \
    INIT;                                                               \
    return lwt_unix_alloc_job(&job->job);                               \
  }

JOB_GET_ENTRY(LWT_UNIX_INIT_JOB_STRING(job, getpwnam, 0, name), getpwnam, GETPW, passwd, name, char *name; char data[], caml_copy_string(job->name))
JOB_GET_ENTRY(LWT_UNIX_INIT_JOB_STRING(job, getgrnam, 0, name), getgrnam, GETGR, group, name, char *name; char data[], caml_copy_string(job->name))
JOB_GET_ENTRY(LWT_UNIX_INIT_JOB(job, getpwuid, 0); job->uid = Int_val(uid), getpwuid, GETPW, passwd, uid, int uid, Nothing)
JOB_GET_ENTRY(LWT_UNIX_INIT_JOB(job, getgrgid, 0); job->gid = Int_val(gid), getgrgid, GETGR, group, gid, int gid, Nothing)

#else

LWT_NOT_AVAILABLE1(unix_getpwnam_job)
LWT_NOT_AVAILABLE1(unix_getgrnam_job)
LWT_NOT_AVAILABLE1(unix_getpwuid_job)
LWT_NOT_AVAILABLE1(unix_getgrgid_job)

#endif

/* Helper functions for not re-entrant functions */

/* keep test in sync with discover.ml */
#if !defined(HAS_GETHOSTBYADDR_R) || (HAS_GETHOSTBYADDR_R != 7 && HAS_GETHOSTBYADDR_R != 8)
#define NON_R_GETHOSTBYADDR 1
#endif

/* keep test in sync with discover.ml */
#if !defined(HAS_GETHOSTBYNAME_R) || (HAS_GETHOSTBYNAME_R != 5 && HAS_GETHOSTBYNAME_R != 6)
#define NON_R_GETHOSTBYNAME 1
#endif

#if defined(NON_R_GETHOSTBYADDR) || defined(NON_R_GETHOSTBYNAME)
static char **
c_copy_addr_array(char ** src, int addr_len)
{
  if ( src == NULL ){
    return NULL;
  }
  char ** p = src;
  size_t i = 0 ;
  while ( *p ){
    i++;
    p++;
  }
  const size_t ar_len = i;
  p = malloc((ar_len+1) * sizeof(char*));
  if ( p == NULL ){
    return NULL;
  }
  for ( i = 0 ; i < ar_len ; ++i ){
    p[i] = malloc(addr_len);
    if ( p[i] == NULL ){
      size_t j;
      for ( j = 0 ; j < i ; j++ ){
        free(p[j]);
      }
      free(p);
      return NULL;
    }
    memcpy(p[i],src[i],addr_len);
  }
  p[ar_len] = NULL;
  return p;
}
#endif
#if !defined(HAVE_NETDB_REENTRANT) || defined(NON_R_GETHOSTBYADDR) || defined(NON_R_GETHOSTBYNAME)
static char **
c_copy_string_array(char **src)
{
  char ** p = src;
  size_t i = 0 ;
  size_t len ;
  if ( src == NULL ){
    return NULL;
  }
  while ( *p ){
    i++;
    p++;
  }
  len = i;
  p = malloc((len+1) * sizeof(char *));
  if ( p == NULL ){
    return NULL;
  }
  for ( i = 0 ; i < len ; ++i ){
    p[i] = strdup(src[i]);
    if ( p[i] == NULL ){
      size_t j;
      for ( j = 0 ; j < i ; j++ ){
        free(p[j]);
      }
      free(p);
      return NULL;
    }
  }
  p[len] = NULL;
  return p;
}

static void c_free_string_array(char ** src)
{
  if ( src ){
    char ** p = src;
    while (*p){
      free(*p);
      ++p;
    }
    free(src);
  }
}

static inline char * s_strdup (const char *s){
  return (strdup( s == NULL ? "" : s ));
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

#if defined(NON_R_GETHOSTBYADDR) || defined(NON_R_GETHOSTBYNAME)
static struct hostent *
hostent_dup(struct hostent *orig)
{
  if ( orig == NULL ){
    return NULL;
  }
  struct hostent *h = malloc(sizeof *h);
  if ( h == NULL ){
    return NULL;
  }
  h->h_name = s_strdup(orig->h_name);
  if ( !h->h_name ){
    goto nomem1;
  }
  if ( !orig->h_aliases ){
    h->h_aliases = NULL;
  }
  else {
    h->h_aliases = c_copy_string_array(orig->h_aliases);
    if ( !h->h_aliases){
      goto nomem2;
    }
  }
  if ( !orig->h_addr_list ){
    h->h_addr_list = NULL;
  }
  else {
    h->h_addr_list = c_copy_addr_array(orig->h_addr_list,orig->h_length);
    if ( !h->h_addr_list ){
      goto nomem3;
    }
  }
  h->h_addrtype = orig->h_addrtype;
  h->h_length = orig->h_length;
  return h;
nomem3:
  c_free_string_array(h->h_aliases);
nomem2:
  free(h->h_name);
nomem1:
  free(h);
  return NULL;
}

static void
hostent_free(struct hostent *h)
{
  if ( h ){
    c_free_string_array(h->h_addr_list);
    c_free_string_array(h->h_aliases);
    free(h->h_name);
    free(h);
  }
}
#endif

static void worker_gethostbyname(struct job_gethostbyname *job)
{
#if HAS_GETHOSTBYNAME_R == 5
  int h_errno;
  job->ptr = gethostbyname_r(job->name, &job->entry, job->buffer, NETDB_BUFFER_SIZE, &h_errno);
#elif HAS_GETHOSTBYNAME_R == 6
  int h_errno;
  if (gethostbyname_r(job->name, &job->entry, job->buffer, NETDB_BUFFER_SIZE, &(job->ptr), &h_errno) != 0)
    job->ptr = NULL;
#else
  job->ptr = gethostbyname(job->name);
  if (job->ptr) {
    job->ptr= hostent_dup(job->ptr);
    if ( job->ptr ){
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
  job->ptr = gethostbyaddr_r(&job->addr, 4, AF_INET, &job->entry, job->buffer, NETDB_BUFFER_SIZE, &h_errno);
#elif HAS_GETHOSTBYADDR_R == 8
  int h_errno;
  if (gethostbyaddr_r(&job->addr, 4, AF_INET, &job->entry, job->buffer, NETDB_BUFFER_SIZE, &job->ptr, &h_errno) != 0)
    job->ptr = NULL;
#else
  job->ptr = gethostbyaddr(&job->addr, 4, AF_INET);
  if (job->ptr) {
    job->ptr = hostent_dup(job->ptr);
    if ( job->ptr ){
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

static value alloc_servent(struct servent *entry)
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

#if defined(HAVE_NETDB_REENTRANT)

#define JOB_GET_ENTRY2(INIT, FUNC, TYPE, ARGS_VAL, ARGS_DECL, ARGS_CALL) \
  struct job_##FUNC {                                                   \
    struct lwt_unix_job job;                                            \
    struct TYPE entry;                                                  \
    struct TYPE *ptr;                                                   \
    char *buffer;                                                       \
    ARGS_DECL;                                                          \
  };                                                                    \
                                                                        \
  static void worker_##FUNC(struct job_##FUNC *job)                     \
  {                                                                     \
    size_t size = 1024;                                                 \
    for (;;) {                                                          \
      job->buffer = (char*)lwt_unix_malloc(size);                       \
                                                                        \
      int result = FUNC##_r(ARGS_CALL, &job->entry, job->buffer, size, &job->ptr); \
                                                                        \
      switch (result) {                                                 \
      case 0:                                                           \
        return;                                                         \
      case ERANGE:                                                      \
        free(job->buffer);                                              \
        size += 1024;                                                   \
        break;                                                          \
      case ENOENT:                                                      \
      default:                                                          \
        job->ptr = NULL;                                                \
        return;                                                         \
      }                                                                 \
    }                                                                   \
  }                                                                     \
                                                                        \
  static value result_##FUNC(struct job_##FUNC *job)                    \
  {                                                                     \
    if (job->ptr == NULL) {                                             \
      free(job->buffer);                                                \
      lwt_unix_free_job(&job->job);                                     \
      caml_raise_not_found();                                           \
    } else {                                                            \
      value res = alloc_##TYPE(&job->entry);                            \
      free(job->buffer);                                                \
      lwt_unix_free_job(&job->job);                                     \
      return res;                                                       \
    }                                                                   \
  }                                                                     \
                                                                        \
  CAMLprim value lwt_unix_##FUNC##_job(ARGS_VAL)                        \
  {                                                                     \
    INIT;                                                               \
    return lwt_unix_alloc_job(&(job->job));                             \
  }

#else /* defined(HAVE_NETDB_REENTRANT) */

static struct servent * servent_dup(const struct servent * serv)
{
  struct servent * s;
  if (!serv){
    return NULL;
  }
  s = malloc(sizeof *s);
  if ( s == NULL ){
    goto nomem1;
  }
  s->s_name = s_strdup(serv->s_name);
  if ( s->s_name == NULL ){
    goto nomem2;
  }
  s->s_proto = s_strdup(serv->s_proto);
  if ( s->s_proto == NULL ){
    goto nomem3;
  }
  s->s_aliases = c_copy_string_array(serv->s_aliases);
  if ( s->s_aliases == NULL && serv->s_aliases != NULL ){
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

static void servent_free(struct servent * s)
{
  if ( ! s ){
    return;
  }
  free(s->s_proto);
  free(s->s_name);
  c_free_string_array(s->s_aliases);
  free(s);
}

static struct protoent * protoent_dup(const struct protoent * proto)
{
  if (!proto){
    return NULL;
  }
  struct protoent * p = malloc(sizeof *p);
  if ( p == NULL ){
    return NULL;
  }
  p->p_name = s_strdup(proto->p_name);
  if ( p->p_name == NULL ){
    goto nomem1;
  }
  p->p_aliases = c_copy_string_array( proto->p_aliases );
  if ( p->p_aliases == NULL && proto->p_aliases != NULL ){
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

static void protoent_free(struct protoent * p)
{
  if ( p ){
    free(p->p_name);
    c_free_string_array(p->p_aliases);
    free(p);
  }
}

#define JOB_GET_ENTRY2(INIT, FUNC, TYPE, ARGS_VAL, ARGS_DECL, ARGS_CALL) \
  struct job_##FUNC {                                                   \
    struct lwt_unix_job job;                                            \
    struct TYPE *entry;                                                 \
    ARGS_DECL;                                                          \
  };                                                                    \
                                                                        \
  static void worker_##FUNC(struct job_##FUNC *job)                     \
  {                                                                     \
    job->entry = FUNC(ARGS_CALL);                                       \
    if ( job->entry ){                                                  \
      job->entry = TYPE ## _dup ( job->entry );                         \
      if (! job->entry ){                                               \
      }                                                                 \
    }                                                                   \
  }                                                                     \
                                                                        \
  static value result_##FUNC(struct job_##FUNC *job)                    \
  {                                                                     \
    if (job->entry == NULL) {                                           \
      lwt_unix_free_job(&job->job);                                     \
      caml_raise_not_found();                                           \
    } else {                                                            \
      value res = alloc_##TYPE(job->entry);                             \
      TYPE ## _free ( job->entry );                                     \
      lwt_unix_free_job(&job->job);                                     \
      return res;                                                       \
    }                                                                   \
  }                                                                     \
                                                                        \
  CAMLprim value lwt_unix_##FUNC##_job(ARGS_VAL)                        \
  {                                                                     \
    INIT;                                                               \
    return lwt_unix_alloc_job(&(job->job));                             \
  }

#endif /* defined(HAVE_NETDB_REENTRANT) */


JOB_GET_ENTRY2(LWT_UNIX_INIT_JOB_STRING(job, getprotobyname, 0, name),
               getprotobyname,
               protoent,
               value name,
               char *name;
               char data[],
               job->name)
JOB_GET_ENTRY2(LWT_UNIX_INIT_JOB(job, getprotobynumber, 0);
               job->num = Int_val(num),
               getprotobynumber,
               protoent,
               value num,
               int num,
               job->num)
JOB_GET_ENTRY2(LWT_UNIX_INIT_JOB_STRING2(job, getservbyname, 0, name, proto),
               getservbyname,
               servent,
               ARGS(value name, value proto),
               char *name;
               char *proto;
               char data[],
               ARGS(job->name, job->proto))
JOB_GET_ENTRY2(LWT_UNIX_INIT_JOB_STRING(job, getservbyport, 0, proto);
               job->port = htons(Int_val(port)),
               getservbyport,
               servent,
               ARGS(value port, value proto),
               int port;
               char *proto;
               char data[],
               ARGS(job->port, job->proto))

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
  job->result =
    getaddrinfo(job->node[0]?job->node:NULL, job->service[0]?job->service:NULL,
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
  if (job->info != NULL)
    freeaddrinfo(job->info);
  lwt_unix_free_job(&job->job);
  CAMLreturn(vres);
}

CAMLprim value lwt_unix_getaddrinfo_job(value node, value service, value hints)
{
  LWT_UNIX_INIT_JOB_STRING2(job, getaddrinfo, 0, node, service);
  job->info = NULL;
  memset(&job->hints, 0, sizeof(struct addrinfo));
  job->hints.ai_family = PF_UNSPEC;
  for (/*nothing*/; Is_block(hints); hints = Field(hints, 1)) {
    value v = Field(hints, 0);
    if (Is_block(v))
      switch (Tag_val(v)) {
      case 0: /* AI_FAMILY of socket_domain */
        job->hints.ai_family = socket_domain_table[Int_val(Field(v, 0))];
        break;
      case 1: /* AI_SOCKTYPE of socket_type */
        job->hints.ai_socktype = socket_type_table[Int_val(Field(v, 0))];
        break;
      case 2: /* AI_PROTOCOL of int */
        job->hints.ai_protocol = Int_val(Field(v, 0));
        break;
      }
    else
      switch (Int_val(v)) {
      case 0: /* AI_NUMERICHOST */
        job->hints.ai_flags |= AI_NUMERICHOST; break;
      case 1: /* AI_CANONNAME */
        job->hints.ai_flags |= AI_CANONNAME; break;
      case 2: /* AI_PASSIVE */
        job->hints.ai_flags |= AI_PASSIVE; break;
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

static int getnameinfo_flag_table[] = {
  NI_NOFQDN, NI_NUMERICHOST, NI_NAMEREQD, NI_NUMERICSERV, NI_DGRAM
};

static void worker_getnameinfo(struct job_getnameinfo *job)
{
  job->result = getnameinfo((const struct sockaddr *)&job->addr.s_gen, job->addr_len,
                            job->host, sizeof(job->host), job->serv, sizeof(job->serv),
                            job->opts);
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
    CAMLreturn(vres);
  }
}

CAMLprim value lwt_unix_getnameinfo_job(value sockaddr, value opts)
{
  LWT_UNIX_INIT_JOB(job, getnameinfo, 0);
  get_sockaddr(sockaddr, &job->addr, &job->addr_len);
  job->opts = convert_flag_list(opts, getnameinfo_flag_table);
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
  Bool, Iflags, IGNBRK,
  Bool, Iflags, BRKINT,
  Bool, Iflags, IGNPAR,
  Bool, Iflags, PARMRK,
  Bool, Iflags, INPCK,
  Bool, Iflags, ISTRIP,
  Bool, Iflags, INLCR,
  Bool, Iflags, IGNCR,
  Bool, Iflags, ICRNL,
  Bool, Iflags, IXON,
  Bool, Iflags, IXOFF,
  /* Output modes */
  Bool, Oflags, OPOST,
  /* Control modes */
  Speed, Output,
  Speed, Input,
  Enum, Cflags, 5, 4, CSIZE, CS5, CS6, CS7, CS8,
  Enum, Cflags, 1, 2, CSTOPB, 0, CSTOPB,
  Bool, Cflags, CREAD,
  Bool, Cflags, PARENB,
  Bool, Cflags, PARODD,
  Bool, Cflags, HUPCL,
  Bool, Cflags, CLOCAL,
  /* Local modes */
  Bool, Lflags, ISIG,
  Bool, Lflags, ICANON,
  Bool, Lflags, NOFLSH,
  Bool, Lflags, ECHO,
  Bool, Lflags, ECHOE,
  Bool, Lflags, ECHOK,
  Bool, Lflags, ECHONL,
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

static tcflag_t* choose_field(struct termios *terminal_status, long field)
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

static void encode_terminal_status(struct termios* terminal_status, value *dst)
{
  long * pc;
  int i;

  for(pc = terminal_io_descr; *pc != End; dst++) {
    switch(*pc++) {
    case Bool:
      { tcflag_t * src = choose_field(terminal_status, *pc++);
        tcflag_t msk = *pc++;
        *dst = Val_bool(*src & msk);
        break; }
    case Enum:
      { tcflag_t * src = choose_field(terminal_status, *pc++);
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
        break; }
    case Speed:
      { int which = *pc++;
        speed_t speed = 0;
        *dst = Val_int(9600);   /* in case no speed in speedtable matches */
        switch (which) {
        case Output:
          speed = cfgetospeed(terminal_status); break;
        case Input:
          speed = cfgetispeed(terminal_status); break;
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
        *dst = Val_int(terminal_status->c_cc[which]);
        break; }
    }
  }
}

static void decode_terminal_status(struct termios* terminal_status, value* src)
{
  long * pc;
  int i;

  for (pc = terminal_io_descr; *pc != End; src++) {
    switch(*pc++) {
    case Bool:
      { tcflag_t * dst = choose_field(terminal_status, *pc++);
        tcflag_t msk = *pc++;
        if (Bool_val(*src))
          *dst |= msk;
        else
          *dst &= ~msk;
        break; }
    case Enum:
      { tcflag_t * dst = choose_field(terminal_status, *pc++);
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
        break; }
    case Speed:
      { int which = *pc++;
        int baud = Int_val(*src);
        int res = 0;
        for (i = 0; i < NSPEEDS; i++) {
          if (baud == speedtable[i].baud) {
            switch (which) {
            case Output:
              res = cfsetospeed(terminal_status, speedtable[i].speed); break;
            case Input:
              res = cfsetispeed(terminal_status, speedtable[i].speed); break;
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
        terminal_status->c_cc[which] = Int_val(*src);
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

static int when_flag_table[] = {
  TCSANOW, TCSADRAIN, TCSAFLUSH
};

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
