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

#include <caml/version.h>
#include <caml/unixsupport.h>
#include <sys/uio.h>
#include <sys/un.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <poll.h>
#include <unistd.h>

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
    uerror("writable", Nothing);
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

#ifdef HAVE_BSD_MINCORE
#   define MINCORE_VECTOR_TYPE char
#else
#   define MINCORE_VECTOR_TYPE unsigned char
#endif

CAMLprim value lwt_unix_mincore(value val_buffer, value val_offset, value val_length, value val_states)
{
  long len = Wosize_val(val_states);
  MINCORE_VECTOR_TYPE vec[len];
  mincore((char*)Caml_ba_data_val(val_buffer) + Long_val(val_offset), Long_val(val_length), vec);
  long i;
  for (i = 0; i < len; i++)
    Field(val_states, i) = Val_bool(vec[i] & 1);
  return Val_unit;
}

#undef MINCORE_VECTOR_TYPE

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



/* readv, writev */

/* For blocking readv calls, arrays of this struct associate temporary buffers
   which are passed to the readv system call with the OCaml bytes buffers into
   which the data must ultimately be copied. */
struct readv_copy_to {
    /* Length of the temporary buffer. */
    size_t length;
    /* Offset into the OCaml buffer to which the temporary buffer must be
       copied. */
    size_t offset;
    value caml_buffer;
    char *temporary_buffer;
};

/* Tags for each of the constructors of type Lwt_unix.IO_vectors._buffer. The
   order must correspond to that in lwt_unix.ml. */
enum {IO_vectors_bytes, IO_vectors_bigarray};

/* Given an uninitialized array of iovec structures `iovecs`, and an OCaml value
   `io_vectors` of type Lwt_unix.IO_vectors._io_vector list, writes pointers to
   the first `count` buffer slices in `io_vectors` to `iovecs`. Each buffer
   slice may be a bytes buffer or a Bigarray buffer.

   In case `buffer_copies` is not NULL, a fresh buffer is allocated on the heap
   for each bytes buffer, and the contents of the bytes buffer are copied there.
   Pointers to these copies are written to `iovecs`, instead of pointers to the
   original buffers. The pointers are also stored as an array at
   `buffer_copies`, so that they can be freed later. This mechanism is used when
   `iovecs` will be passed to a blocking writev call, which is run by Lwt in a
   worker thread. In that case, the original, uncopied bytes buffers may be
   moved by the garbage collector before the I/O call runs, or while it is
   running.

   Similarly, in case `read_buffers` is not NULL, `flatten_io_vectors` allocates
   a temporary buffer for each OCaml bytes buffer. Pointers to the buffers are
   stored in `read_buffers`, together with GC roots for the corresponding OCaml
   buffers. */
static void flatten_io_vectors(
    struct iovec *iovecs, value io_vectors, size_t count, char **buffer_copies,
    struct readv_copy_to *read_buffers)
{
    CAMLparam1(io_vectors);
    CAMLlocal3(node, io_vector, buffer);

    size_t index;
    size_t copy_index = 0;

    for (node = io_vectors, index = 0; index < count;
         node = Field(node, 1), ++index) {

        io_vector = Field(node, 0);

        intnat offset = Long_val(Field(io_vector, 1));
        intnat length = Long_val(Field(io_vector, 2));

        iovecs[index].iov_len = length;

        buffer = Field(Field(io_vector, 0), 0);
        if (Tag_val(Field(io_vector, 0)) == IO_vectors_bytes) {
            if (buffer_copies != NULL) {
                buffer_copies[copy_index] = lwt_unix_malloc(length);
                memcpy(
                    buffer_copies[copy_index],
                    &Byte(String_val(buffer), offset), length);

                iovecs[index].iov_base = buffer_copies[copy_index];
                ++copy_index;
            }
            else if (read_buffers != NULL) {
                read_buffers[copy_index].temporary_buffer =
                    lwt_unix_malloc(length);
                read_buffers[copy_index].length = length;
                read_buffers[copy_index].offset = offset;
                read_buffers[copy_index].caml_buffer = buffer;
                caml_register_generational_global_root(
                    &read_buffers[copy_index].caml_buffer);

                iovecs[index].iov_base =
                    read_buffers[copy_index].temporary_buffer;
                ++copy_index;
            }
            else
                iovecs[index].iov_base = &Byte(String_val(buffer), offset);
        }
        else
            iovecs[index].iov_base = &((char*)Caml_ba_data_val(buffer))[offset];
    }

    if (buffer_copies != NULL)
        buffer_copies[copy_index] = NULL;
    if (read_buffers != NULL)
        read_buffers[copy_index].temporary_buffer = NULL;

    CAMLreturn0;
}

CAMLprim value lwt_unix_iov_max(value unit)
{
    return Val_int(IOV_MAX);
}

/* writev */

/* writev primitive for non-blocking file descriptors. */
CAMLprim value lwt_unix_writev(value fd, value io_vectors, value val_count)
{
    CAMLparam3(fd, io_vectors, val_count);

    size_t count = Long_val(val_count);

    /* Assemble iovec structures on the stack. No data is copied. */
    struct iovec iovecs[count];
    flatten_io_vectors(iovecs, io_vectors, count, NULL, NULL);

    ssize_t result = writev(Int_val(fd), iovecs, count);

    if (result == -1)
        uerror("writev", Nothing);

    CAMLreturn(Val_long(result));
}

/* Job and writev primitives for blocking file descriptors. */
struct job_writev {
    struct lwt_unix_job job;
    int fd;
    int error_code;
    ssize_t result;
    size_t count;
    /* Heap-allocated iovec structures. */
    struct iovec *iovecs;
    /* Heap-allocated array of pointers to heap-allocated copies of bytes buffer
       slices. This array is NULL-terminated. */
    char **buffer_copies;
};

static void worker_writev(struct job_writev *job)
{
    job->result = writev(job->fd, job->iovecs, job->count);
    job->error_code = errno;
}

static value result_writev(struct job_writev *job)
{
    char **buffer_copy;
    for (buffer_copy = job->buffer_copies; *buffer_copy != NULL;
         ++buffer_copy) {

        free(*buffer_copy);
    }
    free(job->buffer_copies);
    free(job->iovecs);

    ssize_t result = job->result;
    LWT_UNIX_CHECK_JOB(job, result < 0, "writev");
    lwt_unix_free_job(&job->job);
    return Val_long(result);
}

CAMLprim value lwt_unix_writev_job(value fd, value io_vectors, value val_count)
{
    CAMLparam3(fd, io_vectors, val_count);

    LWT_UNIX_INIT_JOB(job, writev, 0);
    job->fd = Int_val(fd);
    job->count = Long_val(val_count);

    /* Assemble iovec structures on the heap and copy bytes buffer slices. */
    job->iovecs = lwt_unix_malloc(job->count * sizeof(struct iovec));
    /* The extra (+ 1) pointer is for the NULL terminator, in case all buffer
       slices are in bytes buffers. */
    job->buffer_copies = lwt_unix_malloc((job->count + 1) * sizeof(char*));
    flatten_io_vectors(
        job->iovecs, io_vectors, job->count, job->buffer_copies, NULL);

    CAMLreturn(lwt_unix_alloc_job(&job->job));
}

/* readv */

/* readv primitive for non-blocking file descriptors. */
CAMLprim value lwt_unix_readv(value fd, value io_vectors, value val_count)
{
    CAMLparam3(fd, io_vectors, val_count);

    size_t count = Long_val(val_count);

    /* Assemble iovec structures on the stack. */
    struct iovec iovecs[count];
    flatten_io_vectors(iovecs, io_vectors, count, NULL, NULL);

    /* Data is read directly into the buffers. There is no need to copy
       afterwards. */
    ssize_t result = readv(Int_val(fd), iovecs, count);

    if (result == -1)
      uerror("readv", Nothing);

    CAMLreturn(Val_long(result));
}

/* Job and readv primitives for blocking file descriptors. */
struct job_readv {
    struct lwt_unix_job job;
    int fd;
    int error_code;
    ssize_t result;
    size_t count;
    /* Heap-allocated iovec structures. */
    struct iovec *iovecs;
    /* Data to be read into bytes buffers is first read into temporary buffers
       on the C heap. This is an array of descriptors for copying that data into
       the actual bytes buffers. The array is terminated by a descriptor whose
       temporary_buffer member is NULL. */
    struct readv_copy_to buffers[];
};

static void worker_readv(struct job_readv *job)
{
    job->result = readv(job->fd, job->iovecs, job->count);
    job->error_code = errno;
}

static value result_readv(struct job_readv *job)
{
    struct readv_copy_to *read_buffer;

    /* If the read is successful, copy data to the OCaml buffers. */
    if (job->result != -1) {
        for (read_buffer = job->buffers; read_buffer->temporary_buffer != NULL;
             ++read_buffer) {

            memcpy(
                &Byte(String_val(read_buffer->caml_buffer),
                      read_buffer->offset),
                read_buffer->temporary_buffer,
                read_buffer->length);
        }
    }

    /* Free heap-allocated structures and buffers. */
    for (read_buffer = job->buffers; read_buffer->temporary_buffer != NULL;
         ++read_buffer) {

        free(read_buffer->temporary_buffer);
        caml_remove_generational_global_root(&read_buffer->caml_buffer);
    }
    free(job->iovecs);

    /* Decide on the actual result. */
    ssize_t result = job->result;
    LWT_UNIX_CHECK_JOB(job, result < 0, "readv");
    lwt_unix_free_job(&job->job);
    return Val_long(result);
}

CAMLprim value lwt_unix_readv_job(value fd, value io_vectors, value val_count)
{
    CAMLparam3(fd, io_vectors, val_count);

    size_t count = Long_val(val_count);

    /* The extra struct readv_copy_to (+ 1) is for the final terminator, in case
       all buffer slices are in bytes buffers. */
    LWT_UNIX_INIT_JOB(job, readv, sizeof(struct readv_copy_to) * (count + 1));
    job->fd = Int_val(fd);
    job->count = count;

    /* Assemble iovec structures on the heap. */
    job->iovecs = lwt_unix_malloc(sizeof(struct iovec) * count);
    flatten_io_vectors(job->iovecs, io_vectors, count, NULL, job->buffers);

    CAMLreturn(lwt_unix_alloc_job(&job->job));
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
             caml_convert_flag_list(flags, msg_flag_table));
  if (ret == -1) uerror("recv", Nothing);
  return Val_int(ret);
}

value lwt_unix_bytes_recv(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = recv(Int_val(fd), (char*)Caml_ba_array_val(buf)->data + Long_val(ofs), Long_val(len),
             caml_convert_flag_list(flags, msg_flag_table));
  if (ret == -1) uerror("recv", Nothing);
  return Val_int(ret);
}

value lwt_unix_send(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = send(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len),
             caml_convert_flag_list(flags, msg_flag_table));
  if (ret == -1) uerror("send", Nothing);
  return Val_int(ret);
}

value lwt_unix_bytes_send(value fd, value buf, value ofs, value len, value flags)
{
  int ret;
  ret = send(Int_val(fd), (char*)Caml_ba_array_val(buf)->data + Long_val(ofs), Long_val(len),
             caml_convert_flag_list(flags, msg_flag_table));
  if (ret == -1) uerror("send", Nothing);
  return Val_int(ret);
}

/* +-----------------------------------------------------------------+
   | recvfrom/sendto                                                 |
   +-----------------------------------------------------------------+ */

extern int socket_domain_table[];
extern int socket_type_table[];

value lwt_unix_recvfrom(value fd, value buf, value ofs, value len, value flags)
{
  CAMLparam5(fd, buf, ofs, len, flags);
  CAMLlocal2(result, address);
  int ret;
  union sock_addr_union addr;
  socklen_t addr_len;
  addr_len = sizeof(addr);
  ret = recvfrom(Int_val(fd), &Byte(String_val(buf), Long_val(ofs)), Long_val(len),
                 caml_convert_flag_list(flags, msg_flag_table),
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
                 caml_convert_flag_list(flags, msg_flag_table),
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
               caml_convert_flag_list(flags, msg_flag_table),
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
               caml_convert_flag_list(flags, msg_flag_table),
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
        caml_invalid_argument("Not an Internet socket");
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
        caml_invalid_argument("lwt_unix_mcast_set_loop");
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
        caml_invalid_argument("lwt_unix_mcast_set_ttl");
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

        if (caml_string_length(group_addr) != 4 ||
            caml_string_length(if_addr) != 4) {

            caml_invalid_argument("lwt_unix_mcast_modify: Not an IPV4 address");
        }

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
        caml_invalid_argument("lwt_unix_mcast_modify_membership");
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
    st = caml_alloc_small(1, TAG_WEXITED);
    Field(st, 0) = Val_int(WEXITSTATUS(status));
  }
  else if (WIFSTOPPED(status)) {
    st = caml_alloc_small(1, TAG_WSTOPPED);
    Field(st, 0) = Val_int(caml_rev_convert_signal_number(WSTOPSIG(status)));
  }
  else {
    st = caml_alloc_small(1, TAG_WSIGNALED);
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

  times = caml_alloc_small(2 * Double_wosize, Double_array_tag);
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
  0, /* O_SHARE_DELETE, Windows-only */
  0, /* O_CLOEXEC, treated specially */
  0  /* O_KEEPEXEC, treated specially */
};

enum { CLOEXEC = 1, KEEPEXEC = 2 };

static int open_cloexec_table[15] = {
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0,
  0,
  CLOEXEC, KEEPEXEC
};

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

  atime =
    caml_copy_double((double) buf->st_atime + (NANOSEC(buf, a) / 1000000000.0));
  mtime =
    caml_copy_double((double) buf->st_mtime + (NANOSEC(buf, m) / 1000000000.0));
  ctime =
    caml_copy_double((double) buf->st_ctime + (NANOSEC(buf, c) / 1000000000.0));
  offset = use_64 ? caml_copy_int64(buf->st_size) : Val_int(buf->st_size);
  v = caml_alloc_small(12, 0);
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

    double  atime = Double_val(val_atime);
    double  mtime = Double_val(val_mtime);

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

/* struct dirent size */

/* Some kind of estimate of the true size of a dirent structure, including the
   space used for the name. This is controversial, and there is an ongoing
   discussion (see Internet) about deprecating readdir_r because of the need to
   guess the size in this way. */
static size_t dirent_size(DIR *dir)
{
    size_t size =
        offsetof(struct dirent, d_name) +
        fpathconf(dirfd(dir), _PC_NAME_MAX) + 1;

    if (size < sizeof(struct dirent))
        size = sizeof(struct dirent);

    return size;
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
  job->entry = lwt_unix_malloc(dirent_size(job->dir));
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
  struct dirent entries[];
};

static void worker_readdir_n(struct job_readdir_n *job)
{
  long i;
  for(i = 0; i < job->count; i++) {
    struct dirent *ptr;
    int result = readdir_r(job->dir, &job->entries[i], &ptr);

    /* An error happened. */
    if (result != 0) {
      job->error_code = result;
      return;
    }

    /* End of directory reached */
    if (ptr == NULL)
      break;
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
      Store_field(result, i, caml_copy_string(job->entries[i].d_name));
    }
    lwt_unix_free_job(&job->job);
    CAMLreturn(result);
  }
}

CAMLprim value lwt_unix_readdir_n_job(value val_dir, value val_count)
{
  long count = Long_val(val_count);
  DIR *dir = DIR_Val(val_dir);

  LWT_UNIX_INIT_JOB(job, readdir_n, dirent_size(dir) * count);
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

  Begin_roots3 (name, pass, mem);
    name = caml_copy_string(entry->gr_name);
    pass = caml_copy_string(entry->gr_passwd);
    mem = caml_copy_string_array((const char**)entry->gr_mem);
    res = caml_alloc_small(4, 0);
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
    name = caml_copy_string((char *)(entry->h_name));
    /* PR#4043: protect against buggy implementations of gethostbynamee()
       that return a NULL pointer in h_aliases */
    if (entry->h_aliases)
      aliases = caml_copy_string_array((const char**)entry->h_aliases);
    else
      aliases = Atom(0);
    if (entry->h_length == 16)
      addr_list =
        caml_alloc_array(alloc_one_addr6, (const char**)entry->h_addr_list);
    else
      addr_list =
        caml_alloc_array(alloc_one_addr, (const char**)entry->h_addr_list);
    res = caml_alloc_small(4, 0);
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
  free((char*)h->h_name);
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
    free((char*)h->h_name);
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
    name = caml_copy_string(entry->p_name);
    aliases = caml_copy_string_array((const char**)entry->p_aliases);
    res = caml_alloc_small(3, 0);
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
    name = caml_copy_string(entry->s_name);
    aliases = caml_copy_string_array((const char**)entry->s_aliases);
    proto = caml_copy_string(entry->s_proto);
    res = caml_alloc_small(4, 0);
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

static value cst_to_constr(int n, int *tbl, int size, int deflt)
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
  vcanonname = caml_copy_string(a->ai_canonname == NULL ? "" : a->ai_canonname);
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
    versa. However, the OCaml code lacks conditional compilation, and expects
    all the C symbols to be available on all platforms during linking – it just
    doesn't call the ones that aren't really defined.

    The `LWT_NOT_AVAILABLEx` macros are used to define dummy symbols. `x` is the
    number of arguments the stub takes. For example, the `getcwd` job (which
    takes 1 argument) is currently not defined on Windows. For this reason,
    `lwt_unix_windows.h` has `LWT_NOT_AVAILABLE1(unix_getcwd_job)`. The `lwt_`
    prefix is left off.

    A typical way to discover that you need to use this kind of macro is to
    submit a PR to the Lwt repo, and wait for the CI build bots to tell you. We
    have pretty thorough CI coverage of the systems Lwt supports.


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
    char* result;
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

    /* Create an OCaml string from the temporary buffer into which the
       `getcwd(3)` wrote the current directory. This copies the data.

       Throughout Lwt, blocking C calls run in worker threads can't write
       directly into OCaml strings, because the OCaml garbage collector might
       move the strings after the pointer has already been passed to the call,
       but while the call is still blocked. Bigarrays don't have this problem,
       so pointers into them are passed to blocking C calls, saving a copy.

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

       The first argument is the name of the variable to be craeted to store
       the pointer to the job `struct`, i.e.

         struct job_getcwd *job = ...

       The last argument is the number of bytes of storage to reserve in memory
       immediately following the `struct`. This is for fields such as
       `char data[]` at the end of the struct. It is typically zero. */
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
