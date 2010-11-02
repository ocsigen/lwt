/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_mmap_stubs
 * Copyright (C) 2010 Pierre Chambart
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

#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <assert.h>
#include <assert.h>
#include <sys/stat.h>

#include <caml/config.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/signals.h>

#include "lwt_unix.h"

#if defined(LWT_WINDOWS)
#  include <windows.h>
#else
#  include <sys/mman.h>
#endif

long page_size = -1;

#if defined(LWT_WINDOWS)

CAMLprim value lwt_mmap_init_pagesize()
{
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  page_size = si.dwPageSize;
  return Val_long(page_size);
}

CAMLprim value lwt_mmap_mincore(value v_bigarray, value v_offset, value v_len)
{
  return Val_int(1);
}

CAMLprim value lwt_mmap_madvise(value bigarrayv, value offsetv, value lenv, value advisev)
{
  return Val_unit;
}

#else

CAMLprim value lwt_mmap_init_pagesize()
{
  page_size = sysconf(_SC_PAGESIZE);
  return Val_long(page_size);
}

CAMLprim value
lwt_mmap_mincore ( value v_bigarray, value v_offset, value v_len )
{
  CAMLparam3 ( v_bigarray, v_offset, v_len );
  CAMLlocal1 ( v_vec );

  char err_str[32];
  char *data;
  unsigned char *vec;
  int len, offset, r;

  offset = Int_val (v_offset);
  len = Int_val (v_len);

  assert( offset <= Caml_ba_array_val(v_bigarray)->dim[0] );

  v_vec = caml_alloc_string (len);
  vec = (unsigned char *) String_val(v_vec);

  data = Caml_ba_data_val(v_bigarray) + offset;

  r = mincore (data, len*page_size, vec);

  if (r == -1) {
    snprintf(err_str,32,"%p 0x%x",data,len);
    uerror("mincore",caml_copy_string(err_str));
  }

  CAMLreturn (v_vec);
}

static int advise_table[] =
  { MADV_NORMAL,
    MADV_RANDOM,
    MADV_SEQUENTIAL,
    MADV_WILLNEED,
    MADV_DONTNEED };

static int advise_of_int (int caml_advise)
{
  return advise_table[caml_advise];
}

CAMLprim value
lwt_mmap_madvise (value bigarrayv, value offsetv, value lenv, value advisev)
{
  CAMLparam4 ( bigarrayv, offsetv, lenv, advisev);

  char err_str[32];
  char *data;
  int len, offset, bigarray_len, r, advise;

  offset = Int_val (offsetv);
  len = Int_val (lenv);
  advise = advise_of_int( Int_val (advisev) );

  bigarray_len = Bigarray_val(bigarrayv)->dim[0];

  data = Data_bigarray_val(bigarrayv) + offset;

  r = madvise(data, len, advise);

  if (r == -1) {
    snprintf(err_str,32,"%p 0x%x",data,len);
    uerror("madvise",caml_copy_string(err_str));
  }

  CAMLreturn (Val_unit);
}

#endif

CAMLprim value lwt_mmap_write( value v_fd, value v_bstr, value v_pos, value v_len )
{
  struct caml_ba_array *ba = Caml_ba_array_val(v_bstr);
  char *bstr = (char *) ba->data + Long_val(v_pos);
  size_t len = Long_val(v_len);
  ssize_t written;
  written = write(Int_val(v_fd), bstr, len);
  if (written == -1) uerror("lwt_mmap_write", Nothing);
  return Val_long(written);
}

typedef struct waiter_descr {
  int ident;
  char* data;
} waiter_descr_t;

static void *waiter_function(void *w)
{
  waiter_descr_t *waiter = (waiter_descr_t *)w;

  /* Read the first byte to force the kernel to fetch the page: */
  char dummy = *(waiter->data);
  /* Make the compiler happy: */
  dummy = 0;

  lwt_unix_send_notification(waiter->ident);

  free(waiter);
  return NULL;
}

CAMLprim value lwt_mmap_launch_waiter(value v_bigarray, value v_offset, value v_ident)
{
  waiter_descr_t *waiter = (waiter_descr_t *)malloc(sizeof(waiter_descr_t));

  waiter->ident = Int_val(v_ident);
  waiter->data = Caml_ba_data_val(v_bigarray) + Int_val(v_offset);

  lwt_unix_launch_thread(waiter_function, (void*)waiter);

  return Val_unit;
}

CAMLprim value lwt_mmap_memcpy( value v_bigarray, value v_ba_offset, value v_string, value v_s_offset,
				value v_len )
{
  void *src = Caml_ba_data_val(v_bigarray) + Int_val(v_ba_offset);
  void *dest = String_val(v_string) + Int_val(v_s_offset);
  memcpy( dest, src, Int_val( v_len ) );
  return ( Val_unit );
}

static void lwt_mmap_unmap_file(void * addr, uintnat len)
{
  uintnat page = getpagesize();
  uintnat delta = (uintnat) addr % page;
  munmap((void *)((uintnat)addr - delta), len + delta);
}

static void lwt_mmap_reset_ba(struct caml_ba_array * b)
{
  lwt_mmap_unmap_file(b->data, caml_ba_byte_size(b));
  b->flags = CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL;
  b->dim[0] = 0;
}

CAMLprim value lwt_mmap_munmap(value v)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);

  switch (b->flags & CAML_BA_MANAGED_MASK) {
  case CAML_BA_MAPPED_FILE:
    // the bigarray is only used inside Lwt_mmap: there is never sub arrays created
    // so we can suppose there is no proxy
    lwt_mmap_reset_ba(b);
    break;
  case CAML_BA_EXTERNAL: // that case shouldn't happen
    break;
  case CAML_BA_MANAGED: // that case shouldn't happen
    break;
  }

  return ( Val_unit );
}

CAMLprim value lwt_mmap_fstat(value v_fd)
{
  CAMLparam1(v_fd);
  CAMLlocal2(mtime, v);
  int ret;
  struct stat buf;

  ret = fstat(Int_val(v_fd), &buf);
  if (ret == -1) uerror("fstat", Nothing);
  v = alloc_small(3, 0);
  mtime = copy_double((double) buf.st_mtime);

  Field (v, 0) = Val_int (buf.st_dev);
  Field (v, 1) = Val_int (buf.st_ino);
  Field (v, 2) = mtime;

  CAMLreturn(v);
}
