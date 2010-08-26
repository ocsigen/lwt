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

#include <sys/mman.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <pthread.h>
#include <fcntl.h>

#include <caml/config.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>

CAMLprim value
lwt_mmap_init_pagesize ( value v_unit )
{
  long page_size = sysconf(_SC_PAGESIZE);
  return( Val_long ( page_size ) );
}

CAMLprim value
lwt_mmap_mincore ( value v_bigarray, value v_offset, value v_len )
{
  CAMLparam3 ( v_bigarray, v_offset, v_len );
  CAMLlocal1 ( v_vec );

  char err_str[32];
  char *data;
  unsigned char *vec;
  int len, offset, bigarray_len, r;
  long page_size = sysconf(_SC_PAGESIZE);

  offset = Int_val (v_offset);
  len = Int_val (v_len);
  
  bigarray_len = Caml_ba_array_val(v_bigarray)->dim[0];
  v_vec = caml_alloc_string ((len+page_size-1) / page_size);
  vec = (unsigned char *) String_val(v_vec);

  data = Caml_ba_data_val(v_bigarray) + offset;

  r = mincore (data, len, vec);

  if (r == -1) {
    snprintf(err_str,32,"%p 0x%x",data,len);
    uerror("mincore",caml_copy_string(err_str));
  }

  CAMLreturn (v_vec);
}

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

int lwt_mmap_pipe = -1;
pthread_mutex_t pipe_lock = PTHREAD_MUTEX_INITIALIZER;

CAMLprim value lwt_mmap_init_pipe( value v_unit )
{
  int pipefd[2];
  if ( pipe( pipefd ) == -1 ) uerror("lwt_mmap_write", Nothing);
  if ( fcntl(pipefd[1], F_SETFD, FD_CLOEXEC) == -1 ) uerror("lwt_mmap_write", Nothing);
  lwt_mmap_pipe = pipefd[1];
  return Val_int(pipefd[0]);
}

typedef struct waiter_descr {
  int32 ident;
  char* data;
} waiter_descr_t;

static void *waiter_function(void *w)
{
  waiter_descr_t *waiter = (waiter_descr_t *) w;
  char dummy = *( waiter->data );

  pthread_mutex_lock( &pipe_lock );
  write( lwt_mmap_pipe, (void*) &(waiter->ident) , 4 );
  pthread_mutex_unlock( &pipe_lock );

  free(waiter);
  return NULL;
}

CAMLprim value lwt_mmap_launch_waiter( value v_bigarray, value v_offset, value v_ident )
{
  waiter_descr_t *waiter = (waiter_descr_t *) malloc(sizeof(waiter_descr_t));
  pthread_t thread;
  int32 ident = Int32_val(v_ident);

  waiter->ident = ident;
  waiter->data = Caml_ba_data_val(v_bigarray) + Int_val(v_offset);

  pthread_create( &thread, NULL, waiter_function, (void*) waiter );

  return ( Val_unit );
}

CAMLprim value lwt_mmap_memcpy( value v_bigarray, value v_ba_offset, value v_string, value v_s_offset,
				value v_len )
{
  void *src = Caml_ba_data_val(v_bigarray) + Int_val(v_ba_offset);
  void *dest = String_val(v_string) + Int_val(v_s_offset);
  memcpy( dest, src, Int_val( v_len ) );
  return ( Val_unit );
}
