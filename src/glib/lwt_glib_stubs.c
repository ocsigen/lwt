/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_glib_stubs
 * Copyright (C) 2009-2011 Jérémie Dimino
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

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <glib.h>
#include <ev.h>

#include "../unix/lwt_unix.h"

GMainContext *gc;
GPollFD *gpollfds = NULL;
gint fds_count = 0;
gint n_fds;
gint max_priority;

/* +-----------------------------------------------------------------+
   | Get sources                                                     |
   +-----------------------------------------------------------------+ */

#if defined(LWT_ON_WINDOWS)

static value alloc_fd(HANDLE handle)
{
  value res = win_alloc_handle(handle);
  int opt;
  int optlen = sizeof(opt);
  if (getsockopt((SOCKET)handle, SOL_SOCKET, SO_TYPE, (char *)&opt, &optlen) == 0)
    Descr_kind_val(res) = KIND_SOCKET;
  return res;
}

#endif

CAMLprim value lwt_glib_get_sources()
{
  CAMLparam0();
  CAMLlocal4(fd, fds, src, result);

  gint timeout;

  g_main_context_dispatch(gc);
  g_main_context_prepare(gc, &max_priority);

  while (fds_count < (n_fds = g_main_context_query(gc, max_priority, &timeout, gpollfds, fds_count))) {
    free(gpollfds);
    fds_count = n_fds;
    gpollfds = malloc(fds_count * sizeof (GPollFD));
  }

  int i;
  fds = caml_alloc_tuple(n_fds);
  for (i = 0; i < n_fds; i++) {
    GPollFD *gpollfd = gpollfds + i;
    gpollfd->revents = 0;

#if defined(LWT_ON_WINDOWS)
    /* On windows, glib file descriptors are handles */
    fd = alloc_fd((HANDLE)gpollfd->fd);
#else
    fd = Val_int(gpollfd->fd);
#endif

    src = caml_alloc_tuple(3);
    Field(src, 0) = fd;
    Field(src, 1) = Val_bool(gpollfd->events & G_IO_IN);
    Field(src, 2) = Val_bool(gpollfd->events & G_IO_OUT);

    Field(fds, i) = src;
  }

  result = caml_alloc_tuple(2);
  Store_field(result, 0, fds);
  Store_field(result, 1, caml_copy_double(timeout * 1e-3));

  CAMLreturn(result);
}

/* +-----------------------------------------------------------------+
   | Marking                                                         |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_glib_mark_readable(value i)
{
  gpollfds[Int_val(i)].revents |= G_IO_IN;
  return Val_unit;
}

CAMLprim value lwt_glib_mark_writable(value i)
{
  gpollfds[Int_val(i)].revents |= G_IO_OUT;
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Check                                                           |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_glib_check()
{
  g_main_context_check(gc, max_priority, gpollfds, n_fds);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Initialization/stopping                                         |
   +-----------------------------------------------------------------+ */

value lwt_glib_init()
{
  gc = g_main_context_default();
  g_main_context_ref(gc);
  return Val_unit;
}

value lwt_glib_stop()
{
  g_main_context_unref(gc);
  return Val_unit;
}
