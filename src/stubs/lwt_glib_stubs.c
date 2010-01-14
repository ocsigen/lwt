/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_glib_stubs
 * Copyright (C) 2009 Jérémie Dimino
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
#include <stdlib.h>
#include <string.h>
#include <poll.h>
#include <glib.h>
#include <stdio.h>

CAMLprim value lwt_glib_get_poll_bits(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(x);
  x = caml_alloc_tuple(3);
  Store_field(x, 0, Val_int(POLLIN));
  Store_field(x, 1, Val_int(POLLOUT));
  Store_field(x, 2, Val_int(POLLERR));
  CAMLreturn(x);
}

/* The polling function defined by glib: */
GPollFunc old_poll_func;

#define GPollFD_val(v) *(GPollFD**)Data_custom_val(v)

/* The ``real'' poll function. It is responsible for calling the
   glib-defined poll function with the list of file-descriptors
   monitored by lwt and glib. */
CAMLprim value lwt_glib_real_poll(value glib_ufds, value glib_nfds, value lwt_ufds, value lwt_nfds, value timeout)
{
  CAMLparam5(glib_ufds, glib_nfds, lwt_ufds, lwt_nfds, timeout);

  /* List of glib fds + lwt fds */
  guint nfds = Int_val(lwt_nfds) + Int_val(glib_nfds);
  GPollFD *ufds = (GPollFD*)malloc(sizeof(GPollFD) * nfds);
  if (ufds == NULL) caml_failwith("out of memory");

  /* Copy glib fds: */
  memcpy(ufds, GPollFD_val(glib_ufds), sizeof(GPollFD) * Int_val(glib_nfds));

  GPollFD *fp;
  value l;

  /* Copy lwt fds: */
  for(fp = ufds + Int_val(glib_nfds), l = lwt_ufds; Is_block(l); fp++, l = Field(l, 1)) {
    value poll_fd = Field(l, 0);
    fp->fd = Int_val(Field(poll_fd, 0));
    fp->events = Int_val(Field(poll_fd, 1));
    fp->revents = 0;
  }

  /* Call the old glib polling function: */
  enter_blocking_section();
  int result = old_poll_func(ufds, nfds, Int_val(timeout));
  leave_blocking_section();

  /* Copy back glib fds: */
  memcpy(GPollFD_val(glib_ufds), ufds, sizeof(GPollFD) * Int_val(glib_nfds));

  /* Copy back lwt fds: */
  for(fp = ufds + Int_val(glib_nfds), l = lwt_ufds; Is_block(l); fp++, l = Field(l, 1)) {
    value poll_fd = Field(l, 0);
    Store_field(poll_fd, 1, Val_int(fp->revents));
  }

  CAMLreturn(Val_int(result));
}

/* Just a C pointer */
static struct custom_operations pointer_ops = {
  "lwt.glib.pointer",
  custom_finalize_default,
  NULL,
  NULL,
  custom_serialize_default,
  custom_deserialize_default
};

/* Wrapper for replacing the poll function of glib. It calls the ocaml
   wrapper which adds all the file-descriptors that lwt want to
   monitor: */
gint lwt_glib_poll(GPollFD *ufds, guint nfds, gint timeout)
{
  value pointer = caml_alloc_custom(&pointer_ops, sizeof(GPollFD*), 0, 1);
  GPollFD_val(pointer) = ufds;
  return Int_val(caml_callback3(*caml_named_value("lwt-glib-select"), pointer, Val_int(nfds), Val_int(timeout)));
}

/* ``plug'' our polling function into the glib: */
void lwt_glib_setup()
{
  old_poll_func = g_main_context_get_poll_func(NULL);
  g_main_context_set_poll_func(NULL, lwt_glib_poll);
}

/* ``unplug'' our polling function from the glib: */
void lwt_glib_reset()
{
  g_main_context_set_poll_func(NULL, old_poll_func);
}
