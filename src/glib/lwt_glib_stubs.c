/* Lightweight thread library for OCaml
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

#define CAML_NAME_SPACE

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <glib.h>

extern void *lwt_unix_malloc(size_t size);

GMainContext *gc;
GPollFD *gpollfds = NULL;
gint fds_count = 0;
gint n_fds;
gint max_priority;

/* +-----------------------------------------------------------------+
   | Polling                                                         |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_glib_poll(value val_fds, value val_count, value val_timeout)
{
  gint timeout, lwt_timeout;
  long count;
  int i;
  GPollFD *gpollfd;
  gint events, revents;

  CAMLparam3(val_fds, val_count, val_timeout);
  CAMLlocal5(node, src, node_result, src_result, tmp);

  count = Long_val(val_count);

  g_main_context_dispatch(gc);
  g_main_context_prepare(gc, &max_priority);

  while (fds_count < count + (n_fds = g_main_context_query(gc, max_priority, &timeout, gpollfds, fds_count))) {
    free(gpollfds);
    fds_count = n_fds + count;
    gpollfds = lwt_unix_malloc(fds_count * sizeof (GPollFD));
  }

  /* Clear all revents fields. */
  for (i = 0; i < n_fds + count; i++) gpollfds[i].revents = 0;

  /* Add all Lwt fds. */
  for (i = n_fds, node = val_fds; i < n_fds + count; i++, node = Field(node, 1)) {
    src = Field(node, 0);
    gpollfd = gpollfds + i;
#if defined(LWT_ON_WINDOWS)
    gpollfd->fd = Handle_val(Field(src, 0));
#else
    gpollfd->fd = Int_val(Field(src, 0));
#endif
    events = 0;
    if (Bool_val(Field(src, 1))) events |= G_IO_IN;
    if (Bool_val(Field(src, 2))) events |= G_IO_OUT;
    gpollfd->events = events;
  }

  lwt_timeout = Int_val(val_timeout);
  if (timeout < 0 || (lwt_timeout >= 0 && lwt_timeout < timeout))
    timeout = lwt_timeout;

  /* Do the blocking call. */
  caml_enter_blocking_section();
  g_main_context_get_poll_func(gc)(gpollfds, n_fds + count, timeout);
  caml_leave_blocking_section();

  g_main_context_check(gc, max_priority, gpollfds, n_fds);

  /* Build the result. */
  node_result = Val_int(0);
  for (i = n_fds, node = val_fds; i < n_fds + count; i++, node = Field(node, 1)) {
    gpollfd = gpollfds + i;
    src_result = caml_alloc_tuple(3);
    src = Field(node, 0);
    Field(src_result, 0) = Field(src, 0);
    revents = gpollfd->revents;
    if (revents & G_IO_HUP) {
      /* Treat HUP as ready. There's no point continuing to wait on this FD. */
      if (gpollfd->events & G_IO_IN)
        revents |= G_IO_IN;
      if (gpollfd->events & G_IO_OUT)
        revents |= G_IO_OUT;
    }
    Field(src_result, 1) = Val_bool(revents & G_IO_IN);
    Field(src_result, 2) = Val_bool(revents & G_IO_OUT);
    tmp = caml_alloc_tuple(2);
    Field(tmp, 0) = src_result;
    Field(tmp, 1) = node_result;
    node_result = tmp;
  }

  CAMLreturn(node_result);
}

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

CAMLprim value lwt_glib_get_sources(value Unit)
{
  gint timeout;
  int i;
  int events;
  GPollFD *gpollfd;

  CAMLparam0();
  CAMLlocal3(fds, watches, result);

  g_main_context_dispatch(gc);
  g_main_context_prepare(gc, &max_priority);

  while (fds_count < (n_fds = g_main_context_query(gc, max_priority, &timeout, gpollfds, fds_count))) {
    free(gpollfds);
    fds_count = n_fds;
    gpollfds = lwt_unix_malloc(fds_count * sizeof (GPollFD));
  }

  fds = caml_alloc_tuple(n_fds);
  watches = caml_alloc_tuple(n_fds);
  for (i = 0; i < n_fds; i++) {
    gpollfd = gpollfds + i;
    gpollfd->revents = 0;

    events = 0;
    if (gpollfd->events & G_IO_IN) events |= 1;
    if (gpollfd->events & G_IO_OUT) events |= 2;

#if defined(LWT_ON_WINDOWS)
    /* On windows, glib file descriptors are handles */
    Field(fds, i) = alloc_fd((HANDLE)gpollfd->fd);
#else
    Field(fds, i) = Val_int(gpollfd->fd);
    if (gpollfd->fd < 0) events = 0;
#endif

    Field(watches, i) = Val_int(events);
  }

  result = caml_alloc_tuple(3);
  Store_field(result, 0, fds);
  Store_field(result, 1, watches);
  Store_field(result, 2, caml_copy_double(timeout * 1e-3));

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

CAMLprim value lwt_glib_check(value Unit)
{
  g_main_context_check(gc, max_priority, gpollfds, n_fds);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Initialization/stopping                                         |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_glib_init(value Unit)
{
  gc = g_main_context_default();
  g_main_context_ref(gc);
  return Val_unit;
}

CAMLprim value lwt_glib_stop(value Unit)
{
  g_main_context_unref(gc);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Misc                                                            |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_glib_iter(value may_block)
{
  GMainContext *gc;
  gint max_priority, timeout;
  GPollFD *pollfds = NULL;
  gint pollfds_size = 0;
  gint nfds;
  gint i;

  /* Get the main context. */
  gc = g_main_context_default();

  /* Try to acquire it. */
  if (!g_main_context_acquire(gc))
    caml_failwith("Lwt_glib.iter");

  /* Dispatch pending events. */
  g_main_context_dispatch(gc);

  /* Prepare the context for polling. */
  g_main_context_prepare(gc, &max_priority);

  /* Get all file descriptors to poll. */
  while (pollfds_size < (nfds = g_main_context_query(gc, max_priority, &timeout, pollfds, pollfds_size))) {
    free(pollfds);
    pollfds_size = nfds;
    pollfds = lwt_unix_malloc(pollfds_size * sizeof (GPollFD));
  }

  /* Clear all revents fields. */
  for (i = 0; i < nfds; i++) pollfds[i].revents = 0;

  /* Set the timeout to 0 if we do not want to block. */
  if (!Bool_val(may_block)) timeout = 0;

  /* Do the blocking call. */
  caml_enter_blocking_section();
  g_main_context_get_poll_func(gc)(pollfds, nfds, timeout);
  caml_leave_blocking_section();

  /* Let glib parse the result. */
  g_main_context_check(gc, max_priority, pollfds, nfds);

  /* Release the context. */
  g_main_context_release(gc);

  free(pollfds);

  return Val_unit;
}

CAMLprim value lwt_glib_wakeup(value Unit)
{
  g_main_context_wakeup(g_main_context_default());
  return Val_unit;
}
