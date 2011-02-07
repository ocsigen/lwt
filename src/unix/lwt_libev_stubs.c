/* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_unix_stubs
 * Copyright (C) 2010 Jérémie Dimino
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

/* Stubs for libev */

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <ev.h>

#include "lwt_unix.h"

/* +-----------------------------------------------------------------+
   | Loops                                                           |
   +-----------------------------------------------------------------+ */

static int compare_loops(value a, value b)
{
  return (int)(Data_custom_val(a) - Data_custom_val(b));
}

static long hash_loop(value loop)
{
  return (long)Data_custom_val(loop);
}

static struct custom_operations loop_ops = {
  "lwt.libev.loop",
  custom_finalize_default,
  compare_loops,
  hash_loop,
  custom_serialize_default,
  custom_deserialize_default
};

CAMLprim value lwt_libev_init()
{
  struct ev_loop *loop = ev_loop_new(EVFLAG_FORKCHECK);
  if (!loop) caml_failwith("lwt_libev_init");
  value result = caml_alloc_custom(&loop_ops, sizeof(struct ev_loop*), 0, 1);
  Ev_loop_val(result) = loop;
  return result;
}

CAMLprim value lwt_libev_stop(value loop)
{
  ev_loop_destroy(Ev_loop_val(loop));
  return Val_unit;
}

static int lwt_libev_in_blocking_section = 0;

#define LWT_LIBEV_CHECK                          \
  if (lwt_libev_in_blocking_section) {           \
    lwt_libev_in_blocking_section = 0;           \
    caml_leave_blocking_section();               \
  }

CAMLprim value lwt_libev_loop(value loop, value block)
{
  caml_enter_blocking_section();
  lwt_libev_in_blocking_section = 1;
  ev_loop(Ev_loop_val(loop), Bool_val(block) ? EVLOOP_ONESHOT : EVLOOP_ONESHOT | EVLOOP_NONBLOCK);
  LWT_LIBEV_CHECK;
  return Val_unit;
}

CAMLprim value lwt_libev_unloop(value loop)
{
  ev_unloop(Ev_loop_val(loop), EVUNLOOP_ONE);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Watchers                                                        |
   +-----------------------------------------------------------------+ */

#define Ev_io_val(v) *(struct ev_io**)Data_custom_val(v)
#define Ev_timer_val(v) *(struct ev_timer**)Data_custom_val(v)

static int compare_watchers(value a, value b)
{
  return (int)(Data_custom_val(a) - Data_custom_val(b));
}

static long hash_watcher(value watcher)
{
  return (long)Data_custom_val(watcher);
}

static struct custom_operations watcher_ops = {
  "lwt.libev.watcher",
  custom_finalize_default,
  compare_watchers,
  hash_watcher,
  custom_serialize_default,
  custom_deserialize_default
};

/* +-----------------------------------------------------------------+
   | IO watchers                                                     |
   +-----------------------------------------------------------------+ */

static void handle_io(struct ev_loop *loop, ev_io *watcher, int revents)
{
  LWT_LIBEV_CHECK;
  caml_callback((value)watcher->data, Val_unit);
}

static value lwt_libev_io_init(struct ev_loop *loop, int fd, int event, value callback)
{
  CAMLparam1(callback);
  CAMLlocal1(result);
  /* Create and initialise the watcher */
  struct ev_io* watcher = lwt_unix_new(struct ev_io);
  ev_io_init(watcher, handle_io, fd, event);
  /* Wrap the watcher into a custom caml value */
  result = caml_alloc_custom(&watcher_ops, sizeof(struct ev_io*), 0, 1);
  Ev_io_val(result) = watcher;
  /* Store the callback in the watcher, and register it as a root */
  watcher->data = (void*)callback;
  caml_register_generational_global_root((value*)(&(watcher->data)));
  /* Start the event */
  ev_io_start(loop, watcher);
  CAMLreturn(result);
}

CAMLprim value lwt_libev_readable_init(value loop, value fd, value callback)
{
  return lwt_libev_io_init(Ev_loop_val(loop), FD_val(fd), EV_READ, callback);
}

CAMLprim value lwt_libev_writable_init(value loop, value fd, value callback)
{
  return lwt_libev_io_init(Ev_loop_val(loop), FD_val(fd), EV_WRITE, callback);
}

CAMLprim value lwt_libev_io_stop(value loop, value val_watcher)
{
  struct ev_io* watcher = Ev_io_val(val_watcher);
  caml_remove_generational_global_root((value*)(&(watcher->data)));
  ev_io_stop(Ev_loop_val(loop), watcher);
  free(watcher);
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Timer watchers                                                  |
   +-----------------------------------------------------------------+ */

static void handle_timer(struct ev_loop *loop, ev_timer *watcher, int revents)
{
  LWT_LIBEV_CHECK;
  caml_callback((value)watcher->data, Val_unit);
}

CAMLprim value lwt_libev_timer_init(value loop, value delay, value repeat, value callback)
{
  CAMLparam2(delay, callback);
  CAMLlocal1(result);
  /* Create and initialise the watcher */
  struct ev_timer* watcher = lwt_unix_new(struct ev_timer);
  ev_timer_init(watcher, handle_timer, Double_val(delay), Bool_val(repeat));
  /* Wrap the watcher into a custom caml value */
  result = caml_alloc_custom(&watcher_ops, sizeof(struct ev_timer*), 0, 1);
  Ev_timer_val(result) = watcher;
  /* Store the callback in the watcher, and register it as a root */
  watcher->data = (void*)callback;
  caml_register_generational_global_root((value*)(&(watcher->data)));
  /* Start the event */
  ev_timer_start(Ev_loop_val(loop), watcher);
  CAMLreturn(result);
}

CAMLprim value lwt_libev_timer_stop(value loop, value val_watcher)
{
  struct ev_timer* watcher = Ev_timer_val(val_watcher);
  caml_remove_generational_global_root((value*)(&(watcher->data)));
  ev_timer_stop(Ev_loop_val(loop), watcher);
  free(watcher);
  return Val_unit;
}
