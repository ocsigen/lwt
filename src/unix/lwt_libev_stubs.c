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
#include <sys/wait.h>
#include <ev.h>

#include "lwt_unix.h"

struct ev_loop *lwt_libev_main_loop = NULL;

CAMLextern int caml_convert_signal_number (int);
CAMLextern int caml_rev_convert_signal_number (int);

/* +-----------------------------------------------------------------+
   | Initialisation                                                  |
   +-----------------------------------------------------------------+ */

CAMLprim value lwt_libev_init()
{
  lwt_libev_main_loop = ev_default_loop(EVFLAG_FORKCHECK);
  if (!lwt_libev_main_loop) caml_failwith("lwt_libev_init: could not initialise the default loop");
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ */

int in_blocking_section;

void lwt_libev_check_blocking_section()
{
  if (in_blocking_section) {
    in_blocking_section = 0;
    caml_leave_blocking_section();
  }
}

CAMLprim value lwt_libev_loop()
{
  caml_enter_blocking_section();
  in_blocking_section = 1;
  ev_loop(lwt_libev_main_loop, EVLOOP_ONESHOT);
  lwt_libev_check_blocking_section();
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Watchers                                                        |
   +-----------------------------------------------------------------+ */

#define Ev_io_val(v) *(struct ev_io**)Data_custom_val(v)
#define Ev_signal_val(v) *(struct ev_signal**)Data_custom_val(v)
#define Ev_timer_val(v) *(struct ev_timer**)Data_custom_val(v)
#define Ev_child_val(v) *(struct ev_child**)Data_custom_val(v)
#define Ev_idle_val(v) *(struct ev_idle**)Data_custom_val(v)

static void finalize_watcher(value watcher)
{
  free(Data_custom_val(watcher));
}

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
  finalize_watcher,
  compare_watchers,
  hash_watcher,
  custom_serialize_default,
  custom_deserialize_default
};

static void* xalloc(size_t n)
{
  void* ptr = malloc(n);
  if (!ptr) caml_failwith("out of memory");
  return ptr;
}

#define new(type) ((type*)xalloc(sizeof(type)))

/* +-----------------------------------------------------------------+
   | IO watchers                                                     |
   +-----------------------------------------------------------------+ */

static void handle_io(struct ev_loop *loop, ev_io *watcher, int revents)
{
  lwt_libev_check_blocking_section();
  value meta = (value)watcher->data;
  caml_callback(Field(meta, 0), Field(meta, 1));
}

CAMLprim value lwt_libev_io_init(value fd, value event, value callback)
{
  CAMLparam3(fd, event, callback);
  CAMLlocal2(result, meta);
  /* Create and initialise the watcher */
  struct ev_io* watcher = new(struct ev_io);
  ev_io_init(watcher, handle_io, FD_val(fd), Int_val(event) == 0 ? EV_READ : EV_WRITE);
  /* Wrap the watcher into a custom caml value */
  result = caml_alloc_custom(&watcher_ops, sizeof(struct ev_io*), 0, 1);
  Ev_io_val(result) = watcher;
  /* Creates meta-data */
  meta = caml_alloc_tuple(2);
  Store_field(meta, 0, callback);
  Store_field(meta, 1, result);
  /* Store metadata in the watcher, and register it as a root */
  watcher->data = (void*)meta;
  caml_register_generational_global_root((value*)(&(watcher->data)));
  /* Start the event */
  ev_io_start(lwt_libev_main_loop, watcher);
  CAMLreturn(result);
}

CAMLprim value lwt_libev_io_stop(value watcher)
{
  ev_io_stop(lwt_libev_main_loop, Ev_io_val(watcher));
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Signal watchers                                                 |
   +-----------------------------------------------------------------+ */

static void handle_signal(struct ev_loop *loop, ev_signal *watcher, int revents)
{
  lwt_libev_check_blocking_section();
  value meta = (value)watcher->data;
  caml_callback(Field(meta, 0), Field(meta, 1));
}

CAMLprim value lwt_libev_signal_init(value signum, value callback)
{
  CAMLparam2(signum, callback);
  CAMLlocal2(result, meta);
  /* Create and initialise the watcher */
  struct ev_signal* watcher = new(struct ev_signal);
  ev_signal_init(watcher, handle_signal, caml_convert_signal_number(Int_val(signum)));
  /* Wrap the watcher into a custom caml value */
  result = caml_alloc_custom(&watcher_ops, sizeof(struct ev_signal*), 0, 1);
  Ev_signal_val(result) = watcher;
  /* Creates meta-data */
  meta = caml_alloc_tuple(2);
  Store_field(meta, 0, callback);
  Store_field(meta, 1, result);
  /* Store metadata in the watcher, and register it as a root */
  watcher->data = (void*)meta;
  caml_register_generational_global_root((value*)(&(watcher->data)));
  /* Start the event */
  ev_signal_start(lwt_libev_main_loop, watcher);
  CAMLreturn(result);
}

CAMLprim value lwt_libev_signal_stop(value watcher)
{
  ev_signal_stop(lwt_libev_main_loop, Ev_signal_val(watcher));
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Timer watchers                                                  |
   +-----------------------------------------------------------------+ */

static void handle_timer(struct ev_loop *loop, ev_timer *watcher, int revents)
{
  lwt_libev_check_blocking_section();
  value meta = (value)watcher->data;
  caml_callback(Field(meta, 0), Field(meta, 1));
}

CAMLprim value lwt_libev_timer_init(value delay, value callback)
{
  CAMLparam2(delay, callback);
  CAMLlocal2(result, meta);
  /* Create and initialise the watcher */
  struct ev_timer* watcher = new(struct ev_timer);
  ev_timer_init(watcher, handle_timer, Double_val(delay), 0);
  /* Wrap the watcher into a custom caml value */
  result = caml_alloc_custom(&watcher_ops, sizeof(struct ev_timer*), 0, 1);
  Ev_timer_val(result) = watcher;
  /* Creates meta-data */
  meta = caml_alloc_tuple(2);
  Store_field(meta, 0, callback);
  Store_field(meta, 1, result);
  /* Store metadata in the watcher, and register it as a root */
  watcher->data = (void*)meta;
  caml_register_generational_global_root((value*)(&(watcher->data)));
  /* Start the event */
  ev_timer_start(lwt_libev_main_loop, watcher);
  CAMLreturn(result);
}

CAMLprim value lwt_libev_timer_stop(value watcher)
{
  ev_timer_stop(lwt_libev_main_loop, Ev_timer_val(watcher));
  return Val_unit;
}

/* +-----------------------------------------------------------------+
   | Child watchers                                                  |
   +-----------------------------------------------------------------+ */

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

static value alloc_process_status(int pid, int status)
{
  value st, res;

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
  Begin_root (st);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_int(pid);
    Field(res, 1) = st;
  End_roots();
  return res;
}

static void handle_child(struct ev_loop *loop, ev_child *watcher, int revents)
{
  lwt_libev_check_blocking_section();
  value status = alloc_process_status(watcher->pid, watcher->rstatus);
  value meta = (value)watcher->data;
  caml_callback2(Field(meta, 0), Field(meta, 1), status);
}

CAMLprim value lwt_libev_child_init(value pid, value callback)
{
  CAMLparam2(pid, callback);
  CAMLlocal2(result, meta);
  /* Create and initialise the watcher */
  struct ev_child* watcher = new(struct ev_child);
  ev_child_init(watcher, handle_child, Int_val(pid), 0);
  /* Wrap the watcher into a custom caml value */
  result = caml_alloc_custom(&watcher_ops, sizeof(struct ev_child*), 0, 1);
  Ev_child_val(result) = watcher;
  /* Creates meta-data */
  meta = caml_alloc_tuple(2);
  Store_field(meta, 0, callback);
  Store_field(meta, 1, result);
  /* Store metadata in the watcher, and register it as a root */
  watcher->data = (void*)meta;
  caml_register_generational_global_root((value*)(&(watcher->data)));
  /* Start the event */
  ev_child_start(lwt_libev_main_loop, watcher);
  CAMLreturn(result);
}

CAMLprim value lwt_libev_child_stop(value watcher)
{
  ev_child_stop(lwt_libev_main_loop, Ev_child_val(watcher));
  return Val_unit;
}
