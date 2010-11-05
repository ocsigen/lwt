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
#include <glib.h>
#include <ev.h>

#include "../unix/lwt_unix.h"

extern struct ev_loop *lwt_libev_main_loop;

GMainContext *gc;
ev_prepare prepare_watcher;
ev_check check_watcher;
ev_timer timer_watcher;
ev_io *io_watchers = NULL;
GPollFD *gpollfds = NULL;
gint fds_count = 0;
gint n_fds;
gint max_priority;

/* +-----------------------------------------------------------------+
   | Prepare                                                         |
   +-----------------------------------------------------------------+ */

static void nop() {}

static void prepare(struct ev_loop *loop, ev_prepare *watcher, int revents)
{
  gint timeout;

  g_main_context_dispatch(gc);
  g_main_context_prepare(gc, &max_priority);

  while (fds_count < (n_fds = g_main_context_query(gc, max_priority, &timeout, gpollfds, fds_count))) {
    free(gpollfds);
    free(io_watchers);
    fds_count = n_fds;
    gpollfds = malloc(fds_count * sizeof (GPollFD));
    io_watchers = malloc(fds_count * sizeof (ev_io));
  }

  int i;
  for (i = 0; i < n_fds; i++)
    {
      GPollFD *gpollfd = gpollfds + i;
      ev_io *io_watcher = io_watchers + i;

      gpollfd->revents = 0;

      int events = 0;
      if (gpollfd->events & G_IO_IN) events |= EV_READ;
      if (gpollfd->events & G_IO_OUT) events |= EV_WRITE;

      ev_io_init(io_watcher, nop, gpollfd->fd, events);

      ev_set_priority(io_watcher, EV_MINPRI);
      ev_io_start(lwt_libev_main_loop, io_watcher);
    }

  if (timeout >= 0) {
    ev_timer_set(&timer_watcher, timeout * 1e-3, 0.);
    ev_timer_start(lwt_libev_main_loop, &timer_watcher);
  }
}

/* +-----------------------------------------------------------------+
   | Check                                                           |
   +-----------------------------------------------------------------+ */

static void check(struct ev_loop *loop, ev_check *watcher, int revents)
{
  LWT_UNIX_CHECK;

  int i;
  for (i = 0; i < n_fds; i++)
    {
      ev_io *io_watcher = io_watchers + i;

      if (ev_is_pending(io_watcher))
        {
          GPollFD *gpollfd = gpollfds + i;
          int revents = ev_clear_pending(loop, io_watcher);
          int grevents = 0;

          if (revents & EV_READ) grevents |= G_IO_IN;
          if (revents & EV_WRITE) grevents |= G_IO_OUT;

          gpollfd->revents = grevents;
        }

      ev_io_stop (loop, io_watcher);
    }

  if (ev_is_active(&timer_watcher)) ev_timer_stop(loop, &timer_watcher);

  g_main_context_check(gc, max_priority, gpollfds, n_fds);
}

/* +-----------------------------------------------------------------+
   | Installation/suppression                                        |
   +-----------------------------------------------------------------+ */

value lwt_glib_install()
{
  gc = g_main_context_default();
  g_main_context_ref(gc);

  ev_prepare_init(&prepare_watcher, prepare);
  ev_set_priority(&prepare_watcher, EV_MINPRI);
  ev_prepare_start(EV_DEFAULT, &prepare_watcher);

  ev_check_init(&check_watcher, check);
  ev_set_priority(&check_watcher, EV_MAXPRI);
  ev_check_start(EV_DEFAULT, &check_watcher);

  ev_init(&timer_watcher, nop);
  ev_set_priority(&timer_watcher, EV_MINPRI);

  return Val_unit;
}

value lwt_glib_remove()
{
  g_main_context_unref(gc);
  ev_prepare_stop(lwt_libev_main_loop, &prepare_watcher);
  ev_check_stop(lwt_libev_main_loop, &check_watcher);
  if (ev_is_active(&timer_watcher)) ev_timer_stop(lwt_libev_main_loop, &timer_watcher);
  return Val_unit;
}
