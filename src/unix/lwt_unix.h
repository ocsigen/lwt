/*
 * lwt_unix.h
 * ----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 */

#ifndef __LWT_UNIX_H
#define __LWT_UNIX_H

#include <caml/unixsupport.h>

#if defined(CRT_fd_val)
#  define FD_val(value) CRT_fd_val(value)
#  define LWT_WINDOWS
#else
#  define FD_val(value) Int_val(value)
#endif

/* Sends a notification for the given id */
void lwt_unix_send_notification(int id);

/* Launch a thread with tunned parameters */
void lwt_unix_launch_thread(void* (*start)(void*), void* data);

extern int lwt_unix_in_blocking_section;

/* Macro to add in libev callbacks. See the manual for
   explanations. */
#define LWT_UNIX_CHECK                          \
  if (lwt_unix_in_blocking_section) {           \
    lwt_unix_in_blocking_section = 0;           \
    caml_leave_blocking_section();              \
  }

#endif /* __LWT_UNIX_H */
