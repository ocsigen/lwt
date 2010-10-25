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

#endif /* __LWT_UNIX_H */
