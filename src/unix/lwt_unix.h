/*
 * lwt_unix.h
 * ----------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 */

#ifndef __LWT_UNIX_H
#define __LWT_UNIX_H

/* Sends a notification for the given id */
void lwt_unix_send_notification(int id);

/* Launch a thread with tunned parameters */
void lwt_unix_launch_thread(void* (*start)(void*), void* data);

#endif /* __LWT_UNIX_H */
