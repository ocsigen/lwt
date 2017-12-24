/* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2009-2010 Jérémie Dimino
 *               2009 Mauricio Fernandez
 *               2010 Pierre Chambart
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

#pragma once

#include "lwt_config.h"

/*
 * header included in:
 * unix_send
 * unix_bytes_send
 * unix_recv
 * unix_bytes_recv
 * unix_recvfrom
 * unix_bytes_recvfrom
 * unix_sendto
 * unix_bytes_sendto
 * unix_recv_msg
 * unix_bytes_recv_msg
 * unix_send_msg
 * unix_getaddrinfo_job
 */

#if !defined(LWT_ON_WINDOWS)

#include <caml/mlvalues.h>
#include <caml/socketaddr.h>
#include <sys/socket.h>
#include <sys/uio.h>

static int msg_flag_table[3] = {MSG_OOB, MSG_DONTROUTE, MSG_PEEK};
extern int socket_domain_table[];
extern int socket_type_table[];
extern void get_sockaddr(value mladdr, union sock_addr_union *addr /*out*/,
                         socklen_t *addr_len /*out*/);
void store_iovs(struct iovec *iovs, value iovs_val);
void bytes_store_iovs(struct iovec *iovs, value iovs_val);
value wrapper_recv_msg(int fd, int n_iovs, struct iovec *iovs);
value wrapper_send_msg(int fd, int n_iovs, struct iovec *iovs,
                       value val_n_fds, value val_fds);
#endif
