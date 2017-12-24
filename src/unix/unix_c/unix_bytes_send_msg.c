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

#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#include <caml/mlvalues.h>
#include <sys/uio.h>

#include "unix_recv_send_utils.h"

CAMLprim value lwt_unix_bytes_send_msg(value val_fd, value val_n_iovs,
                                       value val_iovs, value val_n_fds,
                                       value val_fds)
{
    int n_iovs = Int_val(val_n_iovs);
    struct iovec iovs[n_iovs];
    bytes_store_iovs(iovs, val_iovs);
    return wrapper_send_msg(Int_val(val_fd), n_iovs, iovs, val_n_fds, val_fds);
}
#endif
