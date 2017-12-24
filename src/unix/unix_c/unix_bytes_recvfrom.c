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

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/socketaddr.h>
#include <caml/unixsupport.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "unix_recv_send_utils.h"

value lwt_unix_bytes_recvfrom(value fd, value buf, value ofs, value len,
                              value flags)
{
    CAMLparam5(fd, buf, ofs, len, flags);
    CAMLlocal2(result, address);
    int ret;
    union sock_addr_union addr;
    socklen_t addr_len;
    addr_len = sizeof(addr);
    ret = recvfrom(Int_val(fd), (char *)Caml_ba_data_val(buf) + Long_val(ofs),
                   Long_val(len), caml_convert_flag_list(flags, msg_flag_table),
                   &addr.s_gen, &addr_len);
    if (ret == -1) uerror("recvfrom", Nothing);
    address = alloc_sockaddr(&addr, addr_len, -1);
    result = caml_alloc_tuple(2);
    Field(result, 0) = Val_int(ret);
    Field(result, 1) = address;
    CAMLreturn(result);
}
#endif
