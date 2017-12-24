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

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/socketaddr.h>
#include <caml/unixsupport.h>

#include "unix_mcast_utils.h"

int socket_domain(int fd)
{
    /* Return the socket domain, PF_INET or PF_INET6. Fails for non-IP
       protos.
       fd must be a socket!
    */
    union sock_addr_union addr;
    socklen_t l;

    l = sizeof(addr);
    if (getsockname(fd, &addr.s_gen, &l) == -1) uerror("getsockname", Nothing);

    switch (addr.s_gen.sa_family) {
        case AF_INET:
            return PF_INET;
        case AF_INET6:
            return PF_INET6;
        default:
            caml_invalid_argument("Not an Internet socket");
    }

    return 0;
}
#endif
