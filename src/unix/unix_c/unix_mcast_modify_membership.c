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
#include <string.h>

#include "unix_mcast_utils.h"

/* Keep this in sync with the type Lwt_unix.mcast_action */
#define VAL_MCAST_ACTION_ADD (Val_int(0))
#define VAL_MCAST_ACTION_DROP (Val_int(1))

CAMLprim value lwt_unix_mcast_modify_membership(value fd, value v_action,
                                                value if_addr, value group_addr)
{
    int t, r;
    int fd_sock;
    int optname;

    fd_sock = Int_val(fd);
    t = socket_domain(fd_sock);
    r = 0;

    switch (t) {
        case PF_INET: {
            struct ip_mreq mreq;

            if (caml_string_length(group_addr) != 4 ||
                caml_string_length(if_addr) != 4) {
                caml_invalid_argument(
                    "lwt_unix_mcast_modify: Not an IPV4 address");
            }

            memcpy(&mreq.imr_multiaddr, &GET_INET_ADDR(group_addr), 4);
            memcpy(&mreq.imr_interface, &GET_INET_ADDR(if_addr), 4);

            switch (v_action) {
                case VAL_MCAST_ACTION_ADD:
                    optname = IP_ADD_MEMBERSHIP;
                    break;

                default:
                    optname = IP_DROP_MEMBERSHIP;
                    break;
            }

            r = setsockopt(fd_sock, IPPROTO_IP, optname, (void *)&mreq,
                           sizeof(mreq));
            break;
        }
        default:
            caml_invalid_argument("lwt_unix_mcast_modify_membership");
    };

    if (r == -1) uerror("setsockopt", Nothing);

    return Val_unit;
}
#endif
