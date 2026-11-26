/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */



#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#include <errno.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/socketaddr.h>
#include <caml/unixsupport.h>

#include "unix_mcast_utils.h"

CAMLprim value lwt_unix_mcast_set_ttl(value fd, value ttl)
{
    int t, r;
    /* A byte: OpenBSD refuses any other size, and the other systems accept
       it. A TTL that does not fit is refused, as Linux does. */
    unsigned char v;
    int fd_sock;

    fd_sock = Int_val(fd);
    t = socket_domain(fd_sock);
    if (Int_val(ttl) < 0 || Int_val(ttl) > 255)
        unix_error(EINVAL, "setsockopt", Nothing);
    v = Int_val(ttl);
    r = 0;

    switch (t) {
        case PF_INET:
            r = setsockopt(fd_sock, IPPROTO_IP, IP_MULTICAST_TTL, (void *)&v,
                           sizeof(v));
            break;
        default:
            caml_invalid_argument("lwt_unix_mcast_set_ttl");
    };

    if (r == -1) uerror("setsockopt", Nothing);

    return Val_unit;
}
#endif
