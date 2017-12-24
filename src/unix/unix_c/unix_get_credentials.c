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

#define _GNU_SOURCE

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "lwt_unix.h"

#if defined(HAVE_GET_CREDENTIALS_LINUX)
#define CREDENTIALS_TYPE struct ucred
#define CREDENTIALS_FIELD(id) id
#elif defined(HAVE_GET_CREDENTIALS_NETBSD)
#define CREDENTIALS_TYPE struct sockcred
#define CREDENTIALS_FIELD(id) sc_##id
#elif defined(HAVE_GET_CREDENTIALS_OPENBSD)
#define CREDENTIALS_TYPE struct sockpeercred
#define CREDENTIALS_FIELD(id) id
#elif defined(HAVE_GET_CREDENTIALS_FREEBSD)
#define CREDENTIALS_TYPE struct cmsgcred
#define CREDENTIALS_FIELD(id) cmsgcred_##id
#endif

#if defined(CREDENTIALS_TYPE)

CAMLprim value lwt_unix_get_credentials(value fd)
{
    CAMLparam1(fd);
    CAMLlocal1(res);
    CREDENTIALS_TYPE cred;
    socklen_t cred_len = sizeof(cred);

    if (getsockopt(Int_val(fd), SOL_SOCKET, SO_PEERCRED, &cred, &cred_len) ==
        -1)
        uerror("get_credentials", Nothing);

    res = caml_alloc_tuple(3);
    Store_field(res, 0, Val_int(cred.CREDENTIALS_FIELD(pid)));
    Store_field(res, 1, Val_int(cred.CREDENTIALS_FIELD(uid)));
    Store_field(res, 2, Val_int(cred.CREDENTIALS_FIELD(gid)));
    CAMLreturn(res);
}

#elif defined(HAVE_GETPEEREID)

CAMLprim value lwt_unix_get_credentials(value fd)
{
    CAMLparam1(fd);
    CAMLlocal1(res);
    uid_t euid;
    gid_t egid;

    if (getpeereid(Int_val(fd), &euid, &egid) == -1)
        uerror("get_credentials", Nothing);

    res = caml_alloc_tuple(3);

    /* This triggers a warning on OCaml 4.02, because Val_int (actually,
       Val_long) shifts the -1 left by one bit. It seems difficult to suppress,
       because wrapping the -1 in a cast to an unsigned type is undone by
       Val_long, which internally casts the -1 back to a signed type. Inlining a
       suitable macro definition is not future-safe. The warning could be
       suppressed by using conditional compilation to check for OCaml 4.02, and
       inlining a suitable definition only in that case, but it seems not worth
       the trouble, and that it is better to live with the warning for now.

       See

         https://caml.inria.fr/mantis/view.php?id=5934
         ocaml/ocaml@24c118d7b63cdab58ed9bad28e2d337e9d1d30ba */
    Store_field(res, 0, Val_int(-1));

    Store_field(res, 1, Val_int(euid));
    Store_field(res, 2, Val_int(egid));
    CAMLreturn(res);
}

#else

LWT_NOT_AVAILABLE1(unix_get_credentials)

#endif


#endif
