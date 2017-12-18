/* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2010 Jérémie Dimino
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

#if defined(LWT_ON_WINDOWS)

#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

CAMLprim value lwt_unix_bytes_read(value fd, value buf, value vofs, value vlen)
{
    intnat ofs, len, written;
    DWORD numbytes, numwritten;
    DWORD err = 0;

    Begin_root(buf);
    ofs = Long_val(vofs);
    len = Long_val(vlen);
    written = 0;
    if (len > 0) {
        numbytes = len;
        if (Descr_kind_val(fd) == KIND_SOCKET) {
            int ret;
            SOCKET s = Socket_val(fd);
            ret = recv(s, (char *)Caml_ba_array_val(buf)->data + ofs, numbytes,
                       0);
            if (ret == SOCKET_ERROR) err = WSAGetLastError();
            numwritten = ret;
        } else {
            HANDLE h = Handle_val(fd);
            if (!ReadFile(h, (char *)Caml_ba_array_val(buf)->data + ofs,
                          numbytes, &numwritten, NULL))
                err = GetLastError();
        }
        if (err) {
            win32_maperr(err);
            uerror("write", Nothing);
        }
        written = numwritten;
    }
    End_roots();
    return Val_long(written);
}
#endif
