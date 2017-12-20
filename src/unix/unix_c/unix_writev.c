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

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <errno.h>

#include "lwt_unix.h"
#include "unix_readv_writev_utils.h"

/* writev primitive for non-blocking file descriptors. */
CAMLprim value lwt_unix_writev(value fd, value io_vectors, value val_count)
{
    CAMLparam3(fd, io_vectors, val_count);

    size_t count = Long_val(val_count);

    /* Assemble iovec structures on the stack. No data is copied. */
    struct iovec iovecs[count];
    flatten_io_vectors(iovecs, io_vectors, count, NULL, NULL);

    ssize_t result = writev(Int_val(fd), iovecs, count);

    if (result == -1) uerror("writev", Nothing);

    CAMLreturn(Val_long(result));
}
#endif
