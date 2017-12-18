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
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <unistd.h>
#include <sys/mman.h>

#include "lwt_unix.h"

#ifdef __CYGWIN__
LWT_NOT_AVAILABLE4(unix_mincore)
#else

#ifdef HAVE_BSD_MINCORE
#define MINCORE_VECTOR_TYPE char
#else
#define MINCORE_VECTOR_TYPE unsigned char
#endif

CAMLprim value lwt_unix_mincore(value val_buffer, value val_offset,
                                value val_length, value val_states)
{
    long len = Wosize_val(val_states);
    MINCORE_VECTOR_TYPE vec[len];
    mincore((char *)Caml_ba_data_val(val_buffer) + Long_val(val_offset),
            Long_val(val_length), vec);
    long i;
    for (i = 0; i < len; i++) Field(val_states, i) = Val_bool(vec[i] & 1);
    return Val_unit;
}

#undef MINCORE_VECTOR_TYPE

#endif

#endif
