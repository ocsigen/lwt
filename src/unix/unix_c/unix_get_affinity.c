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
#include <sched.h>

#include "lwt_unix.h"

#if defined(HAVE_AFFINITY)

CAMLprim value lwt_unix_get_affinity(value val_pid)
{
    CAMLparam1(val_pid);
    CAMLlocal2(list, node);
    cpu_set_t cpus;
    if (sched_getaffinity(Int_val(val_pid), sizeof(cpu_set_t), &cpus) < 0)
        uerror("sched_getaffinity", Nothing);
    int i;
    list = Val_int(0);
    for (i = sizeof(cpu_set_t) * 8 - 1; i >= 0; i--) {
        if (CPU_ISSET(i, &cpus)) {
            node = caml_alloc_tuple(2);
            Field(node, 0) = Val_int(i);
            Field(node, 1) = list;
            list = node;
        }
    }
    CAMLreturn(list);
}

#else

LWT_NOT_AVAILABLE1(unix_get_affinity)

#endif
#endif
