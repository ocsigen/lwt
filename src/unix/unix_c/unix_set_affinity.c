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

CAMLprim value lwt_unix_set_affinity(value val_pid, value val_cpus)
{
    cpu_set_t cpus;
    CPU_ZERO(&cpus);
    for (; Is_block(val_cpus); val_cpus = Field(val_cpus, 1))
        CPU_SET(Int_val(Field(val_cpus, 0)), &cpus);
    if (sched_setaffinity(Int_val(val_pid), sizeof(cpu_set_t), &cpus) < 0)
        uerror("sched_setaffinity", Nothing);
    return Val_unit;
}

#else

LWT_NOT_AVAILABLE2(unix_set_affinity)

#endif
#endif
