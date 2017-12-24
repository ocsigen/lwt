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

#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <string.h>

#include "lwt_unix.h"

#ifdef __CYGWIN__
LWT_NOT_AVAILABLE2(unix_wait_mincore_job)
#else
struct job_wait_mincore {
    struct lwt_unix_job job;
    char *ptr;
};

static void worker_wait_mincore(struct job_wait_mincore *job)
{
    /* Read the byte to force the kernel to fetch the page: */
    char dummy;
    memcpy(&dummy, job->ptr, 1);
}

static value result_wait_mincore(struct job_wait_mincore *job)
{
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_wait_mincore_job(value val_buffer, value val_offset)
{
    LWT_UNIX_INIT_JOB(job, wait_mincore, 0);
    job->ptr = (char *)Caml_ba_data_val(val_buffer) + Long_val(val_offset);
    return lwt_unix_alloc_job(&(job->job));
}
#endif
#endif
