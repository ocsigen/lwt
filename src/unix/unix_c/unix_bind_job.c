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
#include <caml/socketaddr.h>
#include <caml/unixsupport.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "lwt_unix.h"

struct job_bind {
    struct lwt_unix_job job;
    int fd;
    union sock_addr_union addr;
    socklen_param_type addr_len;
    int result;
    int error_code;
};

static void worker_bind(struct job_bind *job)
{
    job->result = bind(job->fd, &job->addr.s_gen, job->addr_len);
    job->error_code = errno;
}

static value result_bind(struct job_bind *job)
{
    LWT_UNIX_CHECK_JOB(job, job->result != 0, "bind");
    lwt_unix_free_job(&job->job);
    return Val_unit;
}

CAMLprim value lwt_unix_bind_job(value fd, value address)
{
    LWT_UNIX_INIT_JOB(job, bind, 0);
    job->fd = Int_val(fd);
    get_sockaddr(address, &job->addr, &job->addr_len);

    return lwt_unix_alloc_job(&job->job);
}
#endif
