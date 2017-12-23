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

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/socketaddr.h>
#include <caml/unixsupport.h>
#include <netdb.h>
#include <sys/socket.h>

#include "lwt_unix.h"

struct job_getnameinfo {
    struct lwt_unix_job job;
    union sock_addr_union addr;
    socklen_t addr_len;
    int opts;
    char host[4096];
    char serv[1024];
    int result;
};

static int getnameinfo_flag_table[] = {NI_NOFQDN, NI_NUMERICHOST, NI_NAMEREQD,
                                       NI_NUMERICSERV, NI_DGRAM};

static void worker_getnameinfo(struct job_getnameinfo *job)
{
    job->result = getnameinfo((const struct sockaddr *)&job->addr.s_gen,
                              job->addr_len, job->host, sizeof(job->host),
                              job->serv, sizeof(job->serv), job->opts);
}

static value result_getnameinfo(struct job_getnameinfo *job)
{
    CAMLparam0();
    CAMLlocal3(vres, vhost, vserv);
    if (job->result) {
        lwt_unix_free_job(&job->job);
        caml_raise_not_found();
    } else {
        vhost = caml_copy_string(job->host);
        vserv = caml_copy_string(job->serv);
        vres = caml_alloc_small(2, 0);
        Field(vres, 0) = vhost;
        Field(vres, 1) = vserv;
        lwt_unix_free_job(&job->job);
        CAMLreturn(vres);
    }
}

CAMLprim value lwt_unix_getnameinfo_job(value sockaddr, value opts)
{
    LWT_UNIX_INIT_JOB(job, getnameinfo, 0);
    get_sockaddr(sockaddr, &job->addr, &job->addr_len);
    job->opts = caml_convert_flag_list(opts, getnameinfo_flag_table);
    return lwt_unix_alloc_job(&job->job);
}
#endif
