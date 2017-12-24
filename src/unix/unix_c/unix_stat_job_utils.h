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

#pragma once

/* Included in:
 * - unix_stat_job.c
 * - unix_stat64_job.c
 * - unix_lstat_job.c
 * - unix_lstat_64_job.c
 * - unix_fstat_job.c
 * - unix_fstat_64_job.c
 */
#include "lwt_config.h"

#if !defined(LWT_ON_WINDOWS)

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "lwt_unix.h"

struct job_stat {
    struct lwt_unix_job job;
    struct stat stat;
    int result;
    int error_code;
    char *name;
    char data[];
};

struct job_lstat {
    struct lwt_unix_job job;
    struct stat lstat;
    int result;
    int error_code;
    char *name;
    char data[];
};

struct job_fstat {
    struct lwt_unix_job job;
    int fd;
    struct stat fstat;
    int result;
    int error_code;
};

value copy_stat(int use_64, struct stat *buf);

void worker_stat(struct job_stat *job);
value result_stat(struct job_stat *job);
value result_stat_64(struct job_stat *job);

void worker_lstat(struct job_lstat *job);
value result_lstat(struct job_lstat *job);
value result_lstat_64(struct job_lstat *job);

void worker_fstat(struct job_fstat *job);
value result_fstat(struct job_fstat *job);
value result_fstat_64(struct job_fstat *job);
#endif
