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
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <dirent.h>
#include <sys/types.h>
#include <errno.h>

#include "lwt_unix.h"

struct job_readdir {
    struct lwt_unix_job job;
    DIR *dir;
    struct dirent *entry;
    int error_code;
};

static void worker_readdir(struct job_readdir *job)
{
    // From the man page of readdir
    // If the end of the directory stream is reached, NULL is returned and
    // errno is not changed. If an error occurs, NULL is returned and errno
    // is set appropriately. To distinguish end of stream and from an error,
    // set errno to zero before calling readdir() and then check the value of
    // errno if NULL is returned.
    errno = 0;
    job->entry = readdir(job->dir);
    job->error_code = errno;
}

static value result_readdir(struct job_readdir *job)
{
    LWT_UNIX_CHECK_JOB(job, job->entry == NULL && job->error_code != 0,
                       "readdir");
    if (job->entry == NULL) {
        // From the man page
        // On success, readdir() returns a pointer to a dirent structure.
        // (This structure may be statically allocated; do not attempt to
        // free(3) it.)
        lwt_unix_free_job(&job->job);
        caml_raise_end_of_file();
    } else {
        value name = caml_copy_string(job->entry->d_name);
        // see above about not freeing dirent
        lwt_unix_free_job(&job->job);
        return name;
    }
}

CAMLprim value lwt_unix_readdir_job(value val_dir)
{
    LWT_UNIX_INIT_JOB(job, readdir, 0);
    job->dir = DIR_Val(val_dir);
    return lwt_unix_alloc_job(&job->job);
}
#endif
