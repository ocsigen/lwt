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

#include "lwt_config.h"

/*
 * Included in:
 * - unix_gethostname_job.c
 * - unix_gethostbyname_job.c
 * - unix_gethostbyaddr_job.c
 * - unix_getprotoby_getservby_job.c
 */
#if !defined(LWT_ON_WINDOWS)

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <netdb.h>

#include "lwt_unix.h"

#define NETDB_BUFFER_SIZE 10000

/* keep test in sync with discover.ml */
#if !defined(HAS_GETHOSTBYADDR_R) || \
    (HAS_GETHOSTBYADDR_R != 7 && HAS_GETHOSTBYADDR_R != 8)
#define NON_R_GETHOSTBYADDR 1
#endif

/* keep test in sync with discover.ml */
#if !defined(HAS_GETHOSTBYNAME_R) || \
    (HAS_GETHOSTBYNAME_R != 5 && HAS_GETHOSTBYNAME_R != 6)
#define NON_R_GETHOSTBYNAME 1
#endif

#if defined(NON_R_GETHOSTBYADDR) || defined(NON_R_GETHOSTBYNAME)
char **c_copy_addr_array(char **src, int addr_len);
#endif

#if !defined(HAVE_NETDB_REENTRANT) || defined(NON_R_GETHOSTBYADDR) || \
    defined(NON_R_GETHOSTBYNAME)
char **c_copy_string_array(char **src);
void c_free_string_array(char **src);
char *s_strdup(const char *s);
#endif

value alloc_host_entry(struct hostent *entry);

#if defined(NON_R_GETHOSTBYADDR) || defined(NON_R_GETHOSTBYNAME)
struct hostent *hostent_dup(struct hostent *orig);
void hostent_free(struct hostent *h);
#endif
#endif
