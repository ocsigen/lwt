(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Pa_log
 * Copyright (C) 2009 Jérémie Dimino
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
 *)


(** Logging facility.

    It replaces expression of the form:

    {[
      Log#info "x = %d" x
    ]}

    in a file "foo.ml" by:

    {[
      if Lwt_log.level !Lwt_log.default `Info then
        Lwt_log.log ~level:Lwt_log.Info "Foo: x = %d" x
    ]}

    You can also log exception with the backtrace if available:

    {[
      Log#exn exn "uncaught exception"
    ]}

    Note that the application must be complete. For example: [Log#info
    "%d"] will make compilation to fail.

    It also add the command line flags "-no-debug" which remove all
    debug messages.
*)
