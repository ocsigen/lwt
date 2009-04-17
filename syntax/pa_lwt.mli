(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Pa_lwt
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

(** Syntactic sugars for lwt *)

(** This extension add the following sugars:

    {[
      try_lwt
        <expr>
      with
        <branches>
    ]},

    {[
      try_lwt
        <expr>
      finally
        <expr>
    ]}

    and:

    {[
      try_lwt
        <expr>
      with
        <branches>
      finally
        <expr>
    ]}

    For example:

    {[
      try_lwt
        f x
      with
        | Failure msg ->
            prerr_endline msg;
            return ()
    ]}

    is expanded to:

    {[
      catch (fun _ -> f x)
        (function
           | Failure msg ->
               prerr_endline msg;
               return ()
           | exn ->
               Lwt.fail exn)
    ]}

    Note that the [exn -> Lwt.fail exn] branch is automatically addedd
    when needed.

    It also inline the anonymous bind [>>], to make it usable with
    lwt.
*)
