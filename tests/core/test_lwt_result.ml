(* OCaml promise library
 * http://www.ocsigen.org/lwt
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

open Test
open Lwt


let suite = suite "lwt_result" [
                    test "map acts on Ok values"
                         (fun() ->
                           let x = Lwt_result.return 0 in
                           let correct = Lwt_result.return 1 in
                           return (Lwt_result.map ((+) 1) x = correct)
                         );
                    test "map acts as identity on Error values"
                         (fun() ->
                           let x = Lwt_result.fail 0 in
                           return (Lwt_result.map ((+) 1) x = x)
                         );
                    test "map_err acts as identity on Ok values"
                         (fun() ->
                           let x = Lwt_result.return 0 in
                           return (Lwt_result.map_err ((+) 1) x = x)
                         );
                    test "map_err acts on Error values"
                         (fun() ->
                           let x = Lwt_result.fail 0 in
                           let correct = Lwt_result.fail 1 in
                           return (Lwt_result.map_err ((+) 1) x = correct)
                         );
                  ]
