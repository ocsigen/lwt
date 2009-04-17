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

open Camlp4
open Camlp4.PreCast
open Syntax

(* Generate the catching function from a macth-case.

   The main work of this functions is to add a case:

   {[
     | exn -> fail exn
   ]}

   when there is not already one. *)
let gen_catch mc =
  (* Does the match case have a rule of the form "| e -> ..." ? *)
  let rec have_default = function
    | <:match_case< $a$ | $b$ >> -> have_default a || have_default b
    | <:match_case< _ -> $_$ >>
    | <:match_case< $lid:_$ -> $_$ >> -> true
    | _ -> false
  in
  if have_default mc then
    mc
  else
    let _loc = Ast.loc_of_match_case mc in
    <:match_case< $mc$ | exn -> Lwt.fail exn >>

EXTEND Gram
  GLOBAL: expr;

    cases:
      [ [ "with"; c = match_case -> Some(gen_catch c)
        | -> None ] ];

    finally:
      [ [ "finally"; f = sequence -> Some f
        | -> None ] ];

    expr: LEVEL "top"
      [ [ "try_lwt"; e = sequence; c = cases; f = finally ->
            begin match c, f with
              | None, None ->
                  Loc.raise _loc (Failure "``try_lwt'' blocks must have at least a ``with'' part or a ``finally'' part")
              | Some c, None ->
                  <:expr< Lwt.catch (fun _ -> $e$) (function $c$) >>
              | None, Some f ->
                  <:expr< Lwt.finalize (fun _ -> $e$) (fun _ -> $f$) >>
              | Some c, Some f ->
                  <:expr< Lwt.try_bind (fun _ -> $e$)
                            (fun __pa_lwt_x -> Lwt.bind (begin $f$ end) (fun () -> Lwt.return __pa_lwt_x))
                            (fun __pa_lwt_e -> Lwt.bind (begin $f$ end) (fun () -> match __pa_lwt_e with $c$))
                  >>
            end
        ] ];
END

(* Replace the anonymous bind [x >> y] by [x >>= fun _ -> y] *)
let map_anonymous_bind = object
  inherit Ast.map as super
  method expr e = match super#expr e with
    | <:expr@_loc< $lid:f$ $a$ $b$ >> when f = ">>" -> <:expr< bind $a$ (fun _ -> $b$) >>
    | e -> e
end

let _ =
  AstFilters.register_str_item_filter map_anonymous_bind#str_item;
  AstFilters.register_topphrase_filter map_anonymous_bind#str_item
