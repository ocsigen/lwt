(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Ocamlbuild plugin
 * Copyright (C) 2008 Stéphane Glondu
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later
 * version.
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

open Ocamlbuild_plugin

let ocamlfind_mt x = S[A"ocamlfind"; A x; A"-thread"; A"-package"; A"ssl"];;
let ocamlfind x = S[A"ocamlfind"; A x; A"-package"; A"ssl"];;

dispatch begin function

  | Before_options ->
      Options.make_links := false;
      Options.ocamlc := ocamlfind_mt "ocamlc";
      Options.ocamlopt := ocamlfind_mt "ocamlopt";
      (* There seem to be no other way to specify ocamldoc... *)
      Options.ocamldoc := ocamlfind "ocamldoc";

  | After_rules ->
      copy_rule "%.mllib -> %.odocl" "%.mllib" "%.odocl";

  | _ -> ()
end
