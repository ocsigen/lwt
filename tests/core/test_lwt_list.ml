(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Test_lwt_util
 * Copyright (C) 2009 Jérémie Dimino, Pierre Chambart
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

let ( <=> ) v v' =
  assert ( state v = v')

let test_exn f v e =
  assert ( try f v;assert false with exn -> exn = e)

exception Exn

let test_iter f test_list =
  let incr_ x = return ( incr x ) in
  let () =
    let l = [ref 0;ref 0; ref 0] in
    let t = f incr_ l in
    t <=> Return ();
    List.iter2 (fun v r -> assert (v = !r)) [1;1;1] l
  in
  let () =
    let l = [ref 0;ref 0; ref 0] in
    let t,w = wait () in
    let r = ref [incr_;(fun x -> t >>= ( fun () -> incr_ x ));incr_] in
    let t' = f (fun x ->
		  let f = List.hd !r in
		  let t = f x in
		  r := List.tl !r;
		  t ) l
    in
    t' <=> Sleep;
    List.iter2 (fun v r -> assert (v = !r)) test_list l;
    wakeup w ();
    List.iter2 (fun v r -> assert (v = !r)) [1;1;1] l;
    t' <=> Return ()
  in
  ()

let test_exception f =
  let g =
    let r = ref 0 in
    fun _ ->
      incr r;
      match !r with
	| 2 -> raise Exn
	| _ -> return ()
  in
  (* XXX est-ce le comportement souhaite ?
     On pourrait plutot vouloir que iter et map
     passent leur fonctions en parametre dans Lwt.apply.

     Une autre maniere serait d'avoir 2 bind, un tail recursif un non.
  *)
  test_exn (f g) [();();()] Exn

let test_map f test_list =
  let t,w = wait () in
  let t', _ = task () in
  let get =
    let r = ref 0 in
    let c = ref 0 in
    fun () ->
      let th =
	incr c;
	match !c with
	  | 5 -> t
	  | 8 -> t'
	  | _ -> return ()
      in
      th >>= ( fun () ->
		 incr r;
		 return (!r) )
  in
  let () =
    let l = [();();()] in
    let t1 = f get l in
    t1 <=> Return [1;2;3];
    let t2 = f get l in
    t2 <=> Sleep;
    let t3 = f get l in
    t3 <=> Sleep;
    cancel t';
    t3 <=> Fail Canceled;
    wakeup w ();
    t2 <=> Return test_list;
  in
  ()

let suite = suite "lwt_util" [
  test "0"
    (fun () ->
       test_iter Lwt_list.iter_p [1;0;1];
       test_exception Lwt_list.iter_p;
       return true);

  test "1"
    (fun () ->
       test_iter Lwt_list.iter_s [1;0;0];
       test_exception Lwt_list.iter_s;
       return true);

  test "2"
    (fun () ->
       test_map Lwt_list.map_p [4;8;5];
       test_exception Lwt_list.map_p;
       return true);

  test "3"
    (fun () ->
       test_map Lwt_list.map_s [4;7;8];
       test_exception Lwt_list.map_s;
       return true);

  test "4"
    (fun () ->
       let l = [1;2;3] in
       let f acc v = return (v::acc) in
       let t = Lwt_list.fold_left_s f [] l in
       t <=> Return (List.rev l);
       return true);
]
