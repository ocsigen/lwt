(* OCaml promise library
 * http://www.ocsigen.org/lwt
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

let test_for_all_true f =
  let l = [true;true] in
  f (fun x -> return (x = true)) l

let test_for_all_false f =
  let l = [true;true] in
  f (fun x -> return (x = false)) l >>= fun b ->
  return (not b)

let test_exists_true f =
  let l = [true;false] in
  f (fun x -> return (x = true)) l >>= fun b ->
  return b

let test_exists_false f =
  let l = [true;true] in
  f (fun x -> return (x = false)) l >>= fun b ->
  return (not b)

let test_filter f =
  let l = [1;2;3;4] in
  f (fun x -> return (x mod 2 = 0)) l >>= fun after ->
  return (after = [2;4])

let test_partition f =
  let l = [1;2;3;4] in
  f (fun x -> return (x <= 2)) l >>= fun (a, b) ->
  return (a = [1;2] && b = [3;4])

let test_filter_map f =
  let l = [1;2;3;4] in
  let fn = (fun x -> if x mod 2 = 0 then return_some (x * 2) else return_none) in
  f fn l >>= fun after ->
  return (after = [4;8])

let test_iter_i f =
  let count = ref 0 in
  let l = [1;2;3] in
  f (fun i n -> count := !count + i + n; return_unit) l >>= fun () ->
  return (!count = 9)

let test_map_i f =
  let l = [0;0;0] in
  f (fun i n -> return (i + n)) l >>= fun after ->
  return (after = [0;1;2])

let test_rev_map f =
  let l = [1;2;3] in
  f (fun n -> return (n*2)) l >>= fun after ->
  return (after = [6;4;2])

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

    test "5"
      (fun () -> test_for_all_true Lwt_list.for_all_s);

    test "6"
      (fun () -> test_for_all_true Lwt_list.for_all_p);

    test "7"
      (fun () -> test_for_all_false Lwt_list.for_all_s);

    test "8"
      (fun () -> test_for_all_false Lwt_list.for_all_p);

    test "9"
      (fun () -> test_exists_true Lwt_list.exists_s);

    test "10"
      (fun () -> test_exists_true Lwt_list.exists_p);

    test "11"
      (fun () -> test_exists_false Lwt_list.exists_s);

    test "12"
      (fun () -> test_exists_false Lwt_list.exists_p);

    test "13"
      (fun () -> test_filter Lwt_list.filter_s);

    test "14"
      (fun () -> test_filter Lwt_list.filter_p);

    test "15"
      (fun () -> test_partition Lwt_list.partition_p);

    test "16"
      (fun () -> test_partition Lwt_list.partition_s);

    test "17"
      (fun () -> test_filter_map Lwt_list.filter_map_p);

    test "18"
      (fun () -> test_filter_map Lwt_list.filter_map_s);

    test "19"
      (fun () -> test_iter_i Lwt_list.iteri_p);

    test "20"
      (fun () -> test_iter_i Lwt_list.iteri_s);

    test "21"
      (fun () -> test_map_i Lwt_list.mapi_p);

    test "22"
      (fun () -> test_map_i Lwt_list.mapi_s);

    test "23"
      (
        fun () ->
          let l = [1;2;3] in
          Lwt_list.find_s (fun n -> return ((n mod 2) = 0)) l >>= fun result ->
          return (result = 2)
      );

    test "24"
      (
        fun () ->
          let l = [1;3] in
          catch
            (fun () -> Lwt_list.find_s (fun n -> return ((n mod 2) = 0)) l >>= fun result ->
              return false)
            (function
              | Not_found -> return true
              | _ -> return false)
      );

    test "25"
      (fun () -> test_rev_map Lwt_list.rev_map_p);

    test "26"
      (fun () -> test_rev_map Lwt_list.rev_map_s);

    test "27"
      (fun () ->
         let l = [1;2;3] in
         Lwt_list.fold_right_s (fun a n -> return (a + n)) l 0 >>= fun result ->
         return (result = 6)
      );

  ]
