(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Lwt.Syntax

open Test

let l =  [1; 2; 3; 4; 5]

let suite = suite "lwt_seq" [
  test "list" begin fun () ->
    let a = Lwt_seq.of_list l in
    let n = ref 1 in
    Lwt_seq.fold_left (fun acc x ->
      let r = x = !n && acc in
      incr n; Lwt.return r) true a
  end;


  test "filter" begin fun () ->
    let a = Lwt_seq.of_list l in
    let v = Lwt_seq.filter (fun x -> Lwt.return (x mod 2 = 0)) a in
    let+ l' = Lwt_seq.to_list v in
    l' = [2; 4]
  end;
]
