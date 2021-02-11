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

  test "map" begin fun () ->
    let a = Lwt_seq.of_list l in
    let v = Lwt_seq.map (fun x -> Lwt.return (x * 2)) a in
    let+ l' = Lwt_seq.to_list v in
    l' = [2; 4; 6; 8; 10]
  end;

  test "filter" begin fun () ->
    let a = Lwt_seq.of_list l in
    let v = Lwt_seq.filter (fun x -> Lwt.return (x mod 2 = 0)) a in
    let+ l' = Lwt_seq.to_list v in
    l' = [2; 4]
  end;

  test "filter_map" begin fun () ->
    let a = Lwt_seq.of_list l in
    let v = Lwt_seq.filter_map (fun x ->
      Lwt.return (if x mod 2 = 0 then Some (x * 2) else None)) a
    in
    let+ l' = Lwt_seq.to_list v in
    l' = [4; 8]
  end;

  test "unfold" begin fun () ->
    let range first last =
      let step i = if i > last then Lwt.return_none
                   else Lwt.return_some (i, succ i) in
      Lwt_seq.unfold step first
    in
    let* a = Lwt_seq.to_list (range 1 3) in
    let+ b = Lwt_seq.to_list (range 1 0) in
      ([1;2;3] = a) &&
      ([] = b)
  end;

  test "exception" begin fun () ->
    let fail = fun () ->
      let () = failwith "XXX" in
      Seq.Nil
    in
    let seq = fun () -> Seq.Cons (1, (fun () -> Seq.Cons (2, fail))) in
    let a = Lwt_seq.of_seq seq in
    let+ n =
      try
        Lwt_seq.fold_left(fun acc i ->
          Lwt.return (acc + i)
        ) 0 a
      with Failure x when x = "XXX" ->
        Lwt.return (-1)
    in
    n = (-1)
  end;

  test "exception of_seq_lwt" begin fun () ->
    let fail = fun () ->
      let () = failwith "XXX" in
      Seq.Nil
    in
    let seq: int Lwt.t Seq.t = fun () ->
      Seq.Cons (Lwt.return 1,
        fun () ->
          Seq.Cons (Lwt.return 2, fail)) in
    let* a = Lwt_seq.of_seq_lwt seq in
    let+ n =
      try
        Lwt_seq.fold_left(fun acc i ->
          Lwt.return (acc + i)
        ) 0 a
      with Failure x when x = "XXX" ->
        Lwt.return 0
    in
    n = 0
  end;
]
