(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Lwt.Syntax

open Test

let l =  [1; 2; 3; 4; 5]

let suite_base = suite "lwt_seq" [
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

let fs = [(+); (-); (fun x _ -> x); (fun _ x -> x); min; max]
let ls = [
   [];
   [0;1;2;3;4;5];
   [0;0;0];
   [max_int;0;min_int];
   [max_int;max_int];
]
let cs = [0;1;max_int;min_int;44;5]
let with_flc test =
   Lwt_list.for_all_s
     (fun f ->
        Lwt_list.for_all_s
          (fun l ->
             Lwt_list.for_all_s
               (fun c -> test f l c)
               cs)
          ls)
     fs
let equals l1 seq2 =
   let* l2 = Lwt_seq.to_list seq2 in
   Lwt.return (l1 = l2)
let commutes lf sf l =
   equals (lf l) (sf (Lwt_seq.of_list l))


let suite_fuzzing = suite "lwt_seq(pseudo-fuzzing)" [
  test "map" begin fun () ->
     with_flc (fun f l c ->
        let lf = List.map (fun x -> f x c) in
        let sf = Lwt_seq.map (fun x -> f x c) in
        commutes lf sf l
     )
  end;
  test "map_s" begin fun () ->
     with_flc (fun f l c ->
        let lf = List.map (fun x -> f x c) in
        let sf = Lwt_seq.map_s (fun x -> Lwt.return (f x c)) in
        commutes lf sf l
     )
  end;
  test "iter" begin fun () ->
     with_flc (fun f l c ->
        let lf l =
           let r = ref c in
           List.iter (fun x -> r := f x !r) l;
           [!r] in
        let sf s =
           let r = ref c in
           fun () ->
             let* () = Lwt_seq.iter (fun x -> r := f x !r) s in
             Lwt.return (Lwt_seq.Cons (!r, Lwt_seq.empty)) in
        commutes lf sf l
     )
  end;
  test "iter_s" begin fun () ->
     with_flc (fun f l c ->
        let lf l =
           let r = ref c in
           List.iter (fun x -> r := f x !r) l;
           [!r] in
        let sf s =
           let r = ref c in
           fun () ->
             let* () = Lwt_seq.iter_s (fun x -> r := f x !r; Lwt.return_unit) s in
             Lwt.return (Lwt_seq.Cons (!r, Lwt_seq.empty)) in
        commutes lf sf l
     )
  end;
]
