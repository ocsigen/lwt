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
open Lwt.Infix

let (<=>) v v' =
  assert (Lwt.state v = v')

let test_iter f test_list =
  let incr_ x = Lwt.return (incr x) in
  let () =
    let l = [ref 0; ref 0; ref 0] in
    let t = f incr_ l in
    t <=> Lwt.Return ();
    List.iter2 (fun v r -> assert (v = !r)) [1; 1; 1] l
  in
  let () =
    let l = [ref 0; ref 0; ref 0] in
    let t, w = Lwt.wait () in
    let r = ref [incr_; (fun x -> t >>= (fun () -> incr_ x)); incr_] in
    let t' = f (fun x ->
      let f = List.hd !r in
      let t = f x in
      r := List.tl !r;
      t) l
    in
    t' <=> Sleep;
    List.iter2 (fun v r -> assert (v = !r)) test_list l;
    Lwt.wakeup w ();
    List.iter2 (fun v r -> assert (v = !r)) [1; 1; 1] l;
    t' <=> Lwt.Return ()
  in
  ()

let test_exception list_combinator =
  (* This really should be a local exception, but local exceptions require OCaml
     4.04, while Lwt still supports, and is tested on, 4.02. *)
  let module E =
    struct
      exception Exception
    end
  in
  let open E in

  let number_of_callback_calls = ref 0 in

  let callback _ =
    Pervasives.incr number_of_callback_calls;
    match !number_of_callback_calls with
    | 2 -> raise Exception
    | _ -> Lwt.return ()
  in

  (* Even though the callback will raise immediately for one of the list
     elements, we expect the final promise that represents the entire list
     operation to be created (and rejected with the raised exception). The
     raised exception should not be leaked up past the creation of the
     promise. *)
  let p =
    try
      list_combinator callback [(); (); ()]
    with _exn ->
      assert false
  in

  (* Check that the promise was rejected with the expected exception. *)
  assert (Lwt.state p = Lwt.Fail Exception)

let test_map f test_list =
  let t, w = Lwt.wait () in
  let t', _ = Lwt.task () in
  let get =
    let r = ref 0 in
    let c = ref 0 in
    fun () ->
      let th =
        incr c;
        match !c with
        | 5 -> t
        | 8 -> t'
        | _ -> Lwt.return ()
      in
      th >>= (fun () ->
        incr r;
        Lwt.return (!r))
  in
  let () =
    let l = [(); (); ()] in
    let t1 = f get l in
    t1 <=> Lwt.Return [1; 2; 3];
    let t2 = f get l in
    t2 <=> Lwt.Sleep;
    let t3 = f get l in
    t3 <=> Lwt.Sleep;
    Lwt.cancel t';
    t3 <=> Lwt.Fail Lwt.Canceled;
    Lwt.wakeup w ();
    t2 <=> Lwt.Return test_list;
  in
  ()

let test_map_parallelism ?(rev=false) map =
  let t, w = Lwt.wait () in
  let g x =
    Lwt.wakeup_later w ();
    Lwt.return (x + 1) in
  let f x =
    if x = 0 then t >>= (fun _ -> Lwt.return (x + 1))
    else g x
  in
  let p = map f [0; 1] in
  let expected = if rev then [2; 1] else [1; 2] in
  p >>= (fun l -> Lwt.return (l = expected))

let test_map_serialism map =
  let l = ref [] in
  let f x =
    let p = Lwt.pause () >>= fun _ -> Lwt.return x in
    assert (List.length !l = 0 || not @@ Lwt.is_sleeping @@ List.hd !l);
    l := p :: !l;
    p
  in
  let p' = map f [0; 1; 2] in
  p' >>= (fun _ -> Lwt.return_true)

let test_iter_parallelism iter =
  let t, w = Lwt.wait () in
  let g _ =
    Lwt.wakeup_later w ();
    Lwt.return () in
  let f x =
    if x = 0 then t >>= (fun _ -> Lwt.return ())
    else g x
  in
  let p = iter f [0; 1] in
  p >>= (fun _ -> Lwt.return true)

let test_iter_serialism iter =
  let l = ref [] in
  let f _ =
    let p = Lwt.pause () >>= fun _ -> Lwt.return () in
    assert (List.length !l = 0 || not @@ Lwt.is_sleeping @@ List.hd !l);
    l := p :: !l;
    p
  in
  let p' = iter f [0; 1; 2] in
  p' >>= (fun _ -> Lwt.return_true)

let test_fold_serialism fold =
  let l = ref [] in
  let f _ x =
    let p = Lwt.pause () >>= fun _ -> Lwt.return x in
    assert (List.length !l = 0 || not @@ Lwt.is_sleeping @@ List.hd !l);
    l := p :: !l;
    p
  in
  let p' = fold f 0 [0; 1; 2] in
  p' >>= (fun _ -> Lwt.return_true)

let test_quantifier_parallelism quantifier b =
  let t, w = Lwt.wait () in
  let g _ =
    Lwt.wakeup_later w ();
    Lwt.return b in
  let f x =
    if x = 0 then t >>= (fun _ -> Lwt.return b)
    else g x
  in
  let p = quantifier f [0; 1] in
  p >>= (fun u -> Lwt.return (u = b))

let test_quantifier_serialism quantifier b =
  let l = ref [] in
  let f _ =
    let p = Lwt.pause () >>= fun _ -> Lwt.return b in
    assert (List.length !l = 0 || not @@ Lwt.is_sleeping @@ List.hd !l);
    l := p :: !l;
    p
  in
  let p' = quantifier f [0; 1; 2] in
  p' >>= (fun _ -> Lwt.return_true)

let test_for_all_true f =
  let l = [true; true] in
  f (fun x -> Lwt.return (x = true)) l

let test_for_all_false f =
  let l = [true; true] in
  f (fun x -> Lwt.return (x = false)) l >>= fun b ->
  Lwt.return (not b)

let test_exists_true f =
  let l = [true; false] in
  f (fun x -> Lwt.return (x = true)) l >>= fun b ->
  Lwt.return b

let test_exists_false f =
  let l = [true; true] in
  f (fun x -> Lwt.return (x = false)) l >>= fun b ->
  Lwt.return (not b)

let test_filter f =
  let l = [1; 2; 3; 4] in
  f (fun x -> Lwt.return (x mod 2 = 0)) l >>= fun after ->
  Lwt.return (after = [2; 4])

let test_partition f =
  let l = [1; 2; 3; 4] in
  f (fun x -> Lwt.return (x <= 2)) l >>= fun (a, b) ->
  Lwt.return (a = [1; 2] && b = [3; 4])

let test_filter_map f =
  let l = [1; 2; 3; 4] in
  let fn = (fun x ->
    if x mod 2 = 0 then Lwt.return_some (x * 2) else Lwt.return_none) in
  f fn l >>= fun after ->
  Lwt.return (after = [4; 8])

let test_iter_i f =
  let count = ref 0 in
  let l = [1; 2; 3] in
  f (fun i n -> count := !count + i + n; Lwt.return_unit) l >>= fun () ->
  Lwt.return (!count = 9)

let test_map_i f =
  let l = [0; 0; 0] in
  f (fun i n -> Lwt.return (i + n)) l >>= fun after ->
  Lwt.return (after = [0; 1; 2])

let test_rev_map f =
  let l = [1; 2; 3] in
  f (fun n -> Lwt.return (n * 2)) l >>= fun after ->
  Lwt.return (after = [6; 4; 2])

let suite = suite "lwt_list" [
  test "iter_p" begin fun () ->
    test_iter Lwt_list.iter_p [1; 0; 1];
    test_exception Lwt_list.iter_p;
    Lwt.return true
  end;

  test "iter_s" begin fun () ->
    test_iter Lwt_list.iter_s [1; 0; 0];
    test_exception Lwt_list.iter_s;
    Lwt.return true
  end;

  test "map_p" begin fun () ->
    test_map Lwt_list.map_p [4; 8; 5];
    test_exception Lwt_list.map_p;
    Lwt.return true
  end;

  test "map_s" begin fun () ->
    test_map Lwt_list.map_s [4; 7; 8];
    test_exception Lwt_list.map_s;
    Lwt.return true
  end;

  test "fold_left_s" begin fun () ->
    let l = [1; 2; 3] in
    let f acc v = Lwt.return (v::acc) in
    let t = Lwt_list.fold_left_s f [] l in
    t <=> Lwt.Return (List.rev l);
    Lwt.return true
  end;

  test "for_all_s"
    (fun () -> test_for_all_true Lwt_list.for_all_s);

  test "for_all_p"
    (fun () -> test_for_all_true Lwt_list.for_all_p);

  test "for_all_s"
    (fun () -> test_for_all_false Lwt_list.for_all_s);

  test "for_all_p"
    (fun () -> test_for_all_false Lwt_list.for_all_p);

  test "exists_s true"
    (fun () -> test_exists_true Lwt_list.exists_s);

  test "exists_p true"
    (fun () -> test_exists_true Lwt_list.exists_p);

  test "exists_s false"
    (fun () -> test_exists_false Lwt_list.exists_s);

  test "exists_p false"
    (fun () -> test_exists_false Lwt_list.exists_p);

  test "filter_s"
    (fun () -> test_filter Lwt_list.filter_s);

  test "filter_p"
    (fun () -> test_filter Lwt_list.filter_p);

  test "partition_p"
    (fun () -> test_partition Lwt_list.partition_p);

  test "partition_s"
    (fun () -> test_partition Lwt_list.partition_s);

  test "filter_map_p"
    (fun () -> test_filter_map Lwt_list.filter_map_p);

  test "filter_map_s"
    (fun () -> test_filter_map Lwt_list.filter_map_s);

  test "iteri_p"
    (fun () -> test_iter_i Lwt_list.iteri_p);

  test "iteri_s"
    (fun () -> test_iter_i Lwt_list.iteri_s);

  test "mapi_p"
    (fun () -> test_map_i Lwt_list.mapi_p);

  test "mapi_s"
    (fun () -> test_map_i Lwt_list.mapi_s);

  test "find_s existing" begin fun () ->
    let l = [1; 2; 3] in
    Lwt_list.find_s (fun n -> Lwt.return ((n mod 2) = 0)) l >>= fun result ->
    Lwt.return (result = 2)
  end;

  test "find_s missing" begin fun () ->
    let l = [1; 3] in
    Lwt.catch
      (fun () ->
        Lwt_list.find_s (fun n ->
          Lwt.return ((n mod 2) = 0)) l >>= fun _result ->
        Lwt.return false)
      (function
        | Not_found -> Lwt.return true
        | _ -> Lwt.return false)
  end;

  test "rev_map_p"
    (fun () -> test_rev_map Lwt_list.rev_map_p);

  test "rev_map_s"
    (fun () -> test_rev_map Lwt_list.rev_map_s);

  test "fold_right_s" begin fun () ->
    let l = [1; 2; 3] in
    Lwt_list.fold_right_s (fun a n -> Lwt.return (a + n)) l 0 >>= fun result ->
    Lwt.return (result = 6)
  end;

  test "iteri_p exception" begin fun () ->
    let i f = Lwt_list.iteri_p (fun _ x -> f x) in
    test_exception i;
    Lwt.return true
  end;

  test "iteri_s exception" begin fun () ->
    let i f = Lwt_list.iteri_s (fun _ x -> f x) in
    test_exception i;
    Lwt.return true
  end;

  test "map_s exception" begin fun () ->
    test_exception Lwt_list.map_s;
    Lwt.return true
  end;

  test "map_p exception" begin fun () ->
    test_exception Lwt_list.map_p;
    Lwt.return true
  end;

  test "mapi_s exception" begin fun () ->
    let m f = Lwt_list.mapi_s (fun _ x -> f x) in
    test_exception m;
    Lwt.return true
  end;

  test "mapi_p exception" begin fun () ->
    let m f = Lwt_list.mapi_p (fun _ x -> f x) in
    test_exception m;
    Lwt.return true
  end;

  test "rev_map_s exception" begin fun () ->
    test_exception Lwt_list.rev_map_s;
    Lwt.return true
  end;

  test "rev_map_p exception" begin fun () ->
    test_exception Lwt_list.rev_map_p;
    Lwt.return true
  end;

  test "fold_left_s exception" begin fun () ->
    let m f = Lwt_list.fold_left_s (fun _ x -> f x) () in
    test_exception m;
    Lwt.return true
  end;

  test "fold_right_s exception" begin fun() ->
    let m f l = Lwt_list.fold_right_s (fun x _ -> f x) l () in
    test_exception m;
    Lwt.return true
  end;

  test "for_all_p exception" begin fun () ->
    let m f = Lwt_list.for_all_p (fun x -> f x >>= (fun _ -> Lwt.return_true)) in
    test_exception m;
    Lwt.return true
  end;

  test "for_all_s exception" begin fun () ->
    let m f = Lwt_list.for_all_s (fun x -> f x >>= (fun _ -> Lwt.return_true)) in
    test_exception m;
    Lwt.return true
  end;

  test "exists_p exception" begin fun () ->
    let m f = Lwt_list.exists_p (fun x -> f x >>= (fun _ -> Lwt.return_false)) in
    test_exception m;
    Lwt.return true
  end;

  test "exists_s exception" begin fun () ->
    let m f = Lwt_list.exists_s (fun x -> f x >>= (fun _ -> Lwt.return_false)) in
    test_exception m;
    Lwt.return true
  end;

  test "find_s exception" begin fun () ->
    let m f = Lwt_list.find_s (fun x -> f x >>= (fun _ -> Lwt.return_false)) in
    test_exception m;
    Lwt.return true
  end;

  test "filter_p exception" begin fun () ->
    let m f = Lwt_list.filter_p (fun x -> f x >>= (fun _ -> Lwt.return_false)) in
    test_exception m;
    Lwt.return true;
  end;

  test "filter_s exception" begin fun () ->
    let m f = Lwt_list.filter_s (fun x -> f x >>= (fun _ -> Lwt.return_false)) in
    test_exception m;
    Lwt.return true;
  end;

  test "filter_map_p exception" begin fun () ->
    let m f = Lwt_list.filter_map_p (fun x -> f x >>= (fun _ -> Lwt.return (Some ()))) in
    test_exception m;
    Lwt.return true;
  end;

  test "filter_map_s exception" begin fun () ->
    let m f = Lwt_list.filter_map_s (fun x -> f x >>= (fun _ -> Lwt.return (Some ()))) in
    test_exception m;
    Lwt.return true;
  end;

  test "partition_p exception" begin fun () ->
    let m f = Lwt_list.partition_p (fun x -> f x >>= (fun _ -> Lwt.return_false)) in
    test_exception m;
    Lwt.return true;
  end;

  test "partition_s exception" begin fun () ->
    let m f = Lwt_list.partition_s (fun x -> f x >>= (fun _ -> Lwt.return_false)) in
    test_exception m;
    Lwt.return true;
  end;

  test "iter_p parallelism" begin fun () ->
    test_iter_parallelism Lwt_list.iter_p
  end;

  test "iter_s serialism" begin fun () ->
    test_iter_serialism Lwt_list.iter_s
  end;

  test "iteri_p parallelism" begin fun () ->
    let iter f = Lwt_list.iteri_p (fun _ x -> f x) in
    test_iter_parallelism iter
  end;

  test "iteri_s serialism" begin fun () ->
    let iter f = Lwt_list.iteri_s (fun _ x -> f x) in
    test_iter_serialism iter
  end;

  test "map_p parallelism" begin fun () ->
    test_map_parallelism Lwt_list.map_p
  end;

  test "map_s serialism" begin fun () ->
    test_map_serialism Lwt_list.map_s
  end;

  test "mapi_p parallelism" begin fun () ->
    let m f = Lwt_list.mapi_p (fun _ x -> f x) in
    test_map_parallelism m
  end;

  test "mapi_s serialism" begin fun () ->
    let m f = Lwt_list.mapi_s (fun _ x -> f x) in
    test_map_serialism m
  end;

  test "rev_map_p parallelism" begin fun () ->
    test_map_parallelism ~rev:true Lwt_list.rev_map_p
  end;

  test "rev_map_s serialism" begin fun () ->
    test_map_serialism Lwt_list.rev_map_s
  end;

  test "fold_left_s serialism" begin fun () ->
    test_fold_serialism Lwt_list.fold_left_s
  end;

  test "fold_right_s serialism" begin fun () ->
    let fold f s l = Lwt_list.fold_right_s f l s in
    test_fold_serialism fold
  end;

  test "filter_map_p parallelism" begin fun () ->
    let m f = Lwt_list.filter_map_p (fun x -> f x >>= fun u -> Lwt.return (Some u)) in
    test_map_parallelism m
  end;

  test "filter_map_s serlialism" begin fun () ->
    let m f = Lwt_list.filter_map_s (fun x -> f x >>= fun u -> Lwt.return (Some u)) in
    test_map_serialism m
  end;

  test "for_all_p parallelism" begin fun () ->
    test_quantifier_parallelism Lwt_list.for_all_p true
  end;

  test "for_all_s serialism" begin fun () ->
    test_quantifier_serialism Lwt_list.for_all_s true
  end;

  test "exists_p parallelism" begin fun () ->
    test_quantifier_parallelism Lwt_list.exists_p false
  end;

  test "exists_s serialism" begin fun () ->
    test_quantifier_serialism Lwt_list.exists_s false
  end;

  test "find_s serialism" begin fun () ->
    let l = ref [] in
    let f x =
      let p = Lwt.pause () >>= fun _ -> Lwt.return (x = 2) in
      assert (List.length !l = 0 || not @@ Lwt.is_sleeping @@ List.hd !l);
      l := p :: !l;
      p
    in
    let p' = Lwt_list.find_s f [0; 1; 2] in
    p' >>= (fun _ -> Lwt.return_true)
  end;

  test "filter_p parallelism" begin fun () ->
    let t, w = Lwt.wait () in
    let g _ =
      Lwt.wakeup_later w ();
      Lwt.return true in
    let f x =
      if x = 0 then t >>= (fun _ -> Lwt.return true)
      else g x
    in
    let p = Lwt_list.filter_p f [0; 1] in
    p >>= (fun l -> Lwt.return (l = [0; 1]))
  end;

  test "filter_s serialism" begin fun () ->
    let l = ref [] in
    let f _ =
      let p = Lwt.pause () >>= fun _ -> Lwt.return_true in
      assert (List.length !l = 0 || not @@ Lwt.is_sleeping @@ List.hd !l);
      l := p :: !l;
      p
    in
    let p' = Lwt_list.filter_s f [0; 1; 2] in
    p' >>= (fun _ -> Lwt.return_true)
  end;

  test "filter_map_p parallelism" begin fun () ->
    let map f = Lwt_list.filter_map_p (fun x -> f x >>= fun u -> Lwt.return (Some u)) in
    test_map_parallelism map
  end;

  test "filter_map_s serialism" begin fun () ->
    let map f = Lwt_list.filter_map_s (fun x -> f x >>= fun u -> Lwt.return (Some u)) in
    test_map_serialism map
  end;

  test "partition_p parallelism" begin fun () ->
    let t, w = Lwt.wait () in
    let g _ =
      Lwt.wakeup_later w ();
      Lwt.return_true in
    let f x =
      if x = 0 then t >>= (fun _ -> Lwt.return_true)
      else g x
    in
    let p = Lwt_list.partition_p f [0; 1] in
    p >>= (fun _ -> Lwt.return_true)
  end;

  test "partition_s serialism" begin fun () ->
    let l = ref [] in
    let f _ =
      let p = Lwt.pause () >>= fun _ -> Lwt.return_true in
      assert (List.length !l = 0 || not @@ Lwt.is_sleeping @@ List.hd !l);
      l := p :: !l;
      p
    in
    let p' = Lwt_list.partition_s f [0; 1; 2] in
    p' >>= (fun _ -> Lwt.return_true)
  end;
]
