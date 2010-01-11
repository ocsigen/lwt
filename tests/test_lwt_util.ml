open Test
open Lwt
open Lwt_util

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
  let t',w' = task () in
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
       test_iter iter [1;0;1];
       test_exception iter;
       return true);

  test "1"
    (fun () ->
       test_iter iter_serial [1;0;0];
       test_exception iter;
       return true);

  test "2"
    (fun () ->
       test_map map [4;8;5];
       test_exception map;
       return true);

  test "3"
    (fun () ->
       test_map map_serial [4;7;8];
       test_exception map_serial;
       return true);

  test "4"
    (fun () ->
       let l = [1;2;3] in
       let f acc v = return (v::acc) in
       let t = fold_left f [] l in
       t <=> Return (List.rev l);
       return true);

  (* XXX l'espace semble mal compte dans les regions: on peut lancer
     un thread tant que l'espace n'est pas nul, ca ne prends pas en
     compte la taille du thread.  ca devrait bloquer si il n'y a pas
     assez de place. De plus resize region devrait permetre de
     reveiller des threads.

     Une maniere de corriger est de ne pas permetre aux threads de
     faire une taille superieur a 1. *)

  test "5"
    (fun () ->
       let t1,w1 = wait () in
       let t2,w2 = wait () in
       let t3,w3 = task () in
       let region = make_region 3 in
       run_in_region region 1 return <=> Return ();
       (* XXX ne devrait pas pouvoir se lancer *)
       run_in_region region 4 return <=> Return ();
       let a = run_in_region region 3 (fun () -> t1) in
       a <=> Sleep;
       let b = run_in_region region 1 return in
       b <=> Sleep;
       let c = run_in_region region 3 (fun () -> t2) in
       c <=> Sleep;
       let d = run_in_region region 1 return in
       d <=> Sleep;
       let e = run_in_region region 3 (fun () -> t3) in
       e <=> Sleep;
       let f = run_in_region region 1 return in
       f <=> Sleep;
       wakeup w1 ();
       a <=> Return ();
       b <=> Return ();
       c <=> Sleep;
       d <=> Sleep;
       e <=> Sleep;
       f <=> Sleep;
       cancel t3;
       e <=> Sleep;
       f <=> Sleep;
       wakeup w2 ();
       c <=> Return ();
       d <=> Return ();
       e <=> Fail Canceled;
       f <=> Return ();
       return true);

  test "6"
    (fun () ->
       let f () = raise Exn in
       let region = make_region 1 in
       run_in_region region 1 f <=> Fail Exn;
       run_in_region region 1 return <=> Return ();
       return true);
]

(* XXX le comportement souhaite devrait etre:
   ( avec resize qui renvoie un lwt qui se reveille
     quand il y a suffisement de resources libres )
*)
(*
let () =
  let region = make_region 1 in
  run_in_region region 1 return <=> Return ();
  let t = run_in_region region 2 return in
  t <=> Sleep;
  resize_region region 2 <=> Return ();
  t <=> Return ();
  let t,w = wait () in
  let t = run_in_region region 2 (fun () -> t) in
  t <=> Sleep;
  let t2 = run_in_region region 2 return in
  let t3 = resize_region region 1 in
  t2 <=> Sleep;
  t3 <=> Sleep;
  wakeup w ();
  t <=> Return ();
  t3 <=> Return ();
  t2 <=> Sleep
*)

(* XXX ca ne gere pas les cancel non plus *)
