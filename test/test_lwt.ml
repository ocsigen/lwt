open Lwt

let test v v' =
  assert ( state v = v')

let test_exn f v e =
  assert ( try f v;assert false with exn -> exn = e)

let ( <=> ) v v' = test v v'

let f x = return ("test"^x)
let g x = ("test"^x)

exception Exn

let () =
  return "test" <=> Return "test";
  fail Exn <=> Fail Exn;
  bind (return "test") f <=> Return "testtest";
  bind (fail Exn) return <=> Fail Exn;
  (return "test") >>= f <=> Return "testtest";
  f =<< (return "test") <=> Return "testtest";
  map g (return "test") <=> Return "testtest";
  (return "test") >|= g <=> Return "testtest";
  g =|< (return "test") <=> Return "testtest"

let () =
  catch return (fun e -> return ()) <=> Return ();
  catch (fun () -> fail Exn) (function Exn -> return ()| e -> assert false) <=> Return ();
  catch (fun () -> fail Exn) (fun e -> fail e) <=> Fail Exn;

  try_bind return return ( fun e -> assert false ) <=> Return ();
  try_bind (fun () -> fail Exn) return (function Exn -> return ()| e -> assert false) <=> Return ();

  finalize return return <=> Return ();
  finalize (fun () -> fail Exn) return <=> Fail Exn

let () =
  apply (fun () -> raise Exn) () <=> Fail Exn

let () =
  choose [return ()] <=> Return ();
  return () <?> return () <=> Return ()

let () =
  join [return ()] <=> Return ();
  return () <&> return () <=> Return ()

let () =
  assert (ignore_result (return ()) = ());
  test_exn ignore_result (fail Exn) Exn

let () =
  let t,w = wait () in
  t <=> Sleep;
  wakeup w ();
  t <=> Return ()

let () =
  let t,w = wait () in
  wakeup_exn w Exn;
  t <=> Fail Exn

let () =
  let t,w = task () in
  t <=> Sleep;
  wakeup w ();
  t <=> Return ()

let () =
  let t,w = wait () in
  let r1 = choose [t] in r1 <=> Sleep;
  choose [t;return ()] <=> Return ();
  join [fail Exn;t] <=> Fail Exn;
  let r2 = join [t] in r2 <=> Sleep;
  let r3 = join [t;return ()] in r3 <=> Sleep;
  wakeup w ();
  r1 <=> Return (); r2 <=> Return (); r2 <=> Return ()

let () =
  let t,w = wait () in
  let t',w' = wait () in
  let r1 = join [return ();t] in
  let r2 = join [t;t'] in
  wakeup_exn w Exn;
  r1 <=> Fail Exn;
  r2 <=> Fail Exn

let () =
  let t,w = wait () in
  let t',w' = wait () in
  let r = bind (choose [t;t']) return in
  r <=> Sleep;
  wakeup w' ();
  r <=> Return ();
  let r' = bind (choose [t;t]) return in
  wakeup w ();
  r' <=> Return ()

let () =
  assert ( poll (return ()) = Some () );
  test_exn poll (fail Exn) Exn;
  let t,w = wait () in
  assert ( poll t = None )

let () =
  let t,w = wait () in
  assert ( ignore_result t = () );
  wakeup w ();
  let t,w = wait () in
  ignore_result t;
  (* XXX c'est quand meme un comportement bizare *)
  test_exn (wakeup_exn w) Exn Exn

let () =
  let t,w = wait () in
  let r1 = catch (fun () -> t) (fun e -> return ()) in r1 <=> Sleep;
  let r2 = try_bind (fun () -> t) return ( fun e -> assert false ) in r2 <=> Sleep;
  wakeup w ();
  r1 <=> Return ();
  r2 <=> Return ()

(****)

let () =
  let t,w = task () in
  let t',w' = wait () in
  let t'' = return () in
  cancel t;
  cancel t';
  cancel t'';
  t <=> Fail Canceled;
  t' <=> Sleep;
  t'' <=> Return ()  

let () =
  let t,w = task () in
  let r = bind t return in
  cancel r;
  r <=> Fail Canceled

let () =
  let t,w = task () in
  on_cancel t (fun () -> ());
  on_cancel t (fun () -> raise Exn);
  on_cancel (return ()) (fun () -> assert false);
  cancel t;
  on_cancel t (fun () -> ());
  let t,w = wait () in
  on_cancel t (fun () -> ());
  wakeup w ()

let () =
  let t,w = task () in
  let t',w' = wait () in
  let r = select [t;t'] in r <=> Sleep;
  wakeup w' ();
  r <=> Return ();
  t <=> Fail Canceled

let () =
  select [return ()] <=> Return ()

let () =
  let t,w = task () in
  let t',w' = wait () in
  let r = select [t;t'] in
  cancel r;
  r <=> Fail Canceled;
  t <=> Fail Canceled

let () =
  let t,w = task () in
  let r = join [t] in
  cancel r;
  r <=> Fail Canceled;
  t <=> Fail Canceled

let () =
  let t,w = task () in
  let r = choose [t] in
  cancel r;
  r <=> Fail Canceled;
  t <=> Fail Canceled

let () =
  let t,w = task () in
  let r = catch (fun () -> t) (function Canceled -> return ()| _ -> assert false) in
  cancel r;
  r <=> Return ();
  t <=> Fail Canceled

let () =
  let t,w = task () in
  let r = try_bind (fun () -> t) (fun _ -> assert false) (function Canceled -> return ()| _ -> assert false) in
  cancel r;
  r <=> Return ();
  t <=> Fail Canceled

let () =
  let t,w = wait () in
  wakeup w ();
  test_exn (wakeup w) () (Invalid_argument "wakeup")

let () =
  let t,w = task () in
  cancel t;
  wakeup w ()

let () =
  let t,w = wait () in
  let t',w' = wait () in
  let r = bind t ( fun () -> t' ) in
  let r' = bind t ( fun () -> r ) in
  wakeup w ();
  r <=> Sleep;
  r' <=> Sleep;
  wakeup w' ();
  r <=> Return ();
  r' <=> Return ()

let () =
  let t,w = wait () in
  let t',w' = wait () in
  let t'',w'' = wait () in
  let r = bind t ( fun () -> t' ) in
  let r' = bind t'' ( fun () -> r ) in
  wakeup w'' ();
  r <=> Sleep;
  r' <=> Sleep;
  wakeup w ();
  wakeup w' ();
  r' <=> Return ();
  r <=> Return ()

let () =
  let t,w = wait () in
  let a = ref (return ()) in
  let r = bind t ( fun () -> !a ) in
  a := r;
  wakeup w ()


let fact n =
  let rec aux acc = function
    | 0 -> return acc
    | n -> bind (return (n-1)) (aux (n*acc))
  in
  aux 1 n

(*
let () =
  (* Will normaly not overflow and shouldn't take memory *)
  ignore (fact 10000000)
*)

(*
let rec fact = function
  | 0 -> return 1
  | n -> bind (fact (n-1)) (fun x -> return (x*n))

let () =
  (* Will normaly overflow *)
  ignore (fact 10000000)
*)
