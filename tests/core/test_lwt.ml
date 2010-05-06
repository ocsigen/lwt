open Test
open Lwt

let ( <=> ) v v' =
  assert ( state v = v')

let test_exn f v e =
  assert ( try f v;assert false with exn -> exn = e)

let f x = return ("test"^x)
let g x = ("test"^x)

exception Exn

let suite = suite "lwt" [
  test "0"
    (fun () ->
       return "test" <=> Return "test";
       fail Exn <=> Fail Exn;
       bind (return "test") f <=> Return "testtest";
       bind (fail Exn) return <=> Fail Exn;
       (return "test") >>= f <=> Return "testtest";
       f =<< (return "test") <=> Return "testtest";
       map g (return "test") <=> Return "testtest";
       (return "test") >|= g <=> Return "testtest";
       g =|< (return "test") <=> Return "testtest";
       return true);

  test "1"
    (fun () ->
       catch return (fun e -> return ()) <=> Return ();
       catch (fun () -> fail Exn) (function Exn -> return ()| e -> assert false) <=> Return ();
       catch (fun () -> fail Exn) (fun e -> fail e) <=> Fail Exn;
       return true);

  test "2"
    (fun () ->
       try_bind return return ( fun e -> assert false ) <=> Return ();
       try_bind (fun () -> fail Exn) return (function Exn -> return ()| e -> assert false) <=> Return ();
       return true);

  test "3"
    (fun () ->
       finalize return return <=> Return ();
       finalize (fun () -> fail Exn) return <=> Fail Exn;
       return true);

  test "4"
    (fun () ->
       apply (fun () -> raise Exn) () <=> Fail Exn;
       return true);

  test "5"
    (fun () ->
       choose [return ()] <=> Return ();
       return () <?> return () <=> Return ();
       return true);

  test "6"
    (fun () ->
       join [return ()] <=> Return ();
       return () <&> return () <=> Return ();
       return true);

  test "7"
    (fun () ->
       assert (ignore_result (return ()) = ());
       test_exn ignore_result (fail Exn) Exn;
       return true);

  test "8"
    (fun () ->
       let t,w = wait () in
       t <=> Sleep;
       wakeup w ();
       t <=> Return ();
       return true);

  test "9"
    (fun () ->
       let t,w = wait () in
       wakeup_exn w Exn;
       t <=> Fail Exn;
       return true);

  test "10"
    (fun () ->
       let t,w = task () in
       t <=> Sleep;
       wakeup w ();
       t <=> Return ();
       return true);

  test "11"
    (fun () ->
       let t,w = wait () in
       let r1 = choose [t] in r1 <=> Sleep;
       choose [t;return ()] <=> Return ();
       join [fail Exn;t] <=> Fail Exn;
       let r2 = join [t] in r2 <=> Sleep;
       let r3 = join [t;return ()] in r3 <=> Sleep;
       wakeup w ();
       r1 <=> Return (); r2 <=> Return (); r2 <=> Return ();
       return true);

  test "12"
    (fun () ->
       let t,w = wait () in
       let t',w' = wait () in
       let r1 = join [return ();t] in
       let r2 = join [t;t'] in
       wakeup_exn w Exn;
       r1 <=> Fail Exn;
       r2 <=> Fail Exn;
       return true);

  test "13"
    (fun () ->
       let t,w = wait () in
       let t',w' = wait () in
       let r = bind (choose [t;t']) return in
       r <=> Sleep;
       wakeup w' ();
       r <=> Return ();
       let r' = bind (choose [t;t]) return in
       wakeup w ();
       r' <=> Return ();
       return true);

  test "14"
    (fun () ->
       assert ( poll (return ()) = Some () );
       test_exn poll (fail Exn) Exn;
       let t,w = wait () in
       assert ( poll t = None );
       return true);

  test "15"
    (fun () ->
       let t,w = wait () in
       assert ( ignore_result t = () );
       wakeup w ();
       let t,w = wait () in
       ignore_result t;
       (* XXX c'est quand meme un comportement bizare *)
       test_exn (wakeup_exn w) Exn Exn;
       return true);

  test "16"
    (fun () ->
       let t,w = wait () in
       let r1 = catch (fun () -> t) (fun e -> return ()) in r1 <=> Sleep;
       let r2 = try_bind (fun () -> t) return ( fun e -> assert false ) in r2 <=> Sleep;
       wakeup w ();
       r1 <=> Return ();
       r2 <=> Return ();
       return true);

  (****)

  test "17"
    (fun () ->
       let t,w = task () in
       let t',w' = wait () in
       let t'' = return () in
       cancel t;
       cancel t';
       cancel t'';
       t <=> Fail Canceled;
       t' <=> Sleep;
       t'' <=> Return ()  ;
       return true);

  test "18"
    (fun () ->
       let t,w = task () in
       let r = bind t return in
       cancel r;
       r <=> Fail Canceled;
       return true);

  test "19"
    (fun () ->
       let t,w = task () in
       on_cancel t (fun () -> ());
       on_cancel t (fun () -> raise Exn);
       on_cancel (return ()) (fun () -> assert false);
       cancel t;
       on_cancel t (fun () -> ());
       let t,w = wait () in
       on_cancel t (fun () -> ());
       wakeup w ();
       return true);

  test "20"
    (fun () ->
       let t,w = task () in
       let t',w' = wait () in
       let r = pick [t;t'] in r <=> Sleep;
       wakeup w' ();
       r <=> Return ();
       t <=> Fail Canceled;
       return true);

  test "21"
    (fun () ->
       pick [return ()] <=> Return ();
       return true);

  test "22"
    (fun () ->
       let t,w = task () in
       let t',w' = wait () in
       let r = pick [t;t'] in
       cancel r;
       r <=> Fail Canceled;
       t <=> Fail Canceled;
       return true);

  test "23"
    (fun () ->
       let t,w = task () in
       let r = join [t] in
       cancel r;
       r <=> Fail Canceled;
       t <=> Fail Canceled;
       return true);

  test "24"
    (fun () ->
       let t,w = task () in
       let r = choose [t] in
       cancel r;
       r <=> Fail Canceled;
       t <=> Fail Canceled;
       return true);

  test "25"
    (fun () ->
       let t,w = task () in
       let r = catch (fun () -> t) (function Canceled -> return ()| _ -> assert false) in
       cancel r;
       r <=> Return ();
       t <=> Fail Canceled;
       return true);

  test "26"
    (fun () ->
       let t,w = task () in
       let r = try_bind (fun () -> t) (fun _ -> assert false) (function Canceled -> return ()| _ -> assert false) in
       cancel r;
       r <=> Return ();
       t <=> Fail Canceled;
       return true);

  test "27"
    (fun () ->
       let t,w = wait () in
       wakeup w ();
       test_exn (wakeup w) () (Invalid_argument "Lwt.wakeup");
       return true);

  test "28"
    (fun () ->
       let t,w = task () in
       cancel t;
       wakeup w ();
       return true);

  test "29"
    (fun () ->
       let t,w = wait () in
       let t',w' = wait () in
       let r = bind t ( fun () -> t' ) in
       let r' = bind t ( fun () -> r ) in
       wakeup w ();
       r <=> Sleep;
       r' <=> Sleep;
       wakeup w' ();
       r <=> Return ();
       r' <=> Return ();
       return true);

  test "30"
    (fun () ->
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
       r <=> Return ();
       return true);

  test "31"
    (fun () ->
       let t,w = wait () in
       let a = ref (return ()) in
       let r = bind t ( fun () -> !a ) in
       a := r;
       wakeup w ();
       return true);

  test "choose"
    (fun () ->
       let t1,w1 = wait () in
       let t2,w2 = wait () in
       let rec f = function
	 | 0 -> []
	 | i -> (choose [t1;t2])::(f (i-1))
       in
       let l = f 100 in
       t1 <=> Sleep;
       t2 <=> Sleep;
       List.iter (fun t -> t <=> Sleep) l;
       wakeup w1 ();
       List.iter (fun t -> t <=> Return ()) l;
       t1 <=> Return ();
       t2 <=> Sleep;
       return true);

  test "protected return"
    (fun () ->
       let t = return 1 in
       let t' = protected t in
       return ((state t' = Return 1) && (state t = Return 1)));

  test "protected fail"
    (fun () ->
       let t = fail Exn in
       let t' = protected t in
       return ((state t' = Fail Exn) && (state t = Fail Exn)));

  test "protected wait 1"
    (fun () ->
       let t,w = wait () in
       let t' = protected t in
       wakeup w 1;
       return ((state t' = Return 1) && (state t = Return 1)));

  test "protected wait 2"
    (fun () ->
       let t,w = wait () in
       let t' = protected t in
       wakeup_exn w Exn;
       return ((state t' = Fail Exn) && (state t = Fail Exn)));

  test "protected wait 3"
    (fun () ->
       let t,w = wait () in
       let t' = protected t in
       cancel t';
       return ((state t' = Fail Canceled) && (state t = Sleep)));

  test "protected wait 4"
    (fun () ->
       let t,w = wait () in
       let t' = protected t in
       cancel t';
       wakeup w 1;
       return ((state t' = Fail Canceled) && (state t = Return 1)));

  test "protected wait 5"
    (fun () ->
       let t,w = wait () in
       let t' = protected t in
       cancel t';
       wakeup_exn w Exn;
       return ((state t' = Fail Canceled) && (state t = Fail Exn)));

  test "protected wait 6"
    (fun () ->
       let t,w = wait () in
       let t' = protected t in
       wakeup_exn w Exn;
       cancel t';
       return ((state t' = Fail Exn) && (state t = Fail Exn)));

  test "protected wait 7"
    (fun () ->
       let t,w = wait () in
       let t' = protected t in
       wakeup w 1;
       cancel t';
       return ((state t' = Return 1) && (state t = Return 1)));

  test "join 1"
    (fun () ->
       let t1 = fail Exn in
       let t2 = join [t1] in
       return ((state t1 = Fail Exn) && (state t2 = Fail Exn)));

  test "join 2"
    (fun () ->
       let t1,w1 = wait () in
       let t2 = join [t1] in
       wakeup_exn w1 Exn;
       return ((state t1 = Fail Exn) && (state t2 = Fail Exn)));

  test "join 3"
    (fun () ->
       let t1 = fail Exn in
       let t2,w2 = wait () in
       let t3 = fail Not_found in
       let t4 = join [t2;t1;t3] in
       return ((state t1 = Fail Exn) && (state t2 = Sleep) &&
		 (state t3 = Fail Not_found) && (state t4 = Fail Exn)));

  test "join 4"
    (fun () ->
       let t1 = fail Exn in
       let t2,w2 = wait () in
       let t3 = return () in
       let rec f = function
	 | 0 -> return true
	 | i ->
	     let t = join [t2;t3;t1] in
	     if ((state t1 = Fail Exn) && (state t2 = Sleep)
		 && (state t = Fail Exn) && (state t3 = Return ()))
	     then f (i-1)
	     else return false
       in
       f 100);

  test "cancel loop"
    (fun () ->
       let rec loop () =
         lwt () = Lwt_unix.yield () in
         loop ()
       in
       let t = loop () in
       cancel t;
       return (state t = Fail Canceled));

  test "cancel loop 2"
    (fun () ->
       let rec loop () =
         lwt () = Lwt_unix.yield () in
         loop ()
       in
       let t = loop () in
       lwt () = Lwt_unix.yield () in
       cancel t;
       return (state t = Fail Canceled));

  test "nchoose"
    (fun () ->
       lwt l = nchoose [return 1; return 2] in
       return (l = [1; 2]));

  test "npick"
    (fun () ->
       lwt l = npick [return 1; return 2] in
       return (l = [1; 2]));

  test "bind/cancel 1"
    (fun () ->
       let waiter, wakener = wait () in
       let t =
         lwt () = waiter in
         let waiter, wakener = task () in
         waiter
       in
       wakeup wakener ();
       cancel t;
       return (state t = Fail Canceled));

  test "bind/cancel 2"
    (fun () ->
       let waiter, wakener = wait () in
       let t =
         lwt () = waiter in
         let waiter, wakener = task () in
         waiter
       in
       let t = t >>= return in
       wakeup wakener ();
       cancel t;
       return (state t = Fail Canceled));
]
