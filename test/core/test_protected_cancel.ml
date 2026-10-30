(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* The callback that [Lwt.protected] attaches to [p] can run after the mirror
   [Lwt.protected p] has been canceled: when [Lwt.cancel] reaches both, or when
   a cancel callback of the mirror resolves [p]. It must then leave the mirror
   alone. Resolving it crashed the program, hence a standalone test. *)

let check name ok =
  if not ok
  then begin
    prerr_endline ("FAILED: " ^ name);
    exit 1
  end

let canceled p = Lwt.state p = Lwt.Fail Lwt.Canceled

let () =
  (* The mirror itself. *)
  let p, _ = Lwt.task () in
  let p' = Lwt.protected p in
  Lwt.cancel (Lwt.join [p'; p]);
  check "join: both canceled" (canceled p && canceled p');
  (* The same through [Lwt.choose]. *)
  let p, _ = Lwt.task () in
  let p' = Lwt.protected p in
  let c = Lwt.choose [p'; p] in
  Lwt.cancel c;
  check "choose: all canceled" (canceled p && canceled p' && canceled c);
  (* The mirror has become a proxy: the promise it stands for is the one
     canceled. *)
  let p, _ = Lwt.task () in
  let p' = Lwt.protected p in
  let start, start_resolver = Lwt.wait () in
  let outer = Lwt.bind start (fun () -> p') in
  Lwt.wakeup start_resolver ();
  Lwt.cancel (Lwt.join [outer; p]);
  check "proxy: both canceled" (canceled p && canceled outer);
  (* A cancel callback of the mirror resolves [p]. *)
  let p, r = Lwt.wait () in
  let p' = Lwt.protected p in
  Lwt.on_cancel p' (fun () -> Lwt.wakeup r 1);
  Lwt.cancel p';
  check "on_cancel: p resolved, mirror canceled"
    (Lwt.state p = Lwt.Return 1 && canceled p');
  (* A cancel callback, added to a promise before the mirror becomes its
     proxy, resolves [p]. *)
  let p, r = Lwt.wait () in
  let p' = Lwt.protected p in
  let start, start_resolver = Lwt.wait () in
  let outer = Lwt.bind start (fun () -> p') in
  Lwt.on_cancel outer (fun () -> Lwt.wakeup r 1);
  Lwt.wakeup start_resolver ();
  Lwt.cancel outer;
  check "on_cancel through a proxy: p resolved, mirror canceled"
    (Lwt.state p = Lwt.Return 1 && canceled outer);
  (* Nested mirrors. *)
  let p, _ = Lwt.task () in
  let p' = Lwt.protected p in
  let p'' = Lwt.protected p' in
  Lwt.cancel (Lwt.join [p''; p'; p]);
  check "nested: all canceled" (canceled p && canceled p' && canceled p'');
  print_endline "cancel of a protected promise and its source: ok"
