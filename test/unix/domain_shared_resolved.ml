(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* A resolved promise has no owner, and can be used on any domain. Immutable
   data in it needs no protection. A mutex, stream, channel or other value
   bound to its domain still refuses to be used on another domain. An ordinary
   mutable value is not protected, and the test checks that too, to document
   the limitation. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let on_other_domain f = Domain.join (Domain.spawn f)

let refused f =
  match f () with
  | _ -> false
  | exception Invalid_argument _ -> true
  | exception Lwt.Foreign_promise -> true
  | exception _ -> false

let () =
  (* 1. A resolved promise carrying data can be used on any domain. This is
     how a computed constant is shared between loops. *)
  let answer = Lwt.return 42 in
  check "a resolved promise carrying data is readable anywhere"
    (on_other_domain (fun () ->
       match Lwt.state answer with Lwt.Return 42 -> true | _ -> false));
  check "and binding it elsewhere works too"
    (on_other_domain (fun () ->
       match Lwt.state (Lwt.bind answer (fun v -> Lwt.return (v + 1))) with
       | Lwt.Return 43 -> true
       | _ -> false));
  (* 2. A resolved promise carrying a channel can also be used on any domain,
     but using the channel there is refused. *)
  let path = Filename.temp_file "lwt-shared" ".txt" in
  let fd =
    Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false
      (Unix.openfile path [Unix.O_WRONLY] 0o600)
  in
  let channel = Lwt.return (Lwt_io.of_fd ~mode:Lwt_io.output fd) in
  check "the promise carrying a channel crosses"
    (on_other_domain (fun () ->
       match Lwt.state channel with Lwt.Return _ -> true | _ -> false));
  check "but using that channel elsewhere is refused"
    (on_other_domain (fun () ->
       match Lwt.state channel with
       | Lwt.Return oc -> refused (fun () -> Lwt_io.write oc "x")
       | _ -> false));
  (* 3. An ordinary mutable value in a resolved promise is shared with no
     error. *)
  let counter = Lwt.return (ref 0) in
  check "a mutable value in a resolved promise is not protected"
    (on_other_domain (fun () ->
       match Lwt.state counter with
       | Lwt.Return r ->
           (* No exception: nothing protects this value. *)
           incr r; !r = 1
       | _ -> false));
  (try Lwt_main.run (Lwt_unix.close fd) with _ -> ());
  (try Sys.remove path with _ -> ());
  if !failures > 0 then exit 1;
  print_endline "values shared through a resolved promise: ok"
