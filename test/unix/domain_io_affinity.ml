(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* [Lwt_unix.file_descr] and [Lwt_io.channel] belong to the domain that
   created them. The check is where every operation already passes:
   [check_descriptor] for descriptors, [primitive] and [atomic] for channels,
   plus the single-character fast paths, which bypass [primitive]. Also
   checks that another domain can do buffered I/O on a non-blocking descriptor
   of its own. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let contains needle m =
  let n = String.length needle in
  let rec go i =
    i + n <= String.length m && (String.sub m i n = needle || go (i + 1))
  in
  go 0

let refused f =
  match f () with
  | _ -> false
  | exception Invalid_argument m -> contains "belongs to another domain" m
  | exception _ -> false

let on_other_domain f = Domain.join (Domain.spawn f)

let () =
  let path = Filename.temp_file "lwt-io-affinity" ".txt" in
  let fd =
    Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false
      (Unix.openfile path [Unix.O_RDWR] 0o600)
  in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  let ic_fd =
    Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false
      (Unix.openfile path [Unix.O_RDONLY] 0o600)
  in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input ic_fd in
  (* Descriptors. *)
  check "reading a foreign descriptor is refused"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_unix.read fd (Bytes.create 4) 0 4)));
  check "writing a foreign descriptor is refused"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_unix.write fd (Bytes.of_string "x") 0 1)));
  check "closing a foreign descriptor is refused"
    (on_other_domain (fun () -> refused (fun () -> Lwt_unix.close fd)));
  (* A descriptor whose blocking mode is not known yet: the check must come
     before the guess, which would otherwise run on the other domain and leave
     its outcome to the domain that owns the descriptor. *)
  let unguessed, peer = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let unguessed = Lwt_unix.of_unix_file_descr unguessed in
  check "writing a foreign descriptor of unknown mode is refused"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_unix.write_string unguessed "x" 0 1)));
  check "and the descriptor still works for its domain"
    (match Lwt_main.run (Lwt_unix.write_string unguessed "x" 0 1) with
    | 1 -> true
    | _ -> false
    | exception _ -> false);
  Unix.close peer;
  (try Lwt_main.run (Lwt_unix.close unguessed) with _ -> ());
  (* Channels, through [primitive]. *)
  check "writing a foreign channel is refused"
    (on_other_domain (fun () -> refused (fun () -> Lwt_io.write oc "x")));
  check "flushing a foreign channel is refused"
    (on_other_domain (fun () -> refused (fun () -> Lwt_io.flush oc)));
  check "an atomic block on a foreign channel is refused"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_io.atomic (fun _ -> Lwt.return_unit) oc)));
  check "closing a foreign channel is refused"
    (on_other_domain (fun () -> refused (fun () -> Lwt_io.close oc)));
  (* Channels, through the single-character fast paths. The channel is idle
     and has room, so [write_char] never reaches [primitive], and a check
     placed only there would miss this case. *)
  check "write_char on a foreign channel is refused, fast path and all"
    (on_other_domain (fun () -> refused (fun () -> Lwt_io.write_char oc 'x')));
  check "read_char on a foreign channel is refused"
    (on_other_domain (fun () -> refused (fun () -> Lwt_io.read_char ic)));
  (* The consequence for the standard channels. *)
  check "Lwt_io.stdout belongs to the domain that initialized Lwt_io"
    (on_other_domain (fun () ->
       refused (fun () -> Lwt_io.write Lwt_io.stdout "")));
  (* The other domain makes its own channel, on a descriptor of its own in
     non-blocking mode: a blocking descriptor needs jobs, which only run on the
     domain that initialized Lwt_unix. *)
  check "another domain does its own buffered I/O"
    (on_other_domain (fun () ->
       let path' = Filename.temp_file "lwt-io-affinity-theirs" ".txt" in
       let their_fd =
         Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false
           (Unix.openfile path' [Unix.O_RDWR] 0o600)
       in
       let their_oc = Lwt_io.of_fd ~mode:Lwt_io.output their_fd in
       let ok =
         match
           Lwt_main.run
             (Lwt.bind (Lwt_io.write their_oc "theirs") (fun () ->
                Lwt_io.flush their_oc))
         with
         | () ->
             let ic = open_in_bin path' in
             let s = really_input_string ic (in_channel_length ic) in
             close_in ic; s = "theirs"
         | exception _ -> false
       in
       (try Sys.remove path' with _ -> ());
       ok));
  (* Ours still work here. *)
  check "ours still work here"
    (match
       Lwt_main.run
         (Lwt.bind (Lwt_io.write oc "ours") (fun () -> Lwt_io.flush oc))
     with
    | () -> true
    | exception _ -> false);
  (try Lwt_main.run (Lwt_io.close oc) with _ -> ());
  (try Lwt_main.run (Lwt_io.close ic) with _ -> ());
  (try Sys.remove path with _ -> ());
  if !failures > 0 then exit 1;
  print_endline "descriptors and channels bound to their domain: ok"
