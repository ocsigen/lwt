(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

(* The standard channels from every loop: Lwt_io.stdout and the others stand
   for one channel per domain over the same descriptor, flushed when that
   domain exits. Standard output goes to a file meanwhile, so that
   what each domain printed can be read back. Needs other domains, hence
   OCaml 5. *)

let failures = ref 0

let check name b =
  if not b
  then (
    Printf.eprintf "FAILED: %s\n" name;
    incr failures)

let lines = 20

(* Prints from a loop of its own, with neither a flush nor a close: what is
   printed must come out when the domain exits. *)
let printer name () =
  Lwt_main.run
    (let rec go i =
       if i = lines
       then Lwt.return_unit
       else
         Lwt.bind (Lwt_io.printf "%s io %d\n" name i) (fun () ->
           Lwt.bind (Lwt_fmt.printf "%s fmt %d@." name i) (fun () -> go (i + 1)))
     in
     go 0)

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic; s

let () =
  ignore (Unix.alarm 30);
  let path = Filename.temp_file "lwt_std" ".out" in
  let saved = Unix.dup Unix.stdout in
  let file = Unix.openfile path [Unix.O_WRONLY; Unix.O_TRUNC] 0o600 in
  Unix.dup2 file Unix.stdout;
  Unix.close file;
  let raised = Atomic.make None in
  let guard name f () =
    try f ()
    with exn ->
      Atomic.set raised (Some (name ^ ": " ^ Printexc.to_string exn))
  in
  let a = Domain.spawn (guard "A" (printer "A")) in
  let b = Domain.spawn (guard "B" (printer "B")) in
  printer "M" ();
  Domain.join a;
  Domain.join b;
  (* Closing another domain's standard output leaves the descriptor open. *)
  Domain.join
    (Domain.spawn
       (guard "C" (fun () -> Lwt_main.run (Lwt_io.close Lwt_io.stdout))));
  Lwt_main.run (Lwt_io.printf "after\n");
  Lwt_main.run (Lwt_io.flush Lwt_io.stdout);
  Unix.dup2 saved Unix.stdout;
  Unix.close saved;
  let out = read_file path in
  Sys.remove path;
  check
    (match Atomic.get raised with
    | None -> "no domain raised"
    | Some e -> "no domain raised, but " ^ e)
    (Atomic.get raised = None);
  let all = String.split_on_char '\n' out in
  let has l = List.mem l all in
  List.iter
    (fun name ->
       for i = 0 to lines - 1 do
         check
           (Printf.sprintf "%s io %d is printed, whole" name i)
           (has (Printf.sprintf "%s io %d" name i));
         check
           (Printf.sprintf "%s fmt %d is printed, whole" name i)
           (has (Printf.sprintf "%s fmt %d" name i))
       done)
    ["A"; "B"; "M"];
  check "the main domain prints after another closed its stdout" (has "after");
  (* And standard input, from another loop. *)
  let input = Filename.temp_file "lwt_std" ".in" in
  let oc = open_out_bin input in
  output_string oc "first\nsecond\n";
  close_out oc;
  let saved_in = Unix.dup Unix.stdin in
  let fd = Unix.openfile input [Unix.O_RDONLY] 0 in
  Unix.dup2 fd Unix.stdin;
  Unix.close fd;
  let line =
    Domain.join
      (Domain.spawn (fun () ->
         try Lwt_main.run (Lwt_io.read_line Lwt_io.stdin)
         with exn -> "raised " ^ Printexc.to_string exn))
  in
  Unix.dup2 saved_in Unix.stdin;
  Unix.close saved_in;
  Sys.remove input;
  check
    (Printf.sprintf "another loop reads standard input (%S)" line)
    (line = "first");
  (* Only the main domain may change a shared standard descriptor: its mode and
     what it names apply to the whole process. *)
  let refused f =
    Domain.join
      (Domain.spawn (fun () ->
         match f () with () -> false | exception Lwt.Domain_crossing -> true))
  in
  check "another domain cannot change the mode of stdout"
    (refused (fun () -> Lwt_unix.set_blocking Lwt_unix.stdout true));
  check "another domain cannot dup2 onto stdout"
    (refused (fun () ->
       let fd = Lwt_unix.of_unix_file_descr (Unix.dup Unix.stdout) in
       Fun.protect
         ~finally:(fun () -> Unix.close (Lwt_unix.unix_file_descr fd))
         (fun () -> Lwt_unix.dup2 fd Lwt_unix.stdout)));
  (* The main domain redirects standard output to a descriptor whose mode Lwt
     has not guessed yet: another domain must still be able to write to it. *)
  let redirected = Filename.temp_file "lwt_std" ".dup2" in
  let saved_out = Unix.dup Unix.stdout in
  let f =
    Lwt_unix.of_unix_file_descr
      (Unix.openfile redirected [Unix.O_WRONLY; Unix.O_TRUNC] 0o600)
  in
  Lwt_unix.dup2 f Lwt_unix.stdout;
  let wrote =
    Domain.join
      (Domain.spawn (fun () ->
         match Lwt_main.run (Lwt_unix.write_string Lwt_unix.stdout "z" 0 1) with
         | n -> n = 1
         | exception _ -> false))
  in
  Unix.dup2 saved_out Unix.stdout;
  Unix.close saved_out;
  Lwt_unix.set_blocking Lwt_unix.stdout true;
  Lwt_main.run (Lwt_unix.close f);
  Sys.remove redirected;
  check "another domain writes to a stdout redirected by dup2" wrote;
  (* The main domain may make standard output non-blocking. Another domain
     that would then have to wait for it gets a clear error, rather than
     registering an event the main domain's loop would run. Standard output is
     a full non-blocking pipe meanwhile. *)
  let r, w = Unix.pipe () in
  Unix.set_nonblock w;
  let chunk = Bytes.make 4096 'x' in
  (try
     while true do
       ignore (Unix.write w chunk 0 4096)
     done
   with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ());
  let saved_out = Unix.dup Unix.stdout in
  Unix.dup2 w Unix.stdout;
  Lwt_unix.set_blocking Lwt_unix.stdout false;
  check "another domain cannot wait for a non-blocking stdout"
    (refused (fun () ->
       Lwt_main.run
         (Lwt.pick
            [ Lwt.map ignore (Lwt_unix.write_string Lwt_unix.stdout "y" 0 1)
            ; Lwt_unix.sleep 2. ])));
  Unix.dup2 saved_out Unix.stdout;
  Unix.close saved_out;
  Lwt_unix.set_blocking Lwt_unix.stdout true;
  Unix.close w;
  Unix.close r;
  if !failures > 0 then exit 1;
  print_endline "the standard channels from every loop: ok"
