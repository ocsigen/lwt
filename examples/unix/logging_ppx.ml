(** A ppx rewrite of the logging.ml example compile with:
    ocamlfind ocamlopt -package lwt.ppx,lwt.unix -g logging_ppx.ml -linkpkg -o T *)

let section = Lwt_log.Section.make "test"

let%lwt () =
  (* Enable all logging levels superior from [Info] to [Fatal]: *)
  Lwt_log.Section.set_level section Lwt_log.Info;

  (* A message with the default logger: *)
  let%lwt () =
    Lwt_log.log
      ~section
      ~level:Lwt_log.Info
      "this message will appear only on stderr"
  in

  (* Same as begore, but using [Lwt_log.info]: *)
  let%lwt () = Lwt_log.info ~section "this one too" in

  (* A message to a custom logger, logging simultaneously to [stderr]
     and to the system logger daemon: *)
  let logger =
    Lwt_log.broadcast
      [Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stderr ();
       Lwt_log.syslog ~facility:`User ()]
  in
  let%lwt () =
    Lwt_log.info
      ~section
      ~logger
      "this message will appear on stderr and in '/var/log/user.log'"
  in

  (* Logging of exceptions: *)
  Printexc.record_backtrace true;
  let f () : unit = raise Exit in
  let g () = f () in
  let h () = g () in
  let%lwt () =
    try
      h ();
      Lwt.return ()
    with exn ->
      Lwt_log.error ~section ~exn "h failed with"
  in

  let logger =
    Lwt_log.channel
      ~template:"$(name): $(section): $(loc-file): $(loc-line): $(loc-column): $(message)"
      ~close_mode:`Keep
      ~channel:Lwt_io.stderr ()
  in
  Lwt_log.info ~section ~logger "this message will appear with a location"
