
let () =
  (* Enable all logging levels superior from [Info] to [Fatal]: *)
  Lwt_log.set_level !Lwt_log.default Lwt_log.Info;

  (* A message with the default logger: *)
  Lwt_log.log ~level:Lwt_log.Info "this message will appear only on stderr";

  (* Same as begore, but using the syntax extension: *)
  Log#info "this one too";

  (* A message to a custom logger, logging simultaneously to [stderr]
     and to the system logger daemon: *)
  let logger = Lwt_log.merge ~level:Lwt_log.Info
    [Lwt_log.channel ~level:Lwt_log.Info ~close_mode:`keep ~channel:Lwt_io.stderr ();
     Lwt_log.syslog  ~level:Lwt_log.Info ~facility:`User ()] in
  Lwt_log.log ~logger ~level:Lwt_log.Info "this message will appear on stderr and in '/var/log/user.log'";

  (* Logging of exceptions: *)
  Printexc.record_backtrace true;
  let f () : unit = raise Exit in
  let g () = f () in
  let h () = g () in
  try
    h ();
  with exn ->
    Log#exn exn "h failed with"
