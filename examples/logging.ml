
let () =
  (* Enable informative messages: *)
  Lwt_log.set_level `Info true;

  (* A message with the default logger: *)
  Lwt_log.log ~level:`Info "this message will appear only on stderr";

  (* Same as begore, but using the syntax extension: *)
  Log#info "this one too";

  (* A message to a custom logger, logging simultaneously to [stderr]
     and to the system logger daemon: *)
  let logger = Lwt_log.merge [Lwt_log.channel ~close_mode:`keep ~channel:Lwt_io.stderr ~pid:false ~date:false ();
                              Lwt_log.syslog ()] in
  Lwt_log.log ~logger ~level:`Info "this message will appear on stderr and in '/var/log/user.log'";

  (* Logging of exceptions: *)
  Printexc.record_backtrace true;
  let f () : unit = raise Exit in
  let g () = f () in
  let h () = g () in
  try
    h ();
  with exn ->
    Log#exn exn "h failed with"
