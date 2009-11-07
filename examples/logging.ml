
let () = Lwt_main.run begin

  (* Enable informative messages: *)
  Lwt_log.set_level ~logger:!Lwt_log.default `Info true;

  (* A message with the default logger: *)
  Lwt_log.log ~level:`Info "this message will appear only on stderr";

  (* Same as begore, but using the syntax extension: *)
  Log#info "this one too";

  (* A message to a custom logger, logging simultaneously to [stderr]
     and to the system logger daemon: *)
  lwt logger = Lwt_log.create [Lwt_log.dest_stderr; Lwt_log.dest_syslog ()] in
  Lwt_log.log ~logger ~level:`Info "this message will appear on stderr and in '/var/log/user.log'";

  (* Logging d'une exception: *)
  Printexc.record_backtrace true;
  let f () = raise Exit in
  let g () = f () in
  let h () = g () in
  begin try
    h ()
  with exn ->
    Log#exn exn "h failed with"
  end;

  Lwt.return ()
end
