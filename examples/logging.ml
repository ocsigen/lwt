
let () = Lwt_main.run begin

  (* Enabled informative messages: *)
  Lwt_log.set_level !Lwt_log.default `Info true;

  (* A message with the default logger: *)
  Lwt_log.log ~level:`Info "this message will appear only on stderr";

  (* A message to a custom logger, logging simultaneously to [stderr]
     and to the system logger daemon: *)
  lwt logger = Lwt_log.create [Lwt_log.dest_stderr; Lwt_log.dest_syslog ()] in
  Lwt_log.log ~logger ~level:`Info "this message will appear on stderr and in '/var/log/user.log'";
  Lwt.return ()
end
