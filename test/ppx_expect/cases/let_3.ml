let _ =
  let%lwt () = Lwt.return_unit and () = Lwt.return 5 in
  Lwt.return_unit;;
