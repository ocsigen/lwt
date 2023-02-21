let _ =
  match%lwt
    Lwt.return_unit
  with
  | () ->
    5;;
