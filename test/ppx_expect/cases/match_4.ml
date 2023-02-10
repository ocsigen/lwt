let _ =
  match%lwt
    Lwt.return_unit
  with
  | () ->
    Lwt.return_unit
  | exception End_of_file ->
    Lwt.return 5
