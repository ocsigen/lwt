let _ =
  try%lwt
    Lwt.return_unit
  with _ -> Lwt.return 5;;
