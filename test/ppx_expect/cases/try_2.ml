let _ =
  try%lwt
    Lwt.return_unit
  with _ -> 5;;
