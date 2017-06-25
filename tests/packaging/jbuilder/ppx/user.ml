let () =
  let p = Lwt.return () >> Lwt.return () in
  ignore p
