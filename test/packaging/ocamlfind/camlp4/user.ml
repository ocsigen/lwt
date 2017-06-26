let () =
  let p =
    lwt () = Lwt.return () in
    Lwt.return ()
  in
  ignore p
