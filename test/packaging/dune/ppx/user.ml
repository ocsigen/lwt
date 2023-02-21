let () =
  let p =
    let%lwt () = Lwt.return_unit in
    Lwt.return_unit
  in
  ignore p
