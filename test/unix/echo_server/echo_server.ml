

let spf = Printf.sprintf

let str_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr addr) port

let main ~port () : unit Lwt.t =
  let fut, _prom = Lwt.wait () in

  (* TODO: handle exit?? *)
  Printf.printf "listening on port %d\n%!" port;

  let handle_client _client_addr (ic,oc) =

    let buf = Bytes.create 32 in
    let continue = ref true in
    while !continue do
      let n = Lwt.await @@ Lwt_io.read_into ic buf 0 (Bytes.length buf) in
      if n = 0 then
        continue := false
      else (
        Lwt.await @@ Lwt_io.write_from_exactly oc buf 0 n;
        Lwt.await @@ Lwt_io.flush oc
      )
    done;
    Lwt.return ()
  in

  let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let _server = Lwt_io.establish_server_with_client_address addr handle_client in

  fut

let () =
  let port = ref 0 in
  let j = ref 4 in

  let opts =
    [
      "-p", Arg.Set_int port, " port"; "-j", Arg.Set_int j, " number of threads";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo server";

  Lwt_main.run @@ main ~port:!port ()
