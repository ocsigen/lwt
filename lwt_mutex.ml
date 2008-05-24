type t = { mutable locked : bool; mutable waiting : unit Lwt.t list  }

let create () = { locked = false; waiting = [] }

let rec lock m =
  if m.locked then begin
    let res = Lwt.wait () in
    m.waiting <- res :: m.waiting;
    Lwt.bind res (fun () ->
    lock m)
  end else begin
    m.locked <- true;
    Lwt.return ()
  end

let unlock m =
  let w = m.waiting in
  m.waiting <- [];
  m.locked <- false;
  List.iter (fun t -> Lwt.wakeup t ()) w
