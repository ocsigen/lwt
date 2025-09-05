open Lwt.Syntax

let checks = Atomic.make 0

let () = Lwt_unix.init_domain ()

let write w s =
  let b = Bytes.unsafe_of_string s in
  let* l = Lwt_unix.write w b 0 (Bytes.length b) in
  assert (l = Bytes.length b);
  Lwt.return_unit

let read r n =
  let b = Bytes.create n in
  let* l = Lwt_unix.read r b 0 n in
  assert (l = n);
  Lwt.return (Bytes.unsafe_to_string b)

let rec run data w r =
  let* () = Lwt.pause () in
  match data with
  | [] -> Lwt.return_unit
  | datum::data ->
    let* () = write w datum in
    let* readed = read r (String.length datum) in
    assert (datum = readed);
    Atomic.incr checks;
    run data w r

let run_in_domain data w r = Domain.spawn (fun () -> Lwt_main.run (run data w r))

let (a_from_b, b_to_a) = Lwt_unix.pipe ()
let (b_from_a, a_to_b) = Lwt_unix.pipe ()
let data = [ "aaa"; "bbbb"; "alhskjdflkhjasdflkhjhjklasfdlhjksadxf" ]

let a2b = run_in_domain data a_to_b a_from_b
let b2a = run_in_domain data b_to_a b_from_a

let () = Domain.join a2b
let () = Domain.join b2a
let () =
  if Atomic.get checks = 2 * List.length data then begin
    Printf.printf "unixpipe: ✓\n";
    exit 0
  end else begin
    Printf.printf "unixpipe: ×\n";
    exit 1
  end
