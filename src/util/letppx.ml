let split_version = String.split_on_char '.' Sys.ocaml_version
let letppx =
  match split_version with
  | "4" :: _ | "5" :: "0" :: _ -> false
  | "5" :: _ -> true
  | _ ->
      print_endline ("unrecognised version " ^ Sys.ocaml_version);
      exit 1
let () = Printf.printf "letppx=%B\n" letppx

let direct =
  match split_version with
  | "4" :: _ -> false
  | "5" :: _ -> true
  | _ ->
      print_endline ("unrecognised version " ^ Sys.ocaml_version);
      exit 1
let () = Printf.printf "direct=%B\n" direct

let runtime_events =
  match split_version with
  | "4" :: _ | "5" :: ("0"|"1"|"2"|"3") :: _ -> false
  | "5" :: _ -> true
  | _ ->
      print_endline ("unrecognised version " ^ Sys.ocaml_version);
      exit 1
let () = Printf.printf "runtime_events=%B\n" runtime_events
