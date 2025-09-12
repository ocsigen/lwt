let () =
  match String.split_on_char '.' Sys.ocaml_version with
  | "4" :: _ | "5" :: "0" :: _ -> print_endline "letppx=false"
  | "5" :: _ -> print_endline "letppx=true"
  | _ ->
      print_endline ("unrecognised version " ^ Sys.ocaml_version);
      exit 1
