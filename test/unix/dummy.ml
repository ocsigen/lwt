let () =
  let str = "the quick brown fox jumps over the lazy dog" in
  match Sys.argv.(1) with
  | "read" -> if read_line () <> str then exit 1
  | "write" -> print_string str
  | _ -> invalid_arg "Sys.argv"
