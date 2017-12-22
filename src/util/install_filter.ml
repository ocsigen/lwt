(* By default jbuilder will create sub-directories for each sub-project
   in the main lwt library.  This is probably preferred overall, however,
   we would like to avoid this change for (at least) 1 release cycle so
   it can be announced first.

   This script will be run after jbuilder and modify the generated
   lwt.install file to remove the sub-directories.

   This works in concert with removing (actually commenting out for now)
   the "directory = ..." clauses in META.lwt.template.

   Note that we could create a .install file by hand, however,
   we would then have to deal with configuration options (ie does
   camlp4,unix,threads etc exist).  This scheme lets jbuilder work
   all that out for us.
*)

let filter_sub_path path s =
  try
    (* look for the string *)
    let start = String.index s '{' in
    let end_ = String.index_from s start '/' in
    if String.sub s (start + 2) (end_ - start - 2) = path then
      (* remove the path bit *)
      let ofs = start + 3 + String.length path in
      String.sub s 0 (start + 2) ^ String.sub s ofs (String.length s - ofs)
    else
      s
  with _ -> s

let sub_paths = [
  "unix";
  "preemptive";
  "log";
  "options";
  "syntax";
  "ppx";
]

let filter_sub_paths s = List.fold_right filter_sub_path sub_paths s

let main () =
  let read_line file = try Some(input_line file) with End_of_file -> None in
  let rec read_lines file =
    match read_line file with
    | Some(x) -> x :: read_lines file
    | None -> []
  in
  let file = open_in "lwt.install" in
  let lines = List.map filter_sub_paths (read_lines file) in
  let () = close_in file in
  let file = open_out "lwt.install" in
  let () = List.iter (Printf.fprintf file "%s\n") lines in
  let () = close_out file in
  flush stdout

let () = main ()

