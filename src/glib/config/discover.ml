
(* This is largely based on the Glib-related code in the main build system,
   though rewritten somewhat. *)

let split =
  let regexp = Str.regexp " +" in
  fun s -> Str.split regexp s

(* Runs pkg-config with the given arguments. *)
let pkg_config arguments =
  (* Run the pkg-config command. *)
  let command = Printf.sprintf "pkg-config %s" arguments in
  let input_channel = Unix.open_process_in command in
  let result =
    try Pervasives.input_line input_channel
    with End_of_file -> ""
  in
  let status = Unix.close_process_in input_channel in

  match status with
  | Unix.WEXITED 0 ->
    (* On success, split the output of pkg-config result (arguments for the
       compiler) into tokens. *)
    let result = split result in

    (* Then, look for any -Wl,-framework tokens. Replace them with -framework
       tokens, and remove the prefix "-Wl," from the next token. This is
       necessary because pkg-config emits options for the compiler to pass to
       the linker, so they are prefixed with "-Wl,". However, ocamlmktop
       expects the -framework options unprefixed. *)
    let rec unprefix_framework_arguments acc = function
      | [] -> List.rev acc
      | "-Wl,-framework"::framework::rest ->
        let framework = String.sub framework 4 (String.length framework - 4) in
        unprefix_framework_arguments (framework::"-framework"::acc) rest
      | argument::rest ->
        unprefix_framework_arguments (argument::acc) rest
    in
    let result = unprefix_framework_arguments [] result in

    result

  | _ ->
    Printf.eprintf "Command failed: %s" command;
    exit 1

(* read ocamlc -config file, if provided *)
let get_ocamlc_config name =
  let f = open_in name in
  let cfg line =
    let idx = String.index line ':' in
    String.sub line 0 idx,
    String.sub line (idx + 2) (String.length line - idx - 2)
  in
  let input_line () = try Some(input_line f) with End_of_file -> None in
  let rec lines () =
    match input_line () with
    | None -> []
    | Some(x) -> cfg x :: lines ()
  in
  let cfg = lines () in
  let () = close_in f in
  cfg

let ccomp_type =
  try
    let cfg = get_ocamlc_config Sys.argv.(1) in
    List.assoc "ccomp_type" cfg
  with _ ->
    let () = Printf.eprintf "failed to read ccomp_type from ocamlc -config\n" in
    exit 1

let cflags = pkg_config "--cflags glib-2.0"
let libs =
  if String.compare ccomp_type "msvc" = 0 then
    pkg_config "--libs-only-L glib-2.0" @
    pkg_config "--libs-only-l --msvc_syntax glib-2.0"
  else
    pkg_config "--libs glib-2.0"

(* do sexps properly...
let () =
  let write_sexp fn sexp = Out_channel.write_all fn ~data:(Sexp.to_string sexp) in
  write_sexp "glib_c_flags.sexp"         (sexp_of_list sexp_of_string cflags);
  write_sexp "glib_c_library_flags.sexp" (sexp_of_list sexp_of_string libs)
*)

let () =
  let write_sexp n x =
    let f = open_out n in
    output_string f ("(" ^ String.concat " " x ^ ")");
    close_out f
  in
  write_sexp ("glib_c_flags.sexp")         cflags;
  write_sexp ("glib_c_library_flags.sexp") libs
