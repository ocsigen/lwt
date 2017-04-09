(* OASIS_START *)
(* OASIS_STOP *)

(* This is largely based on the Glib-related code in the main build system,
   though rewritten somewhat. *)

let split =
  let regexp = Str.regexp " +" in
  fun s -> Str.split regexp s

(* Runs pkg-config with the given arguments. *)
let pkg_config arguments =
  let command = Printf.sprintf "pkg-config %s" arguments in
  let input_channel = Unix.open_process_in command in
  let result =
    try Pervasives.input_line input_channel
    with End_of_file -> ""
  in
  let status = Unix.close_process_in input_channel in
  match status with
  | Unix.WEXITED 0 -> split result
  | _ ->
    Printf.eprintf "Command failed: %s" command;
    exit 1

(* Loads values from setup.data. In particular, this build needs to check
   whether ccomp_type, as detected by configure, is "msvc". *)
let env =
  BaseEnvLight.load
    ~allow_empty:true
    ~filename:(Pathname.basename BaseEnvLight.default_filename)
    ()

let () =
  dispatch begin fun hook ->
    dispatch_default hook;

    match hook with
    | After_rules ->
      (* Get compiler and linker options using pkg-config. *)
      let cflags = pkg_config "--cflags glib-2.0" in
      let libs =
        let ccomp_type = BaseEnvLight.var_get "ccomp_type" env in
        if ccomp_type = "msvc" then
          pkg_config "--libs-only-L glib-2.0" @
          pkg_config "--libs-only-l --msvc_syntax glib-2.0"
        else
          pkg_config "--libs glib-2.0"
      in

      (* Forward compiler and linker options to Ocamlbuild. *)
      flag ["ocamlmklib"; "c"] @@
        S (List.map (fun s -> A s) libs);
      flag ["compile"; "c"] @@
        S (List.map (fun s -> S [A "-ccopt"; A s]) cflags);
      flag ["link"; "ocaml"] @@
        S (List.map (fun s -> S [A "-cclib"; A s]) libs);

    | _ -> ()
  end
