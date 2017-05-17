(* OASIS_START *)
(* OASIS_STOP *)

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
