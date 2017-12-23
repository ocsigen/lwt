(* top-level lwt feature configuration *)

let use_libev = ref None
let use_pthread = ref None
let android_target = ref None
let libev_default = ref None

let arg_bool r =
  Arg.Symbol (["true"; "false"],
              function
              | "true" -> r := Some true
              | "false" -> r := Some false
              | _ -> assert false)

let usage =
  "enable lwt.unix features\noptions are:"

let args = [
  "-use-libev", arg_bool use_libev,
    " whether to check for libev";
  "-use-pthread", arg_bool use_pthread,
    " whether to use pthread";
  "-android-target", arg_bool android_target,
    " compile for Android";
  "-libev-default", arg_bool libev_default,
    " whether to use the libev backend by default";
]

let oasis_files = [
  "setup-dev.exe";
  "setup.exe";
  "setup.data";
  "setup.log";
  "src/unix/lwt_config.h";
  "src/unix/lwt_config.ml";
  "src/unix/lwt_unix_jobs_generated.ml";
]

let oasis_warning_message =
"WARNING!!!

There appear to be some files left behind by the old oasis
build system.  This may cause the build to fail.  Please clean
the repository before building.

Deleting the files src/unix/lwt_config.ml and src/unix/lwt_config.h
should allow the build to work.

To clean up all oasis generated files you can checkout the old
build system and run distclean ie

$ git checkout 30c3fd86f9ddebc98e54276100a59c79436b174f
$ make clean distclean
$ git checkout master
$ git clean -ffd
"

let oasis_sanity_check () =
  if List.fold_left (fun b f -> b || (Sys.file_exists f)) false oasis_files then
    Printf.printf "%s" oasis_warning_message
  else ()

let main () =
  Arg.parse args ignore usage;

  let () = oasis_sanity_check () in

  let f = open_out "src/unix/lwt_config" in
  let print name var =
    match var with
    | None -> ()
    | Some var -> Printf.fprintf f "%s: %b\n" name var
  in
  print "use_libev" !use_libev;
  print "use_pthread" !use_pthread;
  print "android_target" !android_target;
  print "libev_default" !libev_default;
  close_out f;

  (* Compilers starting from 4.03.0 support the -O3 flag. *)
  let () =
    let major, minor =
      Scanf.sscanf Sys.ocaml_version "%u.%u"
        (fun major minor -> major, minor)
    in
    let supports_o3 = (major, minor) >= (4, 3) in
    let flags_file = open_out "src/core/flambda.flag" in
    begin
      if supports_o3 then
        output_string flags_file "-O3\n"
      else
        output_string flags_file "()\n"
    end;
    close_out flags_file
  in

  ()

let () =
  main ()
