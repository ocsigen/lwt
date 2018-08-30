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

let main () =
  Arg.parse args ignore usage;

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
