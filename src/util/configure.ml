(* top-level lwt feature configuration *)

let use_libev = ref None
let use_pthread = ref None
let android_target = ref None
let libev_default = ref None
let use_camlp4 = ref None

let arg_bool r =
  Arg.Symbol (["true"; "false"],
              function
                | "true" -> r := Some true
                | "false" -> r := Some false
                | _ -> assert false)
let args = [
  "-use-libev", arg_bool use_libev, " whether to check for libev";
  "-use-pthread", arg_bool use_pthread, " whether to use pthread";
  "-android-target", arg_bool android_target, " compile for Android";
  "-libev-default", arg_bool libev_default, " whether to use the libev backend by default";
  "-use-camlp4", arg_bool use_camlp4, " when true enable camlp4 syntax extension";
]

let main () =
  Arg.parse args ignore "enable lwt.unix and camlp4 features\noptions are:";
  let f = open_out "src/unix/lwt_config" in
  let print name var =
    match var with
    | None -> ()
    | Some var -> Printf.fprintf f "%s: %b\n" name var
  in
  print"use_libev" !use_libev;
  print"use_pthread" !use_pthread;
  print"android_target" !android_target;
  print"libev_default" !libev_default;
  close_out f;
  (* '-use-camlp4 false' (or none) will write a jbuild-ignore file directing jbuilder to ignore
   *                     the camlp4 directory.
   * '-use-camlp4 true' will write an empty file which does nothing
   *
   * This is a workaround required to overcome some weird camlp4 packaging behaviour
   * where ocamlfind sometimes installs a dummy META file for it even though it's
   * not actually installed. *)
  let f = open_out "src/jbuild-ignore" in
  (if !use_camlp4 = Some(true) then () else Printf.fprintf f "camlp4");
  close_out f

let () = main ()
