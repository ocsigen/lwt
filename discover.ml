(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Program discover
 * Copyright (C) 2010 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(* Discover available features *)

(* Keep that in sync with the list in myocamlbuild.ml *)
let search_paths = [
  "/usr";
  "/usr/local";
  "/opt";
  "/opt/local";
  "/sw";
  "/mingw";
]

open Printf

(* +-----------------------------------------------------------------+
   | Test codes                                                      |
   +-----------------------------------------------------------------+ *)

let caml_code = "
external test : unit -> unit = \"lwt_test\"
let () = test ()
"

let pthread_code = "
#include <caml/mlvalues.h>
#include <pthread.h>

CAMLprim value lwt_test()
{
  pthread_create(0, 0, 0, 0);
  return Val_unit;
}
"

let libev_code = "
#include <caml/mlvalues.h>
#include <ev.h>

CAMLprim value lwt_test()
{
  ev_default_loop(0);
  return Val_unit;
}
"

let fd_passing_code = "
#include <caml/mlvalues.h>
#include <sys/types.h>
#include <sys/socket.h>

CAMLprim value lwt_test()
{
  struct msghdr msg;
  msg.msg_controllen = 0;
  msg.msg_control = 0;
  return Val_unit;
}
"

let getcpu_code = "
#include <caml/mlvalues.h>
#define _GNU_SOURCE
#include <sched.h>

CAMLprim value lwt_test()
{
  sched_getcpu();
  return Val_unit;
}
"

let affinity_code = "
#include <caml/mlvalues.h>
#define _GNU_SOURCE
#include <sched.h>

CAMLprim value lwt_test()
{
  sched_getaffinity(0, 0, 0);
  return Val_unit;
}
"

let eventfd_code = "
#include <caml/mlvalues.h>
#include <sys/eventfd.h>

CAMLprim value lwt_test()
{
  eventfd(0, 0);
  return Val_unit;
}
"

let get_credentials_code struct_name = "
#define _GNU_SOURCE
#include <caml/mlvalues.h>
#include <sys/types.h>
#include <sys/socket.h>

CAMLprim value lwt_test()
{
  struct " ^ struct_name ^ " cred;
  socklen_t cred_len = sizeof(cred);
  getsockopt(0, SOL_SOCKET, SO_PEERCRED, &cred, &cred_len);
  return Val_unit;
}
"

let get_peereid_code = "
#include <caml/mlvalues.h>
#include <sys/types.h>
#include <unistd.h>

CAMLprim value lwt_test()
{
  uid_t euid;
  gid_t egid;
  getpeereid(0, &euid, &egid);
  return Val_unit;
}
"

let fdatasync_code = "
#include <caml/mlvalues.h>
#include <sys/unistd.h>

CAMLprim value lwt_test()
{
  fdatasync(0);
  return Val_unit;
}
"

let glib_code = "
#include <caml/mlvalues.h>
#include <glib.h>

CAMLprim value lwt_test()
{
  g_main_context_dispatch(0);
  return Val_unit;
}
"

let netdb_reentrant_code = "
#include <caml/mlvalues.h>
#include <netdb.h>

CAMLprim value lwt_test()
{
  getprotobyname_r(0, 0, 0, 0, 0);
  getprotobynumber_r(0, 0, 0, 0, 0);
  getservbyname_r(0, 0, 0, 0, 0, 0);
  getservbyport_r(0, 0, 0, 0, 0, 0);
  return Val_unit;
}
"

(* +-----------------------------------------------------------------+
   | Compilation                                                     |
   +-----------------------------------------------------------------+ *)

let ocamlc = ref "ocamlc"
let ext_obj = ref ".o"
let exec_name = ref "a.out"
let use_libev = ref true
let use_glib = ref false
let use_pthread = ref true
let use_unix = ref true
let os_type = ref "Unix"
let ccomp_type = ref "cc"

let log_file = ref ""
let caml_file = ref ""

(* Search for a header file in standard directories. *)
let search_header header =
  let rec loop = function
    | [] ->
        None
    | dir :: dirs ->
        if Sys.file_exists (dir ^ "/include/" ^ header) then
          Some dir
        else
          loop dirs
  in
  loop search_paths

let compile (opt, lib) stub_file =
  ksprintf
    Sys.command
    "%s -custom %s %s %s %s > %s 2>&1"
    !ocamlc
    (String.concat " " (List.map (sprintf "-ccopt %s") opt))
    (Filename.quote stub_file)
    (Filename.quote !caml_file)
    (String.concat " " (List.map (sprintf "-cclib %s") lib))
    (Filename.quote !log_file)
  = 0

let safe_remove file_name =
  try
    Sys.remove file_name
  with exn ->
    ()

let test_code args stub_code =
  let stub_file, oc = Filename.open_temp_file "lwt_stub" ".c" in
  let cleanup () =
    safe_remove stub_file;
    safe_remove (Filename.chop_extension (Filename.basename stub_file) ^ !ext_obj)
  in
  try
    output_string oc stub_code;
    flush oc;
    close_out oc;
    let result = compile args stub_file in
    cleanup ();
    result
  with exn ->
    (try close_out oc with _ -> ());
    cleanup ();
    raise exn

let config = open_out "src/unix/lwt_config.h"
let config_ml = open_out "src/unix/lwt_config.ml"

let () =
  fprintf config "\
#ifndef __LWT_CONFIG_H
#define __LWT_CONFIG_H
"

let not_available = ref []

let test_feature ?(do_check = true) name macro test =
  if do_check then begin
    printf "testing for %s:%!" name;
    if test () then begin
      if macro <> "" then begin
        fprintf config "#define %s\n" macro;
        fprintf config_ml "#let %s = true\n" macro
      end;
      printf " %s available\n%!" (String.make (34 - String.length name) '.')
    end else begin
      if macro <> "" then begin
        fprintf config "//#define %s\n" macro;
        fprintf config_ml "#let %s = false\n" macro
      end;
      printf " %s unavailable\n%!" (String.make (34 - String.length name) '.');
      not_available := name :: !not_available
    end
  end else begin
    printf "not checking for %s\n%!" name;
    if macro <> "" then begin
      fprintf config "//#define %s\n" macro;
      fprintf config_ml "#let %s = false\n" macro
    end
  end

(* +-----------------------------------------------------------------+
   | pkg-config                                                      |
   +-----------------------------------------------------------------+ *)

let split str =
  let rec skip_spaces i =
    if i = String.length str then
      []
    else
      if str.[i] = ' ' then
        skip_spaces (i + 1)
      else
        extract i (i + 1)
  and extract i j =
    if j = String.length str then
      [String.sub str i (j - i)]
    else
      if str.[j] = ' ' then
        String.sub str i (j - i) :: skip_spaces (j + 1)
      else
        extract i (j + 1)
  in
  skip_spaces 0

let pkg_config flags =
  if ksprintf Sys.command "pkg-config %s > %s 2>&1" flags !log_file = 0 then begin
    let ic = open_in !log_file in
    let line = input_line ic in
    close_in ic;
    split line
  end else
    raise Exit

let pkg_config_flags name =
  try
    (* Get compile flags. *)
    let opt = ksprintf pkg_config "--cflags %s" name in
    (* Get linking flags. *)
    let lib =
      if !ccomp_type = "msvc" then
        (* With msvc we need to pass "glib-2.0.lib" instead of
           "-lglib-2.0" otherwise executables will fail. *)
        ksprintf pkg_config "--libs-only-L %s" name @ ksprintf pkg_config "--libs-only-l --msvc-syntax %s" name
      else
        ksprintf pkg_config "--libs %s" name
    in
    Some (opt, lib)
  with Exit ->
    None

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let arg_bool r =
  Arg.Symbol (["true"; "false"],
              function
                | "true" -> r := true
                | "false" -> r := false
                | _ -> assert false)
let () =
  let args = [
    "-ocamlc", Arg.Set_string ocamlc, "<path> ocamlc";
    "-ext-obj", Arg.Set_string ext_obj, "<ext> C object files extension";
    "-exec-name", Arg.Set_string exec_name, "<name> name of the executable produced by ocamlc";
    "-use-libev", arg_bool use_libev, " whether to check for libev";
    "-use-glib", arg_bool use_glib, " whether to check for glib";
    "-use-pthread", arg_bool use_pthread, " whether to use pthread";
    "-use-unix", arg_bool use_unix, " whether to build lwt.unix";
    "-os-type", Arg.Set_string os_type, "<name> type of the target os";
    "-ccomp-type", Arg.Set_string ccomp_type, "<ccomp-type> C compiler type";
  ] in
  Arg.parse args ignore "check for external C libraries and available features\noptions are:";

  (* Check nothing if we do not build lwt.unix. *)
  if not !use_unix then exit 0;

  (* Put the caml code into a temporary file. *)
  let file, oc = Filename.open_temp_file "lwt_caml" ".ml" in
  caml_file := file;
  output_string oc caml_code;
  close_out oc;

  log_file := Filename.temp_file "lwt_output" ".log";

  (* Cleanup things on exit. *)
  at_exit (fun () ->
             (try close_out config with _ -> ());
             (try close_out config_ml with _ -> ());
             safe_remove !log_file;
             safe_remove !exec_name;
             safe_remove !caml_file;
             safe_remove (Filename.chop_extension !caml_file ^ ".cmi");
             safe_remove (Filename.chop_extension !caml_file ^ ".cmo"));

  let setup_data = ref [] in

  (* Test for pkg-config. *)
  test_feature ~do_check:(!use_libev || !use_glib) "pkg-config" ""
    (fun () ->
       ksprintf Sys.command "pkg-config --version > %s 2>&1" !log_file = 0);

  (* Not having pkg-config is not fatal. *)
  let have_pkg_config = !not_available = [] in
  not_available := [];

  let test_libev () =
    let opt, lib =
      match if have_pkg_config then pkg_config_flags "libev" else None with
        | Some (opt, lib) ->
            (opt, lib)
        | None ->
            match search_header "ev.h" with
              | Some dir ->
                  ([sprintf "-I%s/include" dir], [sprintf "-L%s/lib" dir; "-lev"])
              | None ->
                  ([], ["-lev"])
    in
    setup_data := ("libev_opt", opt) :: ("libev_lib", lib) :: !setup_data;
    test_code (opt, lib) libev_code
  in

  let test_pthread () =
    let opt, lib =
      match search_header "pthread.h" with
        | Some dir ->
            ([sprintf "-I%s/include" dir], [sprintf "-L%s/lib" dir; "-lpthread"])
        | None ->
            ([], ["-lpthread"])
    in
    setup_data := ("pthread_opt", opt) :: ("pthread_lib", lib) :: !setup_data;
    test_code (opt, lib) pthread_code
  in

  let test_glib () =
    if have_pkg_config then
      match pkg_config_flags "glib-2.0" with
        | Some (opt, lib) ->
            setup_data := ("glib_opt", opt) :: ("glib_lib", lib) :: !setup_data;
            test_code (opt, lib) glib_code
        | None ->
            false
    else
      false
  in

  test_feature ~do_check:!use_libev "libev" "HAVE_LIBEV" test_libev;
  test_feature ~do_check:!use_pthread "pthread" "HAVE_PTHREAD" test_pthread;
  test_feature ~do_check:!use_glib "glib" "" test_glib;

  if !not_available <> [] then begin
    if !use_glib && not have_pkg_config then
      printf "The 'pkg-config' command is not available, you need it to build glib support.";
    printf "
The following recquired C libraries are missing: %s.
Please install them and retry. If they are installed in a non-standard location, set the environment variables C_INCLUDE_PATH and LIBRARY_PATH accordingly and retry.

For example, if they are installed in /opt/local, you can type:

export C_INCLUDE_PATH=/opt/local/include
export LIBRARY_PATH=/opt/local/lib

To compile without libev support, use ./configure --disable-libev ...
" (String.concat ", " !not_available);
    exit 1
  end;

  if !os_type <> "Win32" && not !use_pthread then begin
    printf "
No threading library available!

One is needed if you want to build lwt.unix.

Lwt can use pthread or the win32 API.
";
    exit 1
  end;

  let do_check = !os_type <> "Win32" in
  test_feature ~do_check "eventfd" "HAVE_EVENTFD" (fun () -> test_code ([], []) eventfd_code);
  test_feature ~do_check "fd passing" "HAVE_FD_PASSING" (fun () -> test_code ([], []) fd_passing_code);
  test_feature ~do_check "sched_getcpu" "HAVE_GETCPU" (fun () -> test_code ([], []) getcpu_code);
  test_feature ~do_check "affinity getting/setting" "HAVE_AFFINITY" (fun () -> test_code ([], []) affinity_code);
  test_feature ~do_check "credentials getting (Linux)" "HAVE_GET_CREDENTIALS_LINUX" (fun () -> test_code ([], []) (get_credentials_code "ucred"));
  test_feature ~do_check "credentials getting (NetBSD)" "HAVE_GET_CREDENTIALS_NETBSD" (fun () -> test_code ([], []) (get_credentials_code "sockcred"));
  test_feature ~do_check "credentials getting (OpenBSD)" "HAVE_GET_CREDENTIALS_OPENBSD" (fun () -> test_code ([], []) (get_credentials_code "sockpeercred"));
  test_feature ~do_check "credentials getting (FreeBSD)" "HAVE_GET_CREDENTIALS_FREEBSD" (fun () -> test_code ([], []) (get_credentials_code "cmsgcred"));
  test_feature ~do_check "credentials getting (getpeereid)" "HAVE_GETPEEREID" (fun () -> test_code ([], []) get_peereid_code);
  test_feature ~do_check "fdatasync" "HAVE_FDATASYNC" (fun () -> test_code ([], []) fdatasync_code);
  test_feature ~do_check "netdb_reentrant" "HAVE_NETDB_REENTRANT" (fun () -> test_code ([], []) netdb_reentrant_code);

  let get_cred_vars = [
    "HAVE_GET_CREDENTIALS_LINUX";
    "HAVE_GET_CREDENTIALS_NETBSD";
    "HAVE_GET_CREDENTIALS_OPENBSD";
    "HAVE_GET_CREDENTIALS_FREEBSD";
    "HAVE_GETPEEREID";
  ] in

  Printf.fprintf config "\
#if %s
#  define HAVE_GET_CREDENTIALS
#endif
"
    (String.concat " || " (List.map (Printf.sprintf "defined(%s)") get_cred_vars));

  Printf.fprintf config_ml
    "#let HAVE_GET_CREDENTIALS = %s\n"
    (String.concat " || " get_cred_vars);

  if !os_type = "Win32" then begin
    output_string config "#define LWT_ON_WINDOWS\n";
    output_string config_ml "#let windows=true\n"
  end else begin
    output_string config "//#define LWT_ON_WINDOWS\n";
    output_string config_ml "#let windows=false\n"
  end;

  fprintf config "#endif\n";

  (* Our setup.data keys. *)
  let setup_data_keys = [
    "libev_opt";
    "libev_lib";
    "pthread_lib";
    "pthread_opt";
    "glib_opt";
    "glib_lib";
  ] in

  (* Load setup.data *)
  let setup_data_lines =
    match try Some (open_in "setup.data") with Sys_error _ -> None with
      | Some ic ->
          let rec aux acc =
            match try Some (input_line ic) with End_of_file -> None with
              | None ->
                  close_in ic;
                  acc
              | Some line ->
                  match try Some(String.index line '=') with Not_found -> None with
                    | Some idx ->
                        let key = String.sub line 0 idx in
                        if List.mem key setup_data_keys then
                          aux acc
                        else
                          aux (line :: acc)
                    | None ->
                        aux (line :: acc)
          in
          aux []
      | None ->
          []
  in

  (* Add flags to setup.data *)
  let setup_data_lines =
    List.fold_left
      (fun lines (name, args) ->
         sprintf "%s=%S" name (String.concat " " args) :: lines)
      setup_data_lines !setup_data
  in
  let oc = open_out "setup.data" in
  List.iter
    (fun str -> output_string oc str; output_char oc '\n')
    (List.rev setup_data_lines);
  close_out oc;

  close_out config;
  close_out config_ml;

  (* Generate stubs. *)
  print_endline "Generating C stubs...";
  exit (Sys.command "ocaml src/unix/gen_stubs.ml")

