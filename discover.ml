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

let get_credentials_code = "
#include <caml/mlvalues.h>
#include <sys/types.h>
#include <sys/socket.h>

CAMLprim value lwt_test()
{
    getsockopt(0, SOL_SOCKET, SO_PEERCRED, 0, 0);
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

(* +-----------------------------------------------------------------+
   | Compilation                                                     |
   +-----------------------------------------------------------------+ *)

let ocamlc = ref "ocamlc"
let ext_obj = ref ".o"
let exec_name = ref "a.out"

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

let c_args =
  let flags path = Printf.sprintf "-ccopt -I%s/include -cclib -L%s/lib" path path in
  match search_header "ev.h", search_header "pthread.h" with
    | None, None -> ""
    | Some path, None | None, Some path -> flags path
    | Some path1, Some path2 when path1 = path2 -> flags path1
    | Some path1, Some path2 -> flags path1 ^ " " ^ flags path2

let compile args stub_file =
  ksprintf
    Sys.command
    "%s -custom %s %s %s %s 2> %s"
    !ocamlc
    c_args
    (Filename.quote stub_file)
    args
    (Filename.quote !caml_file)
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

let test_lib name ?(args="") code =
  printf "testing for %s:%!" name;
  if test_code args code then begin
    printf " %s available\n%!" (String.make (34 - String.length name) '.');
    true
  end else begin
    printf " %s unavailable\n%!" (String.make (34 - String.length name) '.');
    false
  end

let test_feature name macro ?(args="") code =
  printf "testing for %s:%!" name;
  if test_code args code then begin
    fprintf config "#define %s\n" macro;
    fprintf config_ml "#let %s = true\n" macro;
    printf " %s available\n%!" (String.make (34 - String.length name) '.')
  end else begin
    fprintf config "//#define %s\n" macro;
    fprintf config_ml "#let %s = false\n" macro;
    printf " %s unavailable\n%!" (String.make (34 - String.length name) '.')
  end

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let () =
  let args = [
    "-ocamlc", Arg.Set_string ocamlc, "<path> ocamlc";
    "-ext-obj", Arg.Set_string ext_obj, "<ext> C object files extension";
    "-exec-name", Arg.Set_string exec_name, "<name> name of the executable produced by ocamlc";
  ] in
  Arg.parse args ignore "check for the need of -liconv\noptions are:";

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

  let missing = [] in
  let missing = if test_lib "libev" ~args:"-cclib -lev" libev_code then missing else "libev" :: missing in
  let missing = if test_lib "pthread" ~args:"-cclib -lpthread" pthread_code then missing else "pthread" :: missing in

  if missing <> [] then begin
    printf "
The following recquired C libraries are missing: %s.
Please install them and retry. If they are installed in a non-standard location, set the environment variables C_INCLUDE_PATH and LIBRARY_PATH accordingly and retry.

For example, if they are installed in /opt/local, you can type:

export C_INCLUDE_PATH=/opt/local/include
export LIBRARY_PATH=/opt/local/lib

" (String.concat ", " missing);
    exit 1
  end;

  test_feature "eventfd" "HAVE_EVENTFD" eventfd_code;
  test_feature "fd passing" "HAVE_FD_PASSING" fd_passing_code;
  test_feature "sched_getcpu" "HAVE_GETCPU" getcpu_code;
  test_feature "affinity getting/setting" "HAVE_AFFINITY" affinity_code;
  test_feature "credentials getting" "HAVE_GET_CREDENTIALS" get_credentials_code;
  test_feature "fdatasync" "HAVE_FDATASYNC" fdatasync_code
