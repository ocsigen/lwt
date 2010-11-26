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

open Printf

(* +-----------------------------------------------------------------+
   | Test codes                                                      |
   +-----------------------------------------------------------------+ *)

let caml_code = "
external test : unit -> unit = \"lwt_test\"
let () = test ()
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

(* +-----------------------------------------------------------------+
   | Compilation                                                     |
   +-----------------------------------------------------------------+ *)

let ocamlc = ref "ocamlc"
let ext_obj = ref ".o"
let exec_name = ref "a.out"

let log_file = ref ""
let caml_file = ref ""

let compile stub_file =
  ksprintf
    Sys.command
    "%s -custom %s %s 2> %s"
    !ocamlc
    (Filename.quote stub_file)
    (Filename.quote !caml_file)
    (Filename.quote !log_file)
  = 0

let safe_remove file_name =
  try
    Sys.remove file_name
  with exn ->
    ()

let test_code stub_code =
  let stub_file, oc = Filename.open_temp_file "lwt_stub" ".c" in
  let cleanup () =
    safe_remove stub_file;
    safe_remove (Filename.chop_extension (Filename.basename stub_file) ^ !ext_obj)
  in
  try
    output_string oc stub_code;
    flush oc;
    close_out oc;
    let result = compile stub_file in
    cleanup ();
    result
  with exn ->
    (try close_out oc with _ -> ());
    cleanup ();
    raise exn

let config = open_out "src/unix/config.h"

let test name macro code =
  printf "testing for %s: %!" name;
  if test_code code then begin
    fprintf config "#define %s\n" macro;
    printf "%savailable\n%!" (String.make (30 - String.length name) ' ')
  end else begin
    fprintf config "//#define %s\n" macro;
    printf "%sunavailable\n%!" (String.make (30 - String.length name) ' ')
  end

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let () =
  let args = [
    "-ocamlc", Arg.Set_string ocamlc, "<path> ocamlc";
    "-ext_obj", Arg.Set_string ext_obj, "<ext> C object files extension";
    "-exec_name", Arg.Set_string exec_name, "<name> name of the executable produced by ocamlc";
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
             safe_remove !log_file;
             safe_remove !exec_name;
             safe_remove !caml_file;
             safe_remove (Filename.chop_extension !caml_file ^ ".cmi");
             safe_remove (Filename.chop_extension !caml_file ^ ".cmo"));

  test "fd passing" "HAVE_FD_PASSING" fd_passing_code;
  test "sched_getcpu" "HAVE_GETCPU" getcpu_code;
  test "affinity getting/setting" "HAVE_AFFINITY" affinity_code
