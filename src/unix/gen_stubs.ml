(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 *
 * Copyright (C) 2012 Jérémie Dimino
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

(* Script for generating C stubs for jobs. *)

open Printf

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

(* Kind of caml values. *)
type value_repr =
  | Val_int
  | Val_string
  | Val_block
  | Val_custom of string

type caml_type =
  | Caml_void
  | Caml_int
  | Caml_int32
  | Caml_int64
  | Caml_bool
  | Caml_char
  | Caml_string
  | Caml_bytes
  | Caml_tuple of caml_type list
  | Caml_record of string * (string * caml_type) list
  | Caml_variant of string * (string * caml_type list) list
  | Caml_alias of string * caml_type
  | Caml_list of caml_type

type c_type =
  | C_void
  | C_char
  | C_int
  | C_long
  | C_int_alias of string
  | C_long_alias of string
  | C_ptr of c_type
  | C_array of c_type * int
  | C_dyn_array of c_type

type hint =
  | Hint_none
  | Hint_flag_list of (constructor_map * string option) list
  | Hint_table of (constructor_map * string option) list

and constructor_map =
  | S of string
  | D of string * string

type direction =
  | In
  | Out
  | In_out

type abstract_type = {
  caml_type : caml_type;
  c_type : c_type;
  hint : hint;
}

type unix_error =
  | Uerror_errno
  | Uerror_result
  | Uerror_none

type job = {
  includes : string list;
  name : string;
  params : (string * direction * abstract_type) list;
  result : abstract_type;
  uerror : unix_error;
  check : string option;
  exists_if : (bool * string) list;
  map64 : string list;
}

(* +-----------------------------------------------------------------+
   | Configuration                                                   |
   +-----------------------------------------------------------------+ *)

let prog_name = Filename.basename Sys.executable_name
let log fmt = ksprintf (fun msg -> prerr_endline (prog_name ^ ": " ^ msg)) fmt

(* +-----------------------------------------------------------------+
   | Types                                                           |
   +-----------------------------------------------------------------+ *)

let unit = { c_type = C_int; caml_type = Caml_void; hint = Hint_none }
let int = { c_type = C_int; caml_type = Caml_int; hint = Hint_none }
let int32 = { c_type = C_int; caml_type = Caml_int32; hint = Hint_none }
let int64 c_type = { c_type = c_type; caml_type = Caml_int64; hint = Hint_none }
let long = { c_type = C_long; caml_type = Caml_int; hint = Hint_none }
let char = { c_type = C_int; caml_type = Caml_char; hint = Hint_none }
let string = { c_type = C_ptr C_char; caml_type = Caml_string; hint = Hint_none }
let bool = { c_type = C_int; caml_type = Caml_bool; hint = Hint_none }
let file_descr = { int with caml_type = Caml_alias ("Unix.file_descr", Caml_int) }
let off_t = { long with c_type = C_long_alias "off_t" }

let flag_list name c_type l = {
  c_type = c_type;
  caml_type = Caml_list (Caml_variant (name, List.map (function (S name, _) -> (name, []) | (D (name, _), _) -> (name, [])) l));
  hint = Hint_flag_list l;
}

let table name c_type l = {
  c_type = c_type;
  caml_type = Caml_variant (name, List.map (function (S name, _) -> (name, []) | (D (name, _), _) -> (name, [])) l);
  hint = Hint_table l;
}

let filter_fst l = List.map snd (List.filter fst l)

let open_flag =
  flag_list "Unix.open_flag" C_int [
    S "O_RDONLY", None;
    S "O_WRONLY", None;
    S "O_RDWR", None;
    S "O_NONBLOCK", Some "O_NDELAY";
    S "O_APPEND", None;
    S "O_CREAT", None;
    S "O_TRUNC", None;
    S "O_EXCL", None;
    S "O_NOCTTY", None;
    S "O_DSYNC", Some "0";
    S "O_SYNC", Some "0";
    S "O_RSYNC", Some "0";
    S "O_SHARE_DELETE", None;
  ]

let access_permission =
  flag_list "Unix.access_permission" C_int [
    S "R_OK", None;
    S "W_OK", None;
    S "X_OK", None;
    S "F_OK", None;
  ]

let seek_command =
  table "Unix.seek_command" C_int [
    S "SEEK_SET", None;
    S "SEEK_CUR", None;
    S "SEEK_END", None;
  ]

let flush_queue =
  table "Unix.flush_queue" C_int [
    S "TCIFLUSH", None;
    S "TCOFLUSH", None;
    S "TCIOFLUSH", None;
  ]

let flow_action =
  table "Unix.flow_action" C_int [
    S "TCOOFF", None;
    S "TCOON", None;
    S "TCIOFF", None;
    S "TCION", None;
  ]

(* +-----------------------------------------------------------------+
   | Jobs                                                            |
   +-----------------------------------------------------------------+ *)

let simple_unit includes name args = {
  includes = includes;
  name = name;
  params = List.map (fun (name, a_type) -> (name, In, a_type)) args;
  result = { int with caml_type = Caml_void };
  uerror = Uerror_errno;
  check = Some "$(result) < 0";
  exists_if = [];
  map64 = [];
}

type item =
  | Job of job
  | Seq of item list

let rec map_jobs f item =
  match item with
    | Job job -> f job
    | Seq l -> Seq (List.map (map_jobs f) l)

let path_and_fd =
  map_jobs
    (fun job ->
       if List.exists (fun (name, _, _) -> name = "path") job.params then
         Seq [
           Job job;
           Job { job with
                   name = "f" ^ job.name;
                   params = List.map (fun ((name, _, _) as param) -> if name = "path" then ("fd", In, file_descr) else param) job.params;
               };
         ]
       else
         Job job)

let jobs = Seq [
  Job (simple_unit ["unistd"] "close" [("fd", file_descr)]);
  Job (simple_unit ["unistd"] "unlink" [("path", string)]);
  Job (simple_unit ["unistd"] "chroot" [("path", string)]);
  Job (simple_unit ["unistd"] "rmdir" [("path", string)]);
  Job (simple_unit ["unistd"] "chdir" [("path", string)]);
  path_and_fd (Job (simple_unit ["sys/stat"] "chmod" [("path", string); ("mode", int)]));
  path_and_fd (Job { (simple_unit ["unistd"; "sys/types"] "truncate" [("path", string); ("offset", off_t)]) with map64 = ["offset"] });
  path_and_fd (Job (simple_unit ["unistd"] "chown" [("path", string); ("ower", int); ("group", int)]));
  Job (simple_unit ["stdio"] "rename" [("oldpath", string); ("newpath", string)]);
  Job (simple_unit ["unistd"] "link" [("oldpath", string); ("newpath", string)]);
  Job (simple_unit ["unistd"] "symlink" [("oldpath", string); ("newpath", string)]);
  Job (simple_unit ["sys/stat"; "sys/types"] "mkdir" [("path", string); ("mode", int)]);
  Job (simple_unit ["sys/stat"; "sys/types"] "mkfifo" [("path", string); ("mode", int)]);
  Job (simple_unit ["unistd"] "access" [("path", string); ("mode", access_permission)]);
  Job (simple_unit ["unistd"] "fsync" [("fd", file_descr)]);
  Job { (simple_unit ["unistd"] "fdatasync" [("fd", file_descr)]) with exists_if = [(true, "HAVE_FDATASYNC")] };
  Job { (simple_unit ["sys/types"; "unistd"] "lseek" [("fd", file_descr); ("offset", off_t); ("whence", seek_command)])
        with
          result = off_t;
          check = Some "$(result) == (off_t)-1";
          map64 = ["offset"; "result"];
      };
  Job { (simple_unit ["termios"; "unistd"] "tcdrain" [("fd", file_descr)])
        with exists_if = [(false, "__ANDROID__")] };
  Job (simple_unit ["termios"; "unistd"] "tcflush" [("fd", file_descr); ("queue", flush_queue)]);
  Job (simple_unit ["termios"; "unistd"] "tcflow" [("fd", file_descr); ("action", flow_action)]);
  Job (simple_unit ["termios"; "unistd"] "tcsendbreak" [("fd", file_descr); ("duration", int)]);
]

(* +-----------------------------------------------------------------+
   | Code generation                                                 |
   +-----------------------------------------------------------------+ *)

let rec c_type_name = function
  | C_void -> "void"
  | C_int -> "int"
  | C_long -> "long"
  | C_int_alias name -> name
  | C_long_alias name -> name
  | C_char -> "char"
  | C_ptr t -> c_type_name t ^ "*"
  | C_array (t, n) -> c_type_name t ^ "[" ^ string_of_int n ^ "]"
  | C_dyn_array t -> c_type_name t ^ "[]"

let rec caml_type_name = function
  | Caml_void -> "<void>"
  | Caml_int -> "int"
  | Caml_int32 -> "int32"
  | Caml_int64 -> "int64"
  | Caml_bool -> "bool"
  | Caml_char -> "char"
  | Caml_string -> "string"
  | Caml_bytes -> "Lwt_bytes.t"
  | Caml_tuple l -> String.concat " * " (List.map caml_type_name l)
  | Caml_record (name, _) -> name
  | Caml_variant (name, _) -> name
  | Caml_alias (name, _) -> name
  | Caml_list (Caml_tuple _ as ty) -> sprintf "(%s) list" (caml_type_name ty)
  | Caml_list ty -> sprintf "%s list" (caml_type_name ty)

let rec real_caml_type = function
  | Caml_alias (_, t) -> real_caml_type t
  | t -> t

let string_of_direction = function
  | In -> "in"
  | Out -> "out"
  | In_out -> "in & out"

let subst job patt =
  let buf = Buffer.create 128 in
  Buffer.add_substitute buf (fun var -> if job then "job->" ^ var else var) patt;
  Buffer.contents buf

let rec find_map f l =
  match l with
    | [] ->
        raise Not_found
    | x :: l ->
        match f x with
          | Some x -> x
          | None -> find_map f l

let split_name name =
  try
    let idx = String.rindex name '.' in
    (String.sub name 0 idx, String.sub name (idx + 1) (String.length name - idx - 1))
  with Not_found ->
    ("", name)

let ml_oc = ref stdout

module type Params = sig
  val fname : string
  val oc : out_channel
  val job : job
end

module type Generator = sig
  val gen_worker : string -> unit
  val gen_result : string -> unit
  val gen_job_stub : string -> string -> string -> unit
  val gen_caml : string -> unit
  val gen_file : unit -> unit
end

module MakeGen(Gen64 : Generator)(Params : Params) = struct
  open Params

  let map_in_64 = List.exists (fun name -> List.exists (fun (name', dir, _) -> name = name' && dir = In) job.params) job.map64
  let map_out_64 = List.exists (fun name -> List.exists (fun (name', dir, _) -> name = name' && dir = Out) job.params) job.map64
  let map_result_64 = List.mem "result" job.map64

  let pr fmt = fprintf oc fmt

  let ins =
    List.filter
      (fun (_, dir, { caml_type }) ->
         (dir = In || dir = In_out) && caml_type <> Caml_void)
      job.params

  let strings =
    List.map
      (fun (name, _, _) -> name)
      (List.filter
         (fun (_, dir, { caml_type }) -> dir = In && caml_type = Caml_string)
         job.params)

  let caml_return_type, is_tuple =
    match job.result.caml_type with
      | Caml_void ->
          ("unit", false)
      | Caml_tuple _ as t ->
          (caml_type_name t, true)
      | t ->
          (caml_type_name t, false)

  let caml_arg_types =
    String.concat " -> "
      (match
         List.map
           (fun (_, _, { caml_type }) -> caml_type_name caml_type)
           (List.filter (fun (_, dir, { caml_type }) -> dir = In && caml_type <> Caml_void) job.params)
       with
         | [] -> ["unit"]
         | l -> l)

  let gen_worker suffix =
    pr "\n";
    pr "/* The function calling [%s]. */\n" job.name;
    pr "static void worker_%s%s(struct job_%s* job)\n" job.name suffix job.name;
    pr "{\n";
    pr "  /* Perform the blocking call. */\n";
    pr "  ";
    if job.result.c_type <> C_void then pr "job->result = ";
    pr "%s(%s);\n" job.name
      (String.concat ", "
         (List.map
            (fun (name, dir, _) ->
               match dir with
                 | In -> "job->" ^ name
                 | Out | In_out -> "&job->" ^ name)
            job.params));
    if job.uerror = Uerror_errno then begin
      pr "  /* Save the value of errno. */\n";
      pr "  job->errno_copy = errno;\n"
    end;
    pr "}\n"

  let gen_result suffix =
    pr "\n";
    pr "/* The function building the caml result. */\n";
    pr "static value result_%s%s(struct job_%s* job)\n" job.name suffix job.name;
    pr "{\n";
    if job.result.caml_type <> Caml_void then
      pr "  value result;\n";
    (match job.check with
       | None ->
           ()
       | Some test ->
           pr "  /* Check for errors. */\n";
           pr "  if (%s) {\n" (subst true test);
           pr "    /* Save the value of errno so we can use it once the job has been freed. */\n";
           pr "    int error = job->errno_copy;\n";
           (match strings with
              | [] ->
                  ()
              | name :: _ ->
                  pr "    /* Copy the contents of job->%s into a caml string. */\n" name;
                  pr "    value string_argument = caml_copy_string(job->%s);\n" name);
           pr "    /* Free the job structure. */\n";
           pr "    lwt_unix_free_job(&job->job);\n";
           pr "    /* Raise the error. */\n";
           (match strings with
              | [] ->
                  pr "    unix_error(error, %S, Nothing);\n" job.name
              | name :: _ ->
                  pr "    unix_error(error, %S, string_argument);\n" job.name);
           pr "  }\n");
    if job.result.caml_type <> Caml_void then begin
      pr "  /* Build the caml result. */\n";
      match job.result.caml_type, job.result.c_type with
        | Caml_int, (C_int | C_int_alias _) ->
            pr "  result = Val_int(job->result);\n"
        | Caml_int, (C_long | C_long_alias _) ->
            pr "  result = Val_long(job->result);\n"
        | Caml_int64, (C_long | C_long_alias _) ->
            pr "  result = caml_copy_int64(job->result);\n"
        | _ ->
            assert false
    end;
    pr "  /* Free the job structure. */\n";
    pr "  lwt_unix_free_job(&job->job);\n";
    pr "  /* Return the result. */\n";
    if job.result.caml_type = Caml_void then
      pr "  return Val_unit;\n"
    else
      pr "  return result;\n";
    pr "}\n"

  let gen_job_stub suffix worker_suffix result_suffix =
    pr "\n";
    pr "/* The stub creating the job structure. */\n";
    pr "CAMLprim value lwt_unix_%s%s_job(%s)\n" job.name suffix (String.concat ", " (List.map (fun (name, _, _) -> "value " ^ name) ins));
    pr "{\n";
    List.iter
      (fun name ->
         pr "  /* Get the length of the %s parameter. */\n" name;
         pr "  mlsize_t len_%s = caml_string_length(%s) + 1;\n" name name)
      strings;
    pr "  /* Allocate a new job. */\n";
    if strings = [] then
      pr "  struct job_%s* job = lwt_unix_new(struct job_%s);\n" job.name job.name
    else
      pr "  struct job_%s* job = lwt_unix_new_plus(struct job_%s, %s);\n"
        job.name job.name
        (String.concat " + " (List.map (fun name -> "len_" ^ name) strings));
    let rec loop = function
      | [] ->
          ()
      | name :: names ->
          loop names;
          pr "  /* Set the offset of the %s parameter inside the job structure. */\n" name;
          pr "  job->%s = %s;\n" name (String.concat " + " ("job->data" :: List.map (fun n -> "len_" ^ n) names))
    in
    loop (List.rev strings);
    List.iter
      (fun name ->
         pr "  /* Copy the %s parameter inside the job structure. */\n" name;
         pr "  memcpy(job->%s, String_val(%s), len_%s);\n" name name name)
      strings;
    pr "  /* Initializes function fields. */\n";
    pr "  job->job.worker = (lwt_unix_job_worker)worker_%s%s;\n" job.name worker_suffix;
    pr "  job->job.result = (lwt_unix_job_result)result_%s%s;\n" job.name result_suffix;
    List.iter
      (fun (name, dir, { caml_type; c_type; hint }) ->
         if caml_type <> Caml_string then begin
           pr "  /* Copy the %s parameter. */\n" name;
           pr "  job->%s = " name;
           (match real_caml_type caml_type, c_type, hint with
              | Caml_int, (C_int | C_int_alias _), _ -> pr "Int_val(%s)" name
              | Caml_int, (C_long | C_long_alias _), _ -> pr "Long_val(%s)" name
              | Caml_int64, (C_long | C_long_alias _), _ -> pr "Int64_val(%s)" name
              | Caml_list (Caml_variant (type_name, _)), _, Hint_flag_list _ -> pr "%s_of_%ss(%s)" (c_type_name c_type) (snd (split_name type_name)) name
              | Caml_variant (type_name, _), _, Hint_table _ -> pr "%s_table[Int_val(%s)]" (snd (split_name type_name)) name
              | _ -> assert false);
           pr ";\n"
         end)
      ins;
    pr "  /* Wrap the structure into a caml value. */\n";
    pr "  return lwt_unix_alloc_job(&job->job);\n";
    pr "}\n"

  let gen_caml suffix =
    let pr fmt = fprintf !ml_oc fmt in
    pr "  external %s%s_job : %s -> %s Job.t = \"lwt_unix_%s%s_job\"\n"
      job.name
      suffix
      caml_arg_types
      (if is_tuple then
         "(" ^ caml_return_type ^ ")"
       else
         caml_return_type)
      job.name
      suffix

  let gen_file () =
    pr "\
/*
 * %s
 * %s
 *
 * File generated with %s
 */

/* Note: this file was generated at configure time. If it does not
   work and you want to fix it, you can modify it and send the result
   to the ocsigen mailing list.

   If you are courageous you can also look at the ocaml script that
   generated this file (%s). */

/* Informations:

   - this is the expected prototype of the C function [%s]:

       %s %s(%s)

   - these are the expected ocaml externals for this job:

       external %s_job : %s -> %s Lwt_unix.job = \"lwt_unix_%s_job\"
       external %s_sync : %s -> %s = \"lwt_unix_%s_sync\"
*/
"
      fname
      (String.make (String.length fname) '-')
      prog_name
      Sys.argv.(0)
      job.name
      (c_type_name job.result.c_type)
      job.name
      (String.concat ", "
         (List.map
            (fun (name, dir, { c_type }) ->
              match dir with
                | In -> c_type_name c_type ^ " " ^ name
                | In_out | Out -> c_type_name (C_ptr c_type) ^ " " ^ name)
            job.params))
      job.name
      caml_arg_types
      (if is_tuple then
          "(" ^ caml_return_type ^ ")"
       else
          caml_return_type)
      job.name
      job.name
      caml_arg_types
      caml_return_type
      job.name;

    pr "\n";
    pr "/* Caml headers. */\n";
    pr "#define CAML_NAME_SPACE\n";
    pr "#include <lwt_unix.h>\n";
    List.iter (pr "#include <caml/%s.h>\n") ["memory"; "alloc"; "fail"; "signals"];

    let exists_if =
      String.concat " && "
        (List.map
           (fun (yes, var) ->
             if yes then
               sprintf "defined(%s)" var
             else
               sprintf "!defined(%s)" var)
           job.exists_if)
    in

    if job.exists_if <> [] then pr "\n#if %s\n" exists_if;

    pr "\n";
    pr "/* Specific headers. */\n";
    pr "#include <errno.h>\n";
    pr "#include <string.h>\n";
    List.iter (pr "#include <%s.h>\n") job.includes;

    let converters =
      List.filter
        (fun (name, _, { hint }) -> hint <> Hint_none)
        job.params
    in
    if converters <> [] then begin
      pr "
/* +-----------------------------------------------------------------+
   | Converters                                                      |
   +-----------------------------------------------------------------+ */
";
      List.iter
        (fun (name, _, atype) ->
          match atype.hint with
            | Hint_none ->
              ()
            | Hint_flag_list mapping | Hint_table mapping ->
              let name, cstrs =
                match atype.caml_type with
                  | Caml_list (Caml_variant (name, cstrs))
                  | Caml_variant (name, cstrs) -> (name, cstrs)
                  | _ -> assert false
              in
              let path, item = split_name name in
              pr "\n";
              pr "/* Table mapping constructors of ocaml type %s to C values. */\n" name;
              pr "static %s %s_table[] = {\n" (c_type_name atype.c_type) item;
              let rec loop l =
                match l with
                  | [] ->
                    ()
                  | (cstr, typ) :: l ->
                    assert (typ = []);
                    pr "  /* Constructor %s. */\n" cstr;
                    pr "  %s"
                      (find_map
                         (function
                           | (S name, _) when name = cstr -> Some name
                           | (D (name, name'), _) when name = cstr -> Some name'
                           | _ -> None)
                         mapping);
                    if l = [] then
                      pr "\n"
                    else begin
                      pr ",\n";
                      loop l
                    end
              in
              loop cstrs;
              pr "};\n";
              if (match atype.hint with Hint_flag_list _ -> true | _ -> false) then begin
                pr "\n";
                pr "/* Convert ocaml values of type %s to a C %s. */\n" name (c_type_name atype.c_type);
                pr "static %s %s_of_%ss(value list)\n" (c_type_name atype.c_type) (c_type_name atype.c_type) item;
                pr "{\n";
                pr "  %s result = 0;\n" (c_type_name atype.c_type);
                pr "  while (Is_block(list)) {\n";
                pr "    result |= %s_table[Int_val(Field(list, 0))];\n" item;
                pr "    list = Field(list, 1);\n";
                pr "  };\n";
                pr "  return result;\n";
                pr "}\n";
              end)
        converters
    end;

    pr "
/* +-----------------------------------------------------------------+
   | Asynchronous job                                                |
   +-----------------------------------------------------------------+ */
";

    pr "\n";
    pr "/* Structure holding informations for calling [%s]. */\n" job.name;
    pr "struct job_%s {\n" job.name;
    pr "  /* Informations used by lwt. It must be the first field of the structure. */\n";
    pr "  struct lwt_unix_job job;\n";
    if job.result.c_type <> C_void then begin
      pr "  /* This field store the result of the call. */\n";
      pr "  %s result;\n" (c_type_name job.result.c_type)
    end;
    if job.uerror = Uerror_errno then begin
      pr "  /* This field store the value of [errno] after the call. */\n";
      pr "  int errno_copy;\n"
    end;
    List.iter
      (fun (name, dir, { c_type }) ->
        pr "  /* %s parameter. */\n" (string_of_direction dir);
        pr "  %s %s;\n" (c_type_name c_type) name)
      job.params;
    if strings <> [] then begin
      pr "  /* Buffer for string parameters. */\n";
      pr "  char data[];\n"
    end;
    pr "};\n";

    gen_worker "";
    gen_result "";
    if map_out_64 || map_result_64 then
      Gen64.gen_result "_64";
    gen_job_stub "" "" "";

    if map_in_64 || map_out_64 || map_result_64 then
      Gen64.gen_job_stub "_64" "" (if map_out_64 || map_result_64 then "_64" else "");

    if job.exists_if <> [] then begin
      pr "\n";
      (* XXX: hack for fsync *)
      if job.name <> "fsync" then begin
        pr "#else /* %s */\n" exists_if;
        pr "\n";
        pr "CAMLprim value lwt_unix_%s_job(value Unit)\n" job.name;
        pr "{\n";
        pr "  lwt_unix_not_available(%S);\n" job.name;
        pr "  return Val_unit;\n";
        pr "}\n";
        pr "\n";
        if map_in_64 || map_out_64 || map_result_64 then begin
          pr "CAMLprim value lwt_unix_%s_64_job(value Unit)\n" job.name;
          pr "{\n";
          pr "  lwt_unix_not_available(%S);\n" job.name;
          pr "  return Val_unit;\n";
          pr "}\n";
          pr "\n";
        end;
      end;
      pr "#endif /* %s */\n" exists_if
    end;

    gen_caml "";
    if map_in_64 || map_out_64 || map_result_64 then Gen64.gen_caml "_64"
end

module GenFake = struct
  let gen_worker _ = assert false
  let gen_result _ = assert false
  let gen_job_stub _ _ _ = assert false
  let gen_sync_stub _ = assert false
  let gen_file _ = assert false
  let gen_caml _ = assert false
end

let gen job =
  let fname = "lwt_unix_job_" ^ job.name ^ ".c" in
  let oc = open_out ("src/unix/jobs-unix/" ^ fname) in
  let job64 = {
    job with
      params = (
        List.map
          (fun (name, dir, atype) ->
             if List.mem name job.map64 then
               (name, dir, { atype with caml_type = Caml_int64 })
             else
               (name, dir, atype))
          job.params
      );
      result = if List.mem "result" job.map64 then { job.result with caml_type = Caml_int64 } else job.result;
  } in
  let module Gen64 =
    MakeGen(GenFake)(struct
                       let fname = fname
                       let oc = oc
                       let job = job64
                     end)
  in
  let module Gen =
    MakeGen(Gen64)(struct
                     let fname = fname
                     let oc = oc
                     let job = job
                   end)
  in
  Gen.gen_file ();
  close_out oc

let rec collect_jobs map = function
  | Job job ->
      if StringMap.mem job.name map then begin
        log "job '%s' is defined two times" job.name;
        exit 1
      end;
      StringMap.add job.name { job with exists_if = (false, "LWT_ON_WINDOWS") :: job.exists_if } map
  | Seq l ->
      List.fold_left collect_jobs map l

let jobs = collect_jobs StringMap.empty jobs

let () =
  match Sys.argv with
    | [|_|] ->
        let fname = "lwt_unix_jobs_generated.ml" in
        ml_oc := open_out ("src/unix/" ^ fname);
        let pr_header oc fname =
          fprintf oc "\
(*
 * %s
 * %s
 *
 * File generated with %s
 *)

module type Job = sig
  type 'a t
end

"
            fname
            (String.make (String.length fname) '-')
            prog_name;
        in
        pr_header !ml_oc fname;
        fprintf !ml_oc "\
module Make(Job : Job) = struct
";
        StringMap.iter (fun name job -> gen job) jobs;
        output_string !ml_oc "end\n";
        close_out !ml_oc
    | [|_; "list-job-files"|] ->
        StringMap.iter (fun name job -> printf "src/unix/jobs-unix/lwt_unix_job_%s.c\n" name) jobs
    | [|_; "list-job-names"|] ->
        StringMap.iter (fun name job -> printf "%s\n" name) jobs
    | _ ->
        log "invalid arguments";
        eprintf "usage: %s [list-job-files|list-job-names]\n" prog_name;
        exit 2
