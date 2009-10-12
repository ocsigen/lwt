(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_ocaml_completion
 * Copyright (C) 2009 Jérémie Dimino
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

{
  open Toploop
  open Lwt
  open Lwt_read_line

  let keywords = [
    "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do";
    "done"; "downto"; "else"; "end"; "exception"; "external"; "false";
    "for"; "fun"; "function"; "functor"; "if"; "in"; "include";
    "inherit"; "initializer"; "lazy"; "let"; "match"; "method"; "module";
    "mutable"; "new"; "object";  "of";  "open"; "private";  "rec"; "sig";
    "struct";  "then";  "to";  "true";  "try";  "type";  "val"; "virtual";
    "when"; "while"; "with"; "try_lwt"; "finally"; "for_lwt"; "lwt";
  ]

  let get_directives () =
    Hashtbl.fold (fun k v l -> k :: l) Toploop.directive_table []

  let list_env = ref (fun () -> [])

  let list_files filter fname =
    let dir = Filename.dirname fname and l = ref [] in
    Array.iter (fun name ->
                  let name = Filename.concat dir name in
                  if try Sys.is_directory name with _ -> false then
                    l := Filename.concat name "" :: !l
                  else if filter name then
                    l := name :: !l) (Sys.readdir (if dir = "" then Filename.current_dir_name else dir));
  !l

  let list_directories fname =
    let dir = Filename.dirname fname and l = ref [] in
    Array.iter (fun name ->
                  let name = Filename.concat dir name in
                  if try Sys.is_directory name with _ -> false then
                    l := name :: !l) (Sys.readdir (if dir = "" then Filename.current_dir_name else dir));
    !l
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let digit = ['0'-'9']
let alnum = alpha | digit
let punct = ['!' '"' '#' '$' '%' '&' '\'' '(' ')' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '@' '[' '\\' ']' '^' '_' '`' '{' '|' '}' '~']
let graph = alnum | punct
let print = graph | ' '
let blank = ' ' | '\t'
let cntrl = ['\x00'-'\x1F' '\x7F']
let xdigit = digit | ['a'-'f' 'A'-'F']
let space = blank | ['\n' '\x0b' '\x0c' '\r']

let uchar = ['\x00' - '\x7f'] | _ [ '\x80' - '\xbf' ]*

let identstart = [ 'A'-'Z' 'a'-'z' '_' ]
let identbody = [ 'A'-'Z' 'a'-'z' '_' '\'' '0' - '9' ]
let ident = identstart identbody*
let maybe_ident = "" | ident

(* Parse a line of input. [before] correspond to the input before the
   cursor and [after] to the input after the cursor. The lexing buffer
   is created from [before]. *)

rule complete_input before after = parse

  (* Completion over directives *)
  | (blank* '#' blank* as before') (maybe_ident as dir) (blank* as bl) eof
      {
        return (match lookup dir (get_directives ()) with
                  | (_, []) ->
                      { comp_state = (before, after);
                        comp_words = [] }
                  | (_, [dir]) ->
                      let before = before' ^ dir in
                      (match Hashtbl.find directive_table dir with
                         | Directive_none _ ->
                             { comp_state = (before ^ ";;", after);
                               comp_words = [] }
                         | Directive_string _ ->
                             { comp_state = (before ^ (if bl = "" then " \"" else "\""), after);
                               comp_words = [] }
                         | Directive_bool _ ->
                             { comp_state = ((if bl = "" then before ^ " " else ""), after);
                               comp_words = ["false"; "true"] }
                         | Directive_int _ | Directive_ident _ ->
                             { comp_state = ((if bl = "" then before ^ " " else ""), after);
                               comp_words = ["false"; "true"] })
                  | (prefix, words) ->
                      if bl = "" then
                        { comp_state = (before' ^ prefix, after);
                          comp_words = words }
                      else
                        { comp_state = (before, after);
                          comp_words = [] })
      }

  (* Completion on directive argument *)
  | (blank* '#' blank* (ident as dir) blank* as before') (ident as arg) eof
      {
        return (match try Some(Hashtbl.find directive_table dir) with Not_found -> None with
                  | Some (Directive_bool _) ->
                      complete ~suffix:";;" before' arg after ["false"; "true"]
                  | _ ->
                      { comp_state = (before, after);
                        comp_words = [] })
      }

  (* Completion on packages *)
  | (blank* '#' blank* "require" blank* '"' as before) ([^'"']* as package) eof
      {
        return (complete ~suffix:"\";;" before package after (Fl_package_base.list_packages ()))
      }

  (* Completion on files *)
  | (blank* '#' blank* "load" blank* '"' as before) ([^'"']* as fname) eof
      {
        let list = list_files (fun name ->
                                 Filename.check_suffix name ".cma" || Filename.check_suffix name ".cmo") fname in
        return (complete ~suffix:"" before fname after list)
      }

  | (blank* '#' blank* "use" blank* '"' as before) ([^'"']* as fname) eof
      {
        let list = list_files (fun _ -> true) fname in
        return (complete ~suffix:"" before fname after list)
      }

  (* Completion on directories *)
  | (blank* '#' blank* "directory" blank* '"' as before) ([^'"']* as fname) eof
      {
        let list = list_directories fname in
        return (complete ~suffix:"" before fname after list)
      }

  (* Completion on packages *)
  | blank* '#' blank* ident blank* '"' [^'"']* '"' blank* eof
      {
        return { comp_state = (before ^ ";;", after);
                 comp_words = [] }
      }

  (* A line that do not need to be completed: *)
  | blank* '#' blank* ident blank* '"' [^'"']* '"' blank* ";;" eof
      {
        return { comp_state = (before, after);
                 comp_words = [] }
      }

  | ""
      {
        complete_end (Buffer.create (String.length before)) after lexbuf
      }

and complete_end before after = parse

  (* Completion on keywords *)
  | ((ident '.')* maybe_ident as id) eof
      {
        let before = Buffer.contents before in
        return (match Text.split ~sep:"." id with
                  | [] ->
                      complete ~suffix:"" before "" after keywords
                  | [id] ->
                      complete ~suffix:"" before id after (keywords @ !list_env ())
                  | _ ->
                      complete ~suffix:"" before id after (!list_env ()))
      }

  | uchar as ch
      {
        Buffer.add_string before ch;
        complete_end before after lexbuf
      }

  | ""
      {
        return { comp_state = (Buffer.contents before, after);
                 comp_words = [] }
      }
