(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2010 Jérémie Dimino <jeremie@dimino.org>
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
{exception Parse_error}

let space = [' ' '\t' '\n']
let pattern = [^ ' ' '\t' '\n']+
let level =  ['a'-'z' 'A'-'Z']+

rule rules = parse
  | space* (pattern as pattern) space* "->" space* (level as level)
     { (pattern, level) :: semi_colon_and_rules lexbuf }
  | space* (level as level)
     { ("*", level) :: semi_colon_and_rules lexbuf }
  | space* eof
     { [] }
  | ""
     { raise Parse_error }

and semi_colon_and_rules = parse
  | space* ";"
     { rules lexbuf }
  | space* eof
     { [] }
  | ""
     { raise Parse_error }

{ let rules buf =
  try
    Some (rules buf)
  with Parse_error -> None }
