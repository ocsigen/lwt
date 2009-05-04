(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_read_line
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

open Lwt
open Lwt_io
open Lwt_term

type edition_state = Text.t * Text.t
type history = Text.t list

type completion_result =
  | No_completion
  | Complete_with of edition_state
  | Possibilities of Text.t list

(* +----------+
   | Commands |
   +----------+ *)

type command =
  | Unknown
  | Char of Text.t
  | Backward_delete_char
  | Forward_delete_char
  | Beginning_of_line
  | End_of_line
  | Complete
  | Kill_line
  | Accept_line
  | Backward_delete_word
  | Forward_delete_word
  | History_next
  | History_previous
  | Break
  | Clear_screen
  | Insert
  | Refresh
  | Backward_char
  | Forward_char
  | Set_mark

let get_command _ = get_key () >|= function
  | Key_up -> History_previous
  | Key_down -> History_next
  | Key_left -> Backward_char
  | Key_right -> Forward_char
  | Key_enter -> Accept_line
  | Key_home -> Beginning_of_line
  | Key_end -> End_of_line
  | Key_insert -> Insert
  | Key_backspace -> Backward_delete_char
  | Key_delete -> Forward_delete_char
  | Key_tab -> Complete
  | Key_control '@' -> Set_mark
  | Key_control 'a' -> Beginning_of_line
  | Key_control 'd' -> Break
  | Key_control 'e' -> End_of_line
  | Key_control 'i' -> Complete
  | Key_control 'k' -> Kill_line
  | Key_control 'l' -> Clear_screen
  | Key_control 'm' -> Accept_line
  | Key_control 'n' -> Backward_char
  | Key_control 'p' -> Forward_char
  | Key_control 'r' -> Refresh
  | Key_control 'w' -> Backward_delete_word
  | Key_control '?' -> Backward_delete_char
  | Key ch when Text.length ch = 1 -> Char ch
  | _ -> Unknown

(* Go-up by [n] lines then to the beginning of the line. Normally
   "\027[nF" does exactly this but for some terminal 1 need to be
   added... By the way we can relly on the fact that all terminal
   react the same way to "\027[F" which is to go to the beginning of
   the previous line: *)
let rec beginning_of_line = function
  | 0 ->
      write_char stdout "\r"
  | 1 ->
      write_text stdout "\027[F"
  | n ->
      write_text stdout "\027[F" >> beginning_of_line (n - 1)

(* +------------------+
   | Read-line engine |
   +------------------+ *)

let rec repeat f n =
  if n <= 0 then
    return ()
  else
    f () >> repeat f (n - 1)

let print_words oc cols words =
  let width = List.fold_left (fun x word -> max x (Text.length word)) 0 words + 1 in
  let columns = max 1 (cols / width) in
  let column_width = cols / columns in
  Lwt_util.fold_left
    (fun column word ->
       write_text oc word >>
         if column < columns then
           let len = Text.length word in
           if len < column_width then
             repeat (fun _ -> write_char oc " ") (column_width - len) >> return (column + 1)
           else
             return (column + 1)
         else
           write_text oc "\r\n" >> return 0)
    0 words >>= function
      | 0 -> return ()
      | _ -> write_text oc "\r\n"

type state = {
  before : Text.t;
  after : Text.t;
  hist_before : history;
  hist_after : history;
}

let render columns styled_text =
  let rec loop len = function
    | [] ->
        []
    | Text text :: l ->
        let buf = Buffer.create (Text.length text) in
        let len = Text.fold
          (fun ch len -> match ch with
             | "\r" ->
                 len
             | "\n" ->
                 let padding = columns - (len mod columns) in
                 Buffer.add_string buf (String.make padding ' ');
                 len + padding
             | ch ->
                 Buffer.add_string buf ch;
                 len + 1) text len in
        Text(Buffer.contents buf) :: loop len l
    | style :: l ->
        style :: loop len l
  in
  loop 0 styled_text

let compute_height columns len =
  if len = 0 then
    0
  else
    (len - 1) / columns

let real_read_line prompt history complete =
  let height_before = ref 0 and length = ref 0 in
  let rec loop state =
    print state >> get_command () >>= process_command state
  and print state =
    let col = Lwt_term.columns () in
    let before = render col (prompt @ [Reset; Text state.before]) in
    let total = render col (prompt @ [Reset; Text state.before; Text state.after]) in
    let total' = total @ [Text(String.make (max 0 (!length - styled_length total)) ' ')] in
    beginning_of_line !height_before
    >> printc total'
    >> beginning_of_line (compute_height col (styled_length total'))
    >> printc before
    >> begin
      height_before := compute_height col (styled_length before);
      length := styled_length total;
      if Text.ends_with state.before "\n" then begin
        incr height_before;
        printc [Text "\r\n"]
      end else
        return ()
    end
  and process_command state = function
    | Char ch ->
        loop { state with before = state.before ^ ch }

    | Unknown ->
        loop state

    | Backward_delete_char ->
        loop { state with before = Text.rchop state.before }

    | Forward_delete_char ->
        loop { state with after = Text.lchop state.after }

    | Beginning_of_line ->
        loop { state with
                 before = "";
                 after =  state.before ^ state.after }

    | End_of_line ->
        loop { state with
                 before = state.before ^ state.after;
                 after = "" }

    | Complete ->
        let t_complete = complete (state.before, state.after)
        and t_command = get_command () in
        (* Let the completion and user input run: *)
        choose [(t_complete >>= fun c -> return (`Completion c));
                (t_command >>= fun c -> return (`Command c))]
        >>= begin function
          | `Command command ->
              (* The user continued to type, drop completion *)
              process_command state command
          | `Completion No_completion ->
              t_command >>= process_command state
          | `Completion (Complete_with(before, after)) ->
              let state = { state with before = before; after = after } in
              print state >> t_command >>= process_command state
          | `Completion (Possibilities words) ->
              write_text stdout "\r\n"
              >> let col = Lwt_term.columns () in print_words stdout col words
              >> write_char stdout "\n"
              >> print state
              >> t_command >>= process_command state
        end

    | Accept_line ->
        let { columns = col } = size ()
        and line = state.before ^ state.after in
        printlc (render col [Text state.after])
        >> return line

    | Kill_line ->
        loop { state with after = "" }

    | Backward_delete_word ->
        loop state

    | History_previous ->
        begin match state.hist_before with
          | [] ->
              loop state
          | x :: l ->
              loop { before = x;
                     after = "";
                     hist_before = l;
                     hist_after = (state.before ^ state.after) :: state.hist_after }
        end

    | History_next ->
        begin match state.hist_after with
          | [] ->
              loop state
          | x :: l ->
              loop { before = x;
                     after = "";
                     hist_before = (state.before ^ state.after) :: state.hist_before;
                     hist_after = l }
        end

    | Backward_char ->
        if state.before = "" then
          loop state
        else
          loop { state with
                   before = Text.rchop state.before;
                   after = Text.get state.before (-1) ^ state.after }

    | Forward_char ->
        if state.after = "" then
          loop state
        else
          loop { state with
                   before = state.before ^ (Text.get state.after 0);
                   after = Text.lchop state.after }

    | Clear_screen ->
        clear_screen () >> loop state

    | Break ->
        fail End_of_file

    | _ ->
        loop state
  in
  loop { before = "";
         after = "";
         hist_before = history;
         hist_after = [] }

let read_line ?(history=[]) ?(complete=fun _ -> return No_completion) prompt =
  with_raw_mode (fun _ -> real_read_line prompt history complete)

(* +---------+
   | History |
   +---------+ *)

let save_history name history =
 with_file ~mode:output name
   (fun oc ->
      Lwt_util.iter_serial
        (fun line -> write_text oc line >> write_char oc "\000")
        history)

let load_line ic =
  let buf = Buffer.create 42 in
  let rec loop = function
    | "" | "\000" ->
        let str = Buffer.contents buf in
        begin match Text.check str with
          | Some _ ->
              fail (Failure "invalid UTF-8 strings in history")
          | None ->
              return(Some str)
        end
    | ch ->
        lwt ch = read_text ic 1 in
        Buffer.add_string buf ch;
        loop ch
  in
  read_text ic 1 >>= function
    | "" -> return None
    | ch -> Buffer.add_string buf ch; loop ch

let rec load_lines ic =
  load_line ic >>= function
    | Some line ->
        lwt lines = load_lines ic in
        return (line :: lines)
    | None ->
        return []

let load_history name =
  match try Some(open_file ~mode:input name) with _ -> None with
    | Some ic ->
        try_lwt
          load_lines ic
        finally
          close ic
    | None ->
        return []

(* +------+
   | Misc |
   +------+ *)

let common_prefix a b =
  let lena = String.length a and lenb = String.length b in
  let rec loop i =
    if i = lena || i = lenb || (a.[i] <> b.[i]) then
      String.sub a 0 i
    else
      loop (i + 1)
  in
  loop 0

let complete before word after words =
  match List.filter (fun word' -> Text.starts_with word' word) words with
    | [] ->
        No_completion
    | [word] ->
        Complete_with(before ^ word ^ " ", after)
    | word :: words ->
        let common_prefix = List.fold_left common_prefix word words in
        if String.length common_prefix > String.length word then
          Complete_with(before ^ common_prefix, after)
        else
          Possibilities(List.sort compare (word :: words))
