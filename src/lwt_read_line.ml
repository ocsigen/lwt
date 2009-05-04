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
type prompt = Lwt_term.styled_text
type clipboard = Text.t ref

let clipboard = ref ""

exception Interrupt

(* +------------+
   | Completion |
   +------------+ *)

type completion_result =
  | No_completion
  | Complete_with of edition_state
  | Possibilities of Text.t list

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
  | Yank
  | Kill_ring_save

let get_command _ = read_key () >|= function
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
  | Key_control 'w' -> Kill_ring_save
  | Key_control 'y' -> Yank
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

(* The normal state is when selection is not active: *)
type normal_state = {
  before : Text.t;
  (* The text before the cursor *)
  after : Text.t;
  (* The text after the cursor *)
}

type selection_state = {
  text : Text.t;
  (* The complete text *)
  cursor : Text.pointer;
  (* The position of the cursor *)
  mark : Text.pointer;
  (* The position of the mark *)
}

type mode =
  | Normal of normal_state
  | Selection of selection_state

type engine_state = {
  mode : mode;
  hist_before : history;
  hist_after : history;
}

let engine_init history = {
  mode = Normal { before = ""; after = "" };
  hist_before = history;
  hist_after = [];
}

let all_input engine_state = match engine_state.mode with
  | Normal st -> st.before ^ st.after
  | Selection st -> st.text

let input_before engine_state = match engine_state.mode with
  | Normal st -> st.before
  | Selection st -> Text.chunk (Text.pointer_l st.text) st.cursor

let input_after engine_state = match engine_state.mode with
  | Normal st -> st.after
  | Selection st -> Text.chunk st.cursor (Text.pointer_r st.text)

(* Reset the mode to the normal mode: *)
let reset_selection state = match state.mode with
  | Normal _ ->
      state
  | Selection st ->
      { state with mode = Normal{ before = Text.chunk (Text.pointer_l st.text) st.cursor;
                                  after = Text.chunk st.cursor (Text.pointer_r st.text) } }

(* Update the abstract state without consideration or rendering,
   completion, ... *)
let rec engine_update state clipboard cmd =
  (* Helpers for updating the mode state only: *)
  let normal st = { state with mode = Normal st } and selection st = { state with mode = Selection st } in
  match state.mode with
    | Selection sel_st ->
        (* Change the cursor position: *)
        let maybe_set_cursor = function
          | Some(_, ptr) ->
              selection { sel_st with cursor = ptr }
          | None ->
              state
        in

        begin match cmd with
          | Unknown ->
              state

          | Forward_char ->
              maybe_set_cursor (Text.next sel_st.cursor)

          | Backward_char ->
              maybe_set_cursor (Text.prev sel_st.cursor)

          | Beginning_of_line ->
              selection { sel_st with cursor =  Text.pointer_l sel_st.text }

          | End_of_line ->
              selection { sel_st with cursor =  Text.pointer_r sel_st.text }

          | Kill_ring_save ->
              let a = min sel_st.cursor sel_st.mark and b = max sel_st.cursor sel_st.mark in
              clipboard := Text.chunk a b;
              normal { before = Text.chunk (Text.pointer_l sel_st.text) a;
                       after = Text.chunk b (Text.pointer_r sel_st.text) }

          | cmd ->
              (* If the user sent another command, reset the mode to
                 normal and process the command: *)
              engine_update (reset_selection state) clipboard cmd
        end

    | Normal norm_st ->
        begin match cmd with
          | Char ch ->
              normal { norm_st with before = norm_st.before ^ ch }

          | Set_mark ->
              let txt = norm_st.before ^ norm_st.after in
              let ptr = Text.pointer_at txt (Text.length norm_st.before) in
              selection { text = txt;
                          mark = ptr;
                          cursor = ptr }

          | Yank ->
              normal { norm_st with before = norm_st.before ^ !clipboard }

          | Backward_delete_char ->
              normal { norm_st with before = Text.rchop norm_st.before }

          | Forward_delete_char ->
              normal { norm_st with after = Text.lchop norm_st.after }

          | Beginning_of_line ->
              normal { before = "";
                       after =  norm_st.before ^ norm_st.after }

          | End_of_line ->
              normal { before = norm_st.before ^ norm_st.after;
                       after = "" }

          | Kill_line ->
              normal { norm_st with after = "" }

          | History_previous ->
              begin match state.hist_before with
                | [] ->
                    state
                | x :: l ->
                    { mode = Normal { before = x; after = "" };
                      hist_before = l;
                      hist_after = (norm_st.before ^ norm_st.after) :: state.hist_after }
              end

          | History_next ->
              begin match state.hist_after with
                | [] ->
                    state
                | x :: l ->
                    { mode = Normal { before = x; after = "" };
                      hist_before = (norm_st.before ^ norm_st.after) :: state.hist_before;
                      hist_after = l }
              end

          | Backward_char ->
              if norm_st.before = "" then
                state
              else
                normal { before = Text.rchop norm_st.before;
                         after = Text.get norm_st.before (-1) ^ norm_st.after }

          | Forward_char ->
              if norm_st.after = "" then
                state
              else
                normal { before = norm_st.before ^ (Text.get norm_st.after 0);
                         after = Text.lchop norm_st.after }

          | _ ->
              state
        end

(* +-----------+
   | Rendering |
   +-----------+ *)

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

type terminal_rendering_state = {
  trs_length : int;
  (* Length in characters of the complete printed text: the prompt,
     the input before the cursor and the input after the cursor.*)
  trs_height_before : int;
  (* The height of the complete text printed before the cursor: the
     prompt and the input before the cursor. *)
}

let trs_init = { trs_length = 0; trs_height_before = 0 }

(* Replace "\n" by padding to the end of line in a styled text.

   For example with 8 columns, ["toto\ntiti"] becomes
   ["toto    titi"].

   The goal of that is to erase all previous characters after
   end of lines.
*)
let prepare_for_display columns styled_text =
  let rec loop len = function
    | [] ->
        []
    | Text text :: l ->
        let buf = Buffer.create (Text.length text) in
        let len = Text.fold
          (fun ch len -> match ch with
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

(* Compute the number of row taken by a text given a number of
   columns: *)
let compute_height columns len =
  if len = 0 then
    0
  else
    (len - 1) / columns

(* Render the current state on the terminal, and returns the new
   terminal rendering state: *)
let terminal_update ?(map_text=fun x -> x) render_state engine_state prompt =
  (* Text before and after the cursor, according to the current mode: *)
  let before, after = match engine_state.mode with
    | Normal st -> ([Text(map_text st.before)], [Text(map_text st.after)])
    | Selection st ->
        let a = min st.cursor st.mark and b = max st.cursor st.mark in
        let part_before = [Text(map_text (Text.chunk (Text.pointer_l st.text) a))]
        and part_selected = [Underlined; Text(map_text (Text.chunk a b)); Reset]
        and part_after = [Text(map_text (Text.chunk (Text.pointer_r st.text) b))] in
        if st.cursor < st.mark then
          (part_before, part_selected @ part_after)
        else
          (part_before @ part_selected, part_after)
  in

  let columns = Lwt_term.columns () in

  (* All the text printed before the cursor: *)
  let printed_before = prepare_for_display columns (prompt @ [Reset] @ before) in

  (* The total printed text: *)
  let printed_total = prepare_for_display columns (prompt @ [Reset] @ before @ after) in

  (* The new rendering state: *)
  let new_render_state = {
    trs_height_before = compute_height columns (styled_length printed_before);
    trs_length = styled_length printed_total;
  } in

  (* The total printed text with any needed spaces after to erase all
     previous text: *)
  let printed_total_erase = printed_total @ [Text(String.make (max 0 (render_state.trs_length - styled_length printed_total)) ' ')] in

  (* Go back by the number of rows of the previous text: *)
  beginning_of_line render_state.trs_height_before

  (* Prints and erase everything: *)
  >> printc printed_total_erase

  (* Go back again to the beginning of printed text: *)
  >> beginning_of_line (compute_height columns (styled_length printed_total_erase))

  (* Prints again the text before the cursor, to put the cursor at the
     right place: *)
  >> printc printed_before

  >> begin
    (* Prints another newline to avoid having the cursor displayed at
       the end of line: *)
    if Text.ends_with (input_before engine_state) "\n" then
      printlc [] >> return { new_render_state with trs_height_before = new_render_state.trs_height_before + 1 }
    else
      return new_render_state
  end

let terminal_finish ?(map_text=fun x -> x) render_state engine_state prompt =
  beginning_of_line render_state.trs_height_before
  >> printlc (prepare_for_display (Lwt_term.columns ()) (prompt @ [Reset; Text(map_text(all_input engine_state))]))

let read_line ?(history=[]) ?(complete=fun _ -> return No_completion) ?(clipboard=clipboard) prompt =
  let rec process_command render_state engine_state = function
    | Clear_screen ->
        clear_screen () >> redraw trs_init engine_state

    | Refresh ->
        redraw render_state engine_state

    | Accept_line ->
        terminal_finish render_state engine_state prompt
        >> return (all_input engine_state)

    | Break ->
        terminal_finish render_state engine_state prompt
        >> fail Interrupt

    | Complete ->
        let engine_state = reset_selection engine_state in
        let t_complete = complete (input_before engine_state, input_after engine_state)
        and t_command = get_command () in
        (* Let the completion and user input run in parallel: *)
        choose [(t_complete >>= fun c -> return (`Completion c));
                (t_command >>= fun c -> return (`Command c))]
        >>= begin function
          | `Command command ->
              (* The user continued to type, drop completion *)
              process_command render_state engine_state command
          | `Completion No_completion ->
              t_command >>= process_command render_state engine_state
          | `Completion(Complete_with(before, after)) ->
              let engine_state = { engine_state with mode = Normal{ before = before; after = after } } in
              lwt render_state = terminal_update render_state engine_state prompt in
              t_command >>= process_command render_state engine_state
          | `Completion(Possibilities words) ->
                write_text stdout "\r\n"
                >> print_words stdout (Lwt_term.columns ()) words
                >> write_char stdout "\n"
                >> (lwt render_state = terminal_update render_state engine_state prompt in
                    t_command >>= process_command render_state engine_state)
          end

      | cmd ->
          let new_state = engine_update engine_state clipboard cmd in
          (* Do not redraw if not needed: *)
          if new_state <> engine_state then
            redraw render_state new_state
          else
            loop render_state new_state

  and redraw render_state engine_state =
    lwt render_state = terminal_update render_state engine_state prompt in
    loop render_state engine_state

  and loop render_state engine_state =
    get_command () >>= process_command render_state engine_state
  in
  if Lazy.force stdin_is_atty && Lazy.force stdout_is_atty then
    with_raw_mode (fun _ -> redraw trs_init (engine_init history))
  else
    Lwt_io.read_line stdin

let read_password ?(clipboard=clipboard) ?(style=`text "*") prompt =
  (* Choose a mapping text function according to style: *)
  let map_text = match style with
    | `text ch -> (fun txt -> Text.map (fun _ -> ch) txt)
    | `clear -> (fun x -> x)
    | `empty -> (fun _ -> "") in
  let terminal_update rs es = terminal_update ~map_text rs es
  and terminal_finish rs es = terminal_finish ~map_text rs es in
  let rec process_command render_state engine_state = function
    | Clear_screen ->
        clear_screen () >> redraw trs_init engine_state

    | Refresh ->
        redraw render_state engine_state

    | Accept_line ->
        terminal_finish render_state engine_state prompt
        >> return (all_input engine_state)

    | Break ->
        terminal_finish render_state engine_state prompt
        >> fail Interrupt

    | cmd ->
        let new_state = engine_update engine_state clipboard cmd in
        if new_state <> engine_state then
          redraw render_state new_state
        else
          loop render_state new_state

  and redraw render_state engine_state =
    lwt render_state = terminal_update render_state engine_state prompt in
    loop render_state engine_state

  and loop render_state engine_state =
    get_command () >>= process_command render_state engine_state

  in
  if not (Lazy.force stdin_is_atty && Lazy.force stdout_is_atty) then
    fail (Failure "Lwt_read_line.read_password: not running in a terminal")
  else
    with_raw_mode (fun _ ->  Lwt_stream.junk_old standard_input >> redraw trs_init (engine_init []))

let read_keyword ?(history=[]) prompt keywords =
  let rec process_command render_state engine_state = function
    | Clear_screen ->
        clear_screen () >> redraw trs_init engine_state

    | Refresh ->
        redraw render_state engine_state

    | Accept_line ->
        begin match try Some(List.assoc (all_input engine_state) keywords) with Not_found -> None with
          | Some value ->
              terminal_finish render_state engine_state prompt
              >> return value
          | None ->
              loop render_state engine_state
        end

    | Break ->
        terminal_finish render_state engine_state prompt
        >> fail Interrupt

    | Complete ->
        let engine_state = reset_selection engine_state in
        let txt = input_before engine_state in
        begin match List.filter (fun (kwd, _) -> Text.starts_with kwd txt) keywords with
          | [(kwd, _)] ->
              redraw render_state { engine_state with mode = Normal{ before = kwd; after = "" } }
          | _ ->
              loop render_state engine_state
        end

    | cmd ->
        let new_state = engine_update engine_state clipboard cmd in
        if new_state <> engine_state then
          redraw render_state new_state
        else
          loop render_state new_state

  and redraw render_state engine_state =
    lwt render_state = terminal_update render_state engine_state prompt in
    loop render_state engine_state

  and loop render_state engine_state =
    get_command () >>= process_command render_state engine_state
  in
  if Lazy.force stdin_is_atty && Lazy.force stdout_is_atty then
    with_raw_mode (fun _ -> redraw trs_init (engine_init history))
  else
    lwt txt = Lwt_io.read_line stdin in
    match try Some(List.assoc txt keywords) with Not_found -> None with
      | Some value ->
          return value
      | None ->
          fail (Failure "Lwt_read_line.read_keyword: invalid input")

let read_yes_no ?history prompt =
  read_keyword ?history prompt [("yes", true); ("y", true); ("no", false); ("n", false)]

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
