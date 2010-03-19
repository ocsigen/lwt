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
open Lwt_text
open Lwt_term

module TextSet = Set.Make(Text)
type text_set = TextSet.t

type edition_state = Text.t * Text.t
type history = Text.t list
type prompt = Lwt_term.styled_text
type password_style = [ `empty | `clear | `text of Text.t ]

class clipboard =
  let signal, setter = React.S.create "" in
object
  method set text = setter text
  method contents = signal
end

let clipboard = new clipboard

exception Interrupt

(* +-----------------------------------------------------------------+
   | Completion                                                      |
   +-----------------------------------------------------------------+ *)

type completion_mode = [ `classic | `real_time | `none ]

type completion_result = {
  comp_state : edition_state;
  comp_words : text_set;
}

type completion = edition_state -> completion_result Lwt.t

let no_completion state = return {
  comp_state = state;
  comp_words = TextSet.empty;
}

let common_prefix a b =
  let lena = String.length a and lenb = String.length b in
  let rec loop i =
    if i = lena || i = lenb || (a.[i] <> b.[i]) then
      String.sub a 0 i
    else
      loop (i + 1)
  in
  loop 0

let lookup word words =
  let words = TextSet.filter (fun word' -> Text.starts_with word' word) words in
  if TextSet.is_empty words then
    ("", TextSet.empty)
  else
    (TextSet.fold common_prefix words (TextSet.choose words), words)

let complete ?(suffix=" ") before word after words =
  let prefix, words = lookup word words in
  match TextSet.cardinal words with
    | 0 ->
        { comp_state = (before ^ word, after);
          comp_words = TextSet.empty }
    | 1 ->
        { comp_state = (before ^ prefix ^ suffix, after);
          comp_words = words }
    | _ ->
        { comp_state = (before ^ prefix, after);
          comp_words = words }

(* +-----------------------------------------------------------------+
   | Commands                                                        |
   +-----------------------------------------------------------------+ *)

module Command =
struct

  type t =
    | Nop
    | Char of Text.t
    | Backward_delete_char
    | Forward_delete_char
    | Beginning_of_line
    | End_of_line
    | Complete
    | Meta_complete
    | Kill_line
    | Backward_kill_line
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
    | Paste
    | Copy
    | Cut
    | Uppercase
    | Lowercase
    | Capitalize
    | Backward_word
    | Forward_word
    | Backward_search
    | Complete_left
    | Complete_right
    | Complete_up
    | Complete_down
    | Complete_first
    | Complete_last
    | Undo

  let names = [
    (Nop, "nop");
    (Backward_delete_char, "backward-delete-char");
    (Forward_delete_char, "forward-delete-char");
    (Beginning_of_line, "beginning-of-line");
    (End_of_line, "end-of-line");
    (Complete, "complete");
    (Meta_complete, "meta-complete");
    (Kill_line, "kill-line");
    (Backward_kill_line, "backward-kill-line");
    (Accept_line, "accept-line");
    (Backward_delete_word, "backward-delete-word");
    (Forward_delete_word, "forward-delete-word");
    (History_next, "history-next");
    (History_previous, "history-previous");
    (Break, "break");
    (Clear_screen, "clear-screen");
    (Insert, "insert");
    (Refresh, "refresh");
    (Backward_char, "backward-char");
    (Forward_char, "forward-char");
    (Set_mark, "set-mark");
    (Paste, "paste");
    (Copy, "copy");
    (Cut, "cut");
    (Uppercase, "uppercase");
    (Lowercase, "lowercase");
    (Capitalize, "capitalize");
    (Backward_word, "backward-word");
    (Forward_word, "forward-word");
    (Complete_left, "complete-left");
    (Complete_right, "complete-right");
    (Complete_up, "complete-up");
    (Complete_down, "complete-down");
    (Backward_search, "backward-search");
    (Complete_first, "complete-first");
    (Complete_last, "complete-last");
    (Undo, "undo");
  ]

  let to_string = function
    | Char ch ->
        Printf.sprintf "Char %S" ch
    | command ->
        let rec search = function
          | (command', name) :: _ when command = command' ->
              name
          | _ :: rest ->
              search rest
          | [] ->
              assert false
        in
        search names

  let of_string name =
    let rec search = function
      | (command, name') :: _ when name = name' ->
          command
      | _ :: rest ->
          search rest
      | [] ->
          failwith "Lwt_read_line.Command.of_stirng: cannot convert string to command"
    in
    search names

  let of_key = function
    | Key_up -> History_previous
    | Key_down -> History_next
    | Key_left -> Backward_char
    | Key_right -> Forward_char
    | Key_home -> Beginning_of_line
    | Key_end -> End_of_line
    | Key_insert -> Insert
    | Key_delete -> Forward_delete_char
    | Key_control '@' -> Set_mark
    | Key_control 'a' -> Beginning_of_line
    | Key_control 'd' -> Break
    | Key_control 'e' -> End_of_line
    | Key_control 'h' -> Backward_delete_word
    | Key_control 'i' -> Complete
    | Key_control 'j' -> Accept_line
    | Key_control 'k' -> Kill_line
    | Key_control 'l' -> Clear_screen
    | Key_control 'm' -> Accept_line
    | Key_control 'n' -> Backward_char
    | Key_control 'p' -> Forward_char
    | Key_control 'r' -> Backward_search
    | Key_control 'u' -> Backward_kill_line
    | Key_control 'w' -> Cut
    | Key_control 'y' -> Paste
    | Key_control '_' -> Undo
    | Key_control '?' -> Backward_delete_char
    | Key "\027u" -> Uppercase
    | Key "\027l" -> Lowercase
    | Key "\027c" -> Capitalize
    | Key ("\027Oc" | "\027[1;5C") -> Forward_word
    | Key ("\027Od" | "\027[1;5D") -> Backward_word
    | Key ("\027\027[A" | "\027[1;3A") -> Complete_up
    | Key ("\027\027[B" | "\027[1;3B") -> Complete_down
    | Key ("\027\027[C" | "\027[1;3C") -> Complete_right
    | Key ("\027\027[D" | "\027[1;3D") -> Complete_left
    | Key ("\027\027[7~" | "\027[1;3H") -> Complete_first
    | Key ("\027\027[8~" | "\027[1;3F") -> Complete_last
    | Key ("\027\n" | "\194\141") -> Char "\n"
    | Key ("\027\t" | "\194\137") -> Meta_complete
    | Key ("\027w" | "\195\183") -> Copy
    | Key ("\027[3^" | "\027[3;5~") -> Forward_delete_word
    | Key ch when Text.length ch = 1 && Text.is_print ch -> Char ch
    | _ -> Nop
end

(* +-----------------------------------------------------------------+
   | Read-line engine                                                |
   +-----------------------------------------------------------------+ *)

module Engine =
struct
  open Command

  type selection_state = {
    sel_text : Text.t;
    sel_mark : Text.pointer;
    sel_cursor : Text.pointer;
  }

  type search_state = {
    search_word : Text.t;
    search_history : history;
    search_init_history : history;
  }

  type mode =
    | Edition of edition_state
    | Selection of selection_state
    | Search of search_state

  type state = {
    mode : mode;
    history : history * history;
  }

  let init history = {
    mode = Edition("", "");
    history = (history, []);
  }

  let all_input state = match state.mode with
    | Edition(before, after) ->
        before ^ after
    | Selection sel ->
        sel.sel_text
    | Search search ->
        match search.search_history with
          | [] ->
              ""
          | phrase :: _ ->
              phrase

  let edition_state state = match state.mode with
    | Edition(before, after) ->
        (before, after)
    | Selection sel ->
        (Text.chunk (Text.pointer_l sel.sel_text) sel.sel_cursor,
         Text.chunk sel.sel_cursor (Text.pointer_r sel.sel_text))
    | Search search ->
        match search.search_history with
          | [] ->
              ("", "")
          | phrase :: _ ->
              (phrase, "")

  (* Reset the mode to the edition mode: *)
  let reset state = match state.mode with
    | Edition _ ->
        state
    | Selection sel ->
        { state with mode = Edition(Text.chunk (Text.pointer_l sel.sel_text) sel.sel_cursor,
                                    Text.chunk sel.sel_cursor (Text.pointer_r sel.sel_text)) }
    | Search search ->
        { state with mode = Edition((match search.search_history with
                                       | [] ->
                                           ""
                                       | phrase :: _ ->
                                           phrase), "") }

  let split_first_word text =
    let rec find_last ptr =
      match Text.next ptr with
        | Some(ch, ptr) when Text.is_alnum ch ->
            find_last ptr
        | _ ->
            ptr
    in
    let rec find_first ptr =
      match Text.next ptr with
        | Some(ch, ptr') ->
            if Text.is_alnum ch then
              let ptr' = find_last ptr' in
              (Text.chunk (Text.pointer_l text) ptr,
               Text.chunk ptr ptr',
               Text.chunk ptr' (Text.pointer_r text))
            else
              find_first ptr'
        | None ->
            (text, "", "")
    in
    find_first (Text.pointer_l text)

  let split_last_word text =
    let rec find_first ptr =
      match Text.prev ptr with
        | Some(ch, ptr) when Text.is_alnum ch ->
            find_first ptr
        | _ ->
            ptr
    in
    let rec find_last ptr =
      match Text.prev ptr with
        | Some(ch, ptr') ->
            if Text.is_alnum ch then
              let ptr' = find_first ptr' in
              (Text.chunk (Text.pointer_l text) ptr',
               Text.chunk ptr' ptr,
               Text.chunk ptr (Text.pointer_r text))
            else
              find_last ptr'
        | None ->
            (text, "", "")
    in
    find_last (Text.pointer_r text)

  let rec update ~engine_state ?(clipboard=clipboard) ~command () =
    (* Helpers for updating the mode state only: *)
    let edition st = { engine_state with mode = Edition st }
    and selection st = { engine_state with mode = Selection st }
    and search st = { engine_state with mode = Search st } in
    match engine_state.mode with
      | Selection sel ->
          (* Change the cursor position: *)
          let maybe_set_cursor = function
            | Some(_, ptr) ->
                selection { sel with sel_cursor = ptr }
            | None ->
                engine_state
          in

          begin match command with
            | Nop ->
                engine_state

            | Forward_char ->
                maybe_set_cursor (Text.next sel.sel_cursor)

            | Backward_char ->
                maybe_set_cursor (Text.prev sel.sel_cursor)

            | Forward_word ->
                let rec skip ptr =
                  match Text.next ptr with
                    | Some(ch, ptr) ->
                        if Text.is_alnum ch then find ptr else skip ptr
                    | None ->
                        ptr
                and find ptr =
                  match Text.next ptr with
                    | Some(ch, ptr') ->
                        if Text.is_alnum ch then find ptr' else ptr
                    | None ->
                        ptr
                in
                selection { sel with sel_cursor = skip sel.sel_cursor }

            | Backward_word ->
                let rec skip ptr =
                  match Text.prev ptr with
                    | Some(ch, ptr) ->
                        if Text.is_alnum ch then find ptr else skip ptr
                    | None ->
                        ptr
                and find ptr =
                  match Text.prev ptr with
                    | Some(ch, ptr') ->
                        if Text.is_alnum ch then find ptr' else ptr
                    | None ->
                        ptr
                in
                selection { sel with sel_cursor = skip sel.sel_cursor }

            | Beginning_of_line ->
                selection { sel with sel_cursor =  Text.pointer_l sel.sel_text }

            | End_of_line ->
                selection { sel with sel_cursor =  Text.pointer_r sel.sel_text }

            | Copy ->
                let a = min sel.sel_cursor sel.sel_mark and b = max sel.sel_cursor sel.sel_mark in
                clipboard#set (Text.chunk a b);
                edition (Text.chunk (Text.pointer_l sel.sel_text) sel.sel_cursor,
                         Text.chunk sel.sel_cursor (Text.pointer_r sel.sel_text))

            | Cut ->
                let a = min sel.sel_cursor sel.sel_mark and b = max sel.sel_cursor sel.sel_mark in
                clipboard#set (Text.chunk a b);
                edition (Text.chunk (Text.pointer_l sel.sel_text) a,
                         Text.chunk b (Text.pointer_r sel.sel_text))

            | command ->
                (* If the user sent another command, reset the mode to
                   edition and process the command: *)
                update ~engine_state:(reset engine_state) ~clipboard ~command ()
          end

      | Edition(before, after) ->
          begin match command with
            | Char ch ->
                edition (before ^ ch, after)

            | Set_mark ->
                let txt = before ^ after in
                let ptr = Text.pointer_at txt (Text.length before) in
                selection { sel_text = txt;
                            sel_mark = ptr;
                            sel_cursor = ptr }

            | Paste ->
                edition (before ^ (React.S.value clipboard#contents), after)

            | Backward_delete_char ->
                edition (Text.rchop before, after)

            | Forward_delete_char ->
                edition (before, Text.lchop after)

            | Beginning_of_line ->
                edition ("", before ^ after)

            | End_of_line ->
                edition (before ^ after, "")

            | Kill_line ->
                clipboard#set after;
                edition (before, "")

            | Backward_kill_line ->
                clipboard#set before;
                edition ("", after)

            | History_previous ->
                begin match engine_state.history with
                  | ([], _) ->
                      engine_state
                  | (line :: hist_before, hist_after) ->
                      { mode = Edition(line, "");
                        history = (hist_before, (before ^ after) :: hist_after) }
                end

            | History_next ->
                begin match engine_state.history with
                  | (_, []) ->
                      engine_state
                  | (hist_before, line :: hist_after) ->
                      { mode = Edition(line, "");
                        history = ((before ^ after) :: hist_before, hist_after) }
                end

            | Backward_char ->
                if before = "" then
                  engine_state
                else
                  edition (Text.rchop before,
                           Text.get before (-1) ^ after)

            | Forward_char ->
                if after = "" then
                  engine_state
                else
                  edition (before ^ (Text.get after 0),
                           Text.lchop after)

            | Uppercase ->
                let a, b, c = split_first_word after in
                edition (before ^ a ^ Text.upper b, c)

            | Lowercase ->
                let a, b, c = split_first_word after in
                edition (before ^ a ^ Text.lower b, c)

            | Capitalize ->
                let a, b, c = split_first_word after in
                edition (before ^ a ^ Text.capitalize (Text.lower b), c)

            | Backward_word ->
                let a, b, c = split_last_word before in
                edition (a, b ^ c ^ after)

            | Forward_word ->
                let a, b, c = split_first_word after in
                edition (before ^ a ^ b, c)

            | Backward_delete_word ->
                let a, b, c = split_last_word before in
                edition (a, c ^ after)

            | Forward_delete_word ->
                let a, b, c = split_first_word after in
                edition (before ^ a, c)

            | Backward_search ->
                let hist_before, hist_after = engine_state.history in
                let history = List.rev_append hist_after ((before ^ after) :: hist_before) in
                search { search_word = "";
                         search_history = history;
                         search_init_history = history }

            | _ ->
                engine_state
          end

      | Search st ->
          let lookup word history =
            let rec aux history = match history with
              | [] ->
                  []
              | phrase :: rest ->
                  if Text.contains phrase word then
                    history
                  else
                    aux rest
            in
            aux history
          in

          begin match command with
            | Char ch ->
                let word = st.search_word ^ ch in
                search {
                  st with
                    search_word = word;
                    search_history = lookup word st.search_history;
                }

            | Backward_search ->
                search {
                  st with
                    search_history = match st.search_history with
                      | [] -> []
                      | _ :: rest -> lookup st.search_word rest
                }

            | Backward_delete_char ->
                if st.search_word <> "" then
                  let word = Text.rchop st.search_word in
                  search {
                    st with
                      search_word = word;
                      search_history = lookup word st.search_init_history;
                  }
                else
                  search st

            | cmd ->
                let phrase = match st.search_history with
                  | [] -> ""
                  | phrase :: _ -> phrase
                in
                edition (phrase, "")
          end
end

(* +-----------------------------------------------------------------+
   | Rendering                                                       |
   +-----------------------------------------------------------------+ *)

let rec repeat f n =
  if n <= 0 then
    return ()
  else
    lwt () = f () in
    repeat f (n - 1)

let print_words oc screen_width words = match List.filter ((<>) "") words with
  | [] ->
      return ()
  | words ->
      let max_width = List.fold_left (fun x word -> max x (Text.length word)) 0 words + 1 in
      let count = List.length words in
      let columns = max 1 (screen_width / max_width) in
      let lines =
        if count < columns then
          1
        else
          let l = count / columns in
          if columns mod count = 0 then l else l + 1
      in
      let column_width = screen_width / columns in
      let m = Array.make_matrix lines columns "" in
      let rec fill_display line column = function
        | [] ->
            ()
        | word :: words ->
            m.(line).(column) <- word;
            let line = line + 1 in
            if line < lines then
              fill_display line column words
            else
              fill_display 0 (column + 1) words
      in
      fill_display 0 0 words;
      for_lwt line = 0 to lines - 1 do
        lwt () =
          for_lwt column = 0 to columns - 1 do
            let word = m.(line).(column) in
            lwt () = write oc word in
            let len = Text.length word in
            if len < column_width then
              repeat (fun () -> write_char oc " ") (column_width - len)
            else
              return ()
          done
        in
        write_char oc "\n"
      done

module Terminal =
struct
  open Engine
  open Command

  type state = {
    printed_before : styled_text;
    (* The text displayed before the cursor *)
    printed_after : styled_text;
    (* The text displayed after the cursor *)
    box : bool;
    (* Tell whether a box is currently displayed *)
    display_start : int;
    (* For dynamic completion. It is the index of the first displayed word. *)
  }

  let init = { printed_before = []; printed_after = []; display_start = 0; box = false }

  type box =
    | Box_none
    | Box_empty
    | Box_words of text_set * int
    | Box_message of string

  let make_completion index columns words =
    let rec aux ofs idx = function
      | [] ->
          [Text(Text.repeat (columns - ofs) " ")]
      | word :: words ->
          let len = Text.length word in
          let ofs' = ofs + len in
          if ofs' <= columns then
            if idx = index then
              Inverse :: Text word :: Reset ::
                if ofs' + 1 > columns then
                  []
                else
                  Text "│" :: aux (ofs' + 1) (idx + 1) words
            else
              Text word ::
                if ofs' + 1 > columns then
                  []
                else
                  Text "│" :: aux (ofs' + 1) (idx + 1) words
          else
            [Text(Text.sub word 0 (columns - ofs))]
    in
    aux 0 0 words

  let make_bar delimiter columns words =
    let buf = Buffer.create (columns * 3) in
    let rec aux ofs = function
      | [] ->
          for i = ofs + 1 to columns do
            Buffer.add_string buf "─"
          done;
          Buffer.contents buf
      | word :: words ->
          let len = Text.length word in
          let ofs' = ofs + len in
          if ofs' <= columns then begin
            for i = 1 to len do
              Buffer.add_string buf "─"
            done;
            if ofs' + 1 > columns then
              Buffer.contents buf
            else begin
              Buffer.add_string buf delimiter;
              aux (ofs' + 1) words
            end
          end else begin
            for i = ofs + 1 to columns do
              Buffer.add_string buf "─"
            done;
            Buffer.contents buf
          end
    in
    aux 0 words

  let rec drop count l =
    if count <= 0 then
      l
    else match l with
      | [] -> []
      | e :: l -> drop (count - 1) l

  let rec goto_beginning_of_line = function
    | 0 ->
        [Text "\r"]
    | 1 ->
        [Text "\027[F"]
    | n ->
        Text "\027[F" :: goto_beginning_of_line (n - 1)

  let rec compute_position columns acc = function
    | [] ->
        acc
    | Text txt :: rest ->
        let acc = Text.fold (fun ch (column, line) ->
                               match ch with
                                 | "\n" ->
                                     (0, line + 1)
                                 | _ ->
                                     if column = columns then
                                       (1, line + 1)
                                     else
                                       (column + 1, line)) txt acc in
        compute_position columns acc rest
    | _ :: rest ->
        compute_position columns acc rest

  let _draw columns old_render_state new_render_state =

    let new_width_before, new_height_before =
      compute_position columns (0, 0) new_render_state.printed_before
    and old_width_before, old_height_before =
      compute_position columns (0, 0) old_render_state.printed_before in

    let new_render_state, new_width_before, new_height_before =
      (* If we terminates on the right margin, we add a "\n" to ensure
         that the cursor will be printed at the beginning of the next
         line: *)
      if new_width_before = columns then
        ({ new_render_state with printed_before = new_render_state.printed_before @ [Text "\n"] }, 0, new_height_before + 1)
      else
        (new_render_state, new_width_before, new_height_before)
    in

    let new_width_total, new_height_total =
      compute_position columns (new_width_before, new_height_before) new_render_state.printed_after
    and old_width_total, old_height_total =
      compute_position columns (old_width_before, old_height_before) old_render_state.printed_after in

    (* Produce a sequence erasing n lines: *)
    let rec eraser acc = function
      | 0 -> acc
      | n -> eraser (Text "\027[K\n" :: acc) (n - 1)
    in

    let text = List.flatten [
      (* Go back by the number of rows of the previous text: *)
      goto_beginning_of_line old_height_before;

      (* Erase all old contents: *)
      eraser [Text "\027[K"] old_height_total;

      (* Go back to the starting point: *)
      goto_beginning_of_line old_height_total;

      (* Print all new contents: *)
      new_render_state.printed_before;
      new_render_state.printed_after;

      (* Go back again to the beginning of printed text: *)
      goto_beginning_of_line new_height_total;

      (* Prints again the text before the cursor, to put the cursor at
         the right place: *)
      new_render_state.printed_before;
    ] in

    (text, new_render_state)

  (* Render the current state on the terminal, and returns the new
     terminal rendering state: *)
  let draw ~columns ?(map_text=fun x -> x) ?(box=Box_none) ~render_state ~engine_state ~prompt () =
    match engine_state.mode with
      | Search st ->
          let printed_before = Reset :: prompt @ [Reset; Text "(reverse-i-search)'"; Text st.search_word] in
          let printed_after = match st.search_history with
            | [] ->
                [Text "'"]
            | phrase :: _ ->
                let ptr_start = match Text.find phrase st.search_word with
                  | Some ptr ->
                      ptr
                  | None ->
                      (* The first phrase of st.search_history is a
                         phrase containing st.search_word, so this
                         case will never happen *)
                      assert false
                in
                let ptr_end = Text.move (Text.length st.search_word) ptr_start in
                [Text "': ";
                 Text(Text.chunk (Text.pointer_l phrase) ptr_start);
                 Underlined;
                 Text(Text.chunk ptr_start ptr_end);
                 Reset;
                 Text(Text.chunk ptr_end (Text.pointer_r phrase))]
          in
          _draw columns render_state { render_state with
                                         printed_before = printed_before;
                                         printed_after = printed_after }

      | _ ->
          (* Text before and after the cursor, according to the current mode: *)
          let before, after = match engine_state.mode with
            | Edition(before, after) ->
                ([Text(map_text before)], [Text(map_text after)])
            | Selection sel ->
                let a = min sel.sel_cursor sel.sel_mark and b = max sel.sel_cursor sel.sel_mark in
                let part_before = [Text(map_text (Text.chunk (Text.pointer_l sel.sel_text) a))]
                and part_selected = [Underlined; Text(map_text (Text.chunk a b)); Reset]
                and part_after = [Text(map_text (Text.chunk (Text.pointer_r sel.sel_text) b))] in
                if sel.sel_cursor < sel.sel_mark then
                  (part_before, part_selected @ part_after)
                else
                  (part_before @ part_selected, part_after)
            | Search _ ->
                assert false
          in

          (* All the text printed before the cursor: *)
          let printed_before = List.flatten [[Reset]; prompt; [Reset]; before] in

          match box with
            | Box_none ->
                _draw columns render_state
                  { render_state with
                      printed_before = printed_before;
                      printed_after = after;
                      box = false }

            | Box_message message ->
                let bar = Text(Text.repeat (columns - 2) "─") in
                let message_len = Text.length message in
                let message = if message_len + 2 > columns then Text.sub message 0 (columns - 2) else message in
                let printed_after =
                  after @
                    [Text "\n";
                     Text "┌"; bar; Text "┐\n";
                     Text "│"; Text message; Text(String.make (columns - 2 - message_len) ' '); Text "│\n";
                     Text "└"; bar; Text "┘"]
                in
                _draw columns render_state
                  { render_state with
                      printed_before = printed_before;
                      printed_after = printed_after;
                      box = true }

            | Box_empty ->
                let bar = Text(Text.repeat (columns - 2) "─") in
                let printed_after =
                  after @
                    [Text "\n";
                     Text "┌"; bar; Text "┐\n";
                     Text "│"; Text(Text.repeat (columns - 2) " "); Text "│\n";
                     Text "└"; bar; Text "┘"]
                in
                _draw columns render_state
                  { render_state with
                      printed_before = printed_before;
                      printed_after = printed_after;
                      box = true }

            | Box_words(words, position) ->
                let words = TextSet.elements words and count = TextSet.cardinal words in

                (* Sets the index of the first displayed words such
                   that the cursor is displayed: *)
                let display_start =

                  (* Given a list of words and an offset, it returns
                     the index of the last word that can be
                     dusplayed *)
                  let rec compute_end offset index words =
                    match words with
                      | [] ->
                          index - 1
                      | word :: words ->
                          let offset = offset + Text.length word in
                          if offset <= columns - 1 then
                            compute_end (offset + 1) (index + 1) words
                          else
                            index - 1
                  in

                  if position < render_state.display_start then
                    (* The cursor is before the left margin *)
                    let rev_index = count - position - 1 in
                    count - compute_end 1 rev_index (drop rev_index (List.rev words)) - 1
                  else if compute_end 1 render_state.display_start (drop render_state.display_start words) < position then
                    (* The cursor is after the right margin *)
                    position
                  else
                    (* The cursor points to a word which is
                       displayed *)
                    render_state.display_start
                in

                let words = drop display_start words in
                let printed_after =
                  List.flatten
                    [after;
                     [Text "\n";
                      Text "┌"; Text(make_bar "┬" (columns - 2) words); Text "┐\n";
                      Text "│"];
                     make_completion (position - display_start) (columns - 2) words;
                     [Text "│\n";
                      Text "└"; Text(make_bar "┴" (columns - 2) words); Text "┘"]]
                in

                _draw columns render_state
                  { display_start = display_start;
                    box = true;
                    printed_before = printed_before;
                    printed_after = printed_after }

  let last_draw ~columns ?(map_text=fun x -> x) ~render_state ~engine_state ~prompt () =
    let printed = prompt @ [Reset; Text(map_text(all_input engine_state)); Text "\n"] in
    fst (_draw columns render_state { render_state with
                                        printed_before = printed;
                                        printed_after = [] })

  let erase ~columns ~render_state () =
    goto_beginning_of_line (snd(compute_position columns (0, 0) render_state.printed_before)) @ [Text "\027[J"]
end

(* +-----------------------------------------------------------------+
   | Controlling a running instance                                  |
   +-----------------------------------------------------------------+ *)

module Control =
struct
  type 'a t = {
    result : 'a Lwt.t;
    send_command : Command.t -> unit;
    hide : unit -> unit Lwt.t;
    show : unit -> unit Lwt.t;
  }

  type prompt = Engine.state React.signal -> Lwt_term.styled_text React.signal

  let fake w = { result = w;
                 send_command = ignore;
                 hide = return;
                 show = return }

  let result ctrl = ctrl.result
  let send_command ctrl command = ctrl.send_command command
  let accept ctrl = ctrl.send_command Command.Accept_line
  let interrupt ctrl = ctrl.send_command Command.Break
  let hide ctrl = ctrl.hide ()
  let show ctrl = ctrl.show ()

  (* +---------------------------------------------------------------+
     | Instance parameters                                           |
     +---------------------------------------------------------------+ *)

  open Command

  let set_nth set n =
    let module M = struct exception Return of string end in
    try
      let _ = TextSet.fold (fun x n -> if n = 0 then raise (M.Return x) else n - 1) set n in
      invalid_arg "Lwt_read_line.set_nth"
    with M.Return x ->
      x

  let read_command () = read_key () >|= Command.of_key

  (* State of a read-line instance *)
  type state = {
    render : Terminal.state;
    engine : Engine.state;
    box : Terminal.box;
    prompt : Lwt_term.styled_text;
    visible : bool;
    old_states : edition_state list;
  }

  type event =
    | Ev_command of Command.t
    | Ev_prompt of Lwt_term.styled_text
    | Ev_box of Terminal.box
    | Ev_completion of completion_result
    | Ev_screen_size_changed
    | Ev_hide of unit Lwt.u
    | Ev_show of unit Lwt.u

  let engine_state state = state.engine
  let render_state state = state.render

  (* +---------------------------------------------------------------+
     | Read-line generator                                           |
     +---------------------------------------------------------------+ *)

  let default_prompt _ = React.S.const [Text "# "]

  let rec truncate_list n l = match n, l with
    | 0, l ->
        l
    | _, [] ->
        []
    | n, x :: l ->
        if n > 0 then
          x :: truncate_list (n - 1) l
        else
          []

  let make ?(history=[]) ?(complete=no_completion) ?(clipboard=clipboard) ?(mode=`real_time)
      ?(map_text=fun x -> x) ?(filter=fun s c -> return c) ~map_result ?(prompt=default_prompt) () =
    (* Signal holding the last engine state before waiting for a new
       command: *)
    let engine_state, set_engine_state = React.S.create (Engine.init history) in

    let prompt = prompt engine_state in

    (* The thread of the last launched completion *)
    let completion_thread = ref (return ()) in

    (*** Events ***)

    (* Thread of the last [read_command]. It is cancelled when
       read-line terminates. *)
    let last_read_command_thread = ref (fail Exit) in

    (* Events typed by the user: *)
    let user_events = Lwt_stream.from (fun () ->
                                         let t = read_command () in
                                         last_read_command_thread := t;
                                         lwt command = t in
                                         return (Some(Ev_command command))) in

    (* Events sent by the program: *)
    let program_events, push_event = Lwt_stream.create () in
    let push_event event = push_event (Some event) in

    (* Screan resizing *)
    let size_events = Lwt_event.to_stream (React.E.stamp (React.S.changes Lwt_term.size) Ev_screen_size_changed) in

    (* Prompt events *)
    let prompt_events = Lwt_event.to_stream (React.E.map (fun prompt -> Ev_prompt prompt) (React.S.changes prompt)) in

    (* All events  *)
    let events = Lwt_stream.choose [user_events; program_events; size_events; prompt_events] in

    (*** Box for `real_time mode ***)

    (* Contains the last suggested completion: *)
    let last_completion = ref None in

    (* If [true], [update_box] will generate an [Ev_completion] when
       completion is done, instead of an [Ev_box]. *)
    let want_completion = ref false in

    let update_box =
      match mode with
        | `real_time ->
            React.S.map
              (function
                 | { Engine.mode = Engine.Selection _ } ->
                     push_event (Ev_box(Terminal.Box_message "<selection>"))
                 | { Engine.mode = Engine.Search _ } ->
                     push_event (Ev_box Terminal.Box_none)
                 | { Engine.mode = Engine.Edition edition_state } ->
                     last_completion := None;
                     want_completion := false;
                     completion_thread := begin
                       let thread = complete edition_state >|= fun x -> `Result x in
                       let start_date = Unix.time () in
                       (* Animation to make the user happy: *)
                       let rec loop anim =
                         pick [thread; Lwt_unix.sleep 0.1 >> return `Timeout] >>= function
                           | `Result comp ->
                               last_completion := Some comp.comp_state;
                               if !want_completion then
                                 push_event (Ev_completion comp)
                               else
                                 push_event (Ev_box(Terminal.Box_words(comp.comp_words, 0)));
                               return ()
                           | `Timeout ->
                               let delta = truncate (Unix.time () -. start_date) in
                               let seconds = delta mod 60
                               and minutes = (delta / 60) mod 60
                               and hours = (delta / (60 * 60)) mod 24
                               and days = (delta / (60 * 60 * 24)) in
                               let message =
                                 if days = 0 then
                                   Printf.sprintf "working %s %02d:%02d:%02d" (List.hd anim) hours minutes seconds
                                 else if days = 1 then
                                   Printf.sprintf "working %s 1 day %02d:%02d:%02d" (List.hd anim) hours minutes seconds
                                 else
                                   Printf.sprintf "working %s %d days %02d:%02d:%02d" (List.hd anim) days hours minutes seconds
                               in
                               push_event (Ev_box(Terminal.Box_message message));
                               loop (List.tl anim)
                       in
                       let rec anim = "─" :: "\\" :: "│" :: "/" :: anim in
                       let thread = loop anim in
                       Lwt.on_cancel thread (fun () -> push_event (Ev_box Terminal.Box_empty));
                       thread
                     end)
              engine_state
        | `classic | `none ->
            React.S.const ()
    in

    (*** Main loop ***)

    (* Draw the state on the terminal and update the rendering
       state: *)
    let draw state =
      let text, render_state =
        Terminal.draw
          ~columns:(React.S.value columns)
          ~box:state.box
          ~render_state:state.render
          ~engine_state:state.engine
          ~prompt:state.prompt
          ~map_text
          ()
      in
      lwt () = printc text in
      return { state with render = render_state }
    in

    (* - [prev] is the last displayed state
       - [state] is the current state *)
    let rec loop prev state =
      let thread = Lwt_stream.next events in
      match Lwt.state thread with
        | Sleep ->
            (* This may update the prompt and dynamic completion: *)
            set_engine_state state.engine;
            (* Check a second time since the last command may have
               created new messages: *)
            begin match Lwt.state thread with
              | Sleep ->
                  (* Redraw screen if the event queue is empty *)
                  lwt state = (if state.visible && prev <> state then draw else return) state in
                  lwt event = thread in
                  process_event state event (loop state)

              | Return event ->
                  process_event state event (loop prev)

              | Fail exn ->
                  fail exn
            end

        | Return event ->
            process_event state event (loop prev)

        | Fail exn ->
            fail exn

    (* loop_refresh redraw the current state, even if it haa not
       changed: *)
    and loop_refresh state =
      let thread = Lwt_stream.next events in
      match Lwt.state thread with
        | Sleep ->
            set_engine_state state.engine;
            begin match Lwt.state thread with
              | Sleep ->
                  lwt state = (if state.visible then draw else return) state in
                  lwt event = thread in
                  process_event state event (loop state)

              | Return event ->
                  process_event state event loop_refresh

              | Fail exn ->
                  fail exn
            end

        | Return event ->
            process_event state event loop_refresh

        | Fail exn ->
            fail exn

    and process_event state event loop = match event with
      | Ev_prompt prompt ->
          loop { state with prompt = prompt }

      | Ev_screen_size_changed ->
          lwt () = printc (Terminal.erase ~columns:(React.S.value columns) ~render_state:state.render ()) in
          loop_refresh { state with render = Terminal.init }

      | Ev_hide wakener ->
          if state.visible then begin
            lwt () = printc (Terminal.erase ~columns:(React.S.value columns) ~render_state:state.render ()) in
            wakeup wakener ();
            loop { state with render = Terminal.init; visible = false }
          end else
            loop state

      | Ev_show wakener ->
          if not state.visible then begin
            lwt state = draw state in
            wakeup wakener ();
            loop { state with visible = true }
          end else
            loop state

      | Ev_box box ->
          loop { state with box = box }

      | Ev_completion comp ->
          let state = { state with engine = { state.engine with Engine.mode = Engine.Edition comp.comp_state } } in
          if mode = `classic && TextSet.cardinal comp.comp_words > 1 then
            lwt () =
              printc (Terminal.last_draw
                        ~columns:(React.S.value columns)
                        ~render_state:state.render
                        ~engine_state:state.engine
                        ~prompt:state.prompt
                        ~map_text
                        ())
            in
            lwt () = print_words stdout (React.S.value Lwt_term.columns) (TextSet.elements comp.comp_words) in
            loop_refresh { state with render = Terminal.init }
          else
            loop state

      | Ev_command command ->
          if not (command = Complete && mode = `real_time) then
            (* Cancel completion on user input: *)
            Lwt.cancel !completion_thread;

          (* Save the command for possible [Undo] command *)
          let state =
            match command, state.engine with
              | Undo, _ ->
                  state
              | _, { Engine.mode = Engine.Edition es } -> begin
                  let old_states =
                    match state.old_states with
                      | es' :: _ when es = es' ->
                          state.old_states
                      | old_states ->
                          es :: old_states
                  in
                  { state with old_states = truncate_list 1000 old_states }
                end
              | _ ->
                  state
          in

          filter state command >>= function
            | Nop ->
                loop state

            | Undo -> begin
                match state.old_states with
                  | [] ->
                      loop state
                  | s :: l ->
                      loop { state with
                               engine = { state.engine with Engine.mode = Engine.Edition s };
                               old_states = l }
              end

            | Complete_right ->
                begin match state.box with
                  | Terminal.Box_words(words, index) when index < TextSet.cardinal words - 1 ->
                      loop { state with box = Terminal.Box_words(words, index + 1) }
                  | _ ->
                      loop state
                end

            | Complete_left ->
                begin match state.box with
                  | Terminal.Box_words(words, index) when index > 0 ->
                      loop { state with box = Terminal.Box_words(words, index - 1) }
                  | _ ->
                      loop state
                end

            | Complete_first ->
                begin match state.box with
                  | Terminal.Box_words(words, index) ->
                      loop { state with box = Terminal.Box_words(words, 0) }
                  | _ ->
                      loop state
                end

            | Complete_last ->
                begin match state.box with
                  | Terminal.Box_words(words, index) when not (TextSet.is_empty words)->
                      loop { state with box = Terminal.Box_words(words, TextSet.cardinal words - 1) }
                  | _ ->
                      loop state
                end

            | Complete ->
                begin match mode with
                  | `none ->
                      loop state
                  | `classic ->
                      let state = { state with engine = Engine.reset state.engine } in
                      completion_thread := begin
                        lwt comp = complete (Engine.edition_state state.engine) in
                        push_event (Ev_completion comp);
                        return ()
                      end;
                      loop state
                  | `real_time ->
                      match !last_completion with
                        | Some comp_state ->
                            loop { state with engine = { state.engine with Engine.mode = Engine.Edition comp_state } }
                        | None ->
                            want_completion := true;
                            loop state
                end

            | Meta_complete ->
                if mode = `real_time then begin
                  let state = { state with engine = Engine.reset state.engine } in
                  match state.box with
                    | Terminal.Box_words(words, index) when not (TextSet.is_empty words) ->
                        let before, after = Engine.edition_state state.engine in
                        let word = set_nth words index in
                        let word_len = Text.length word and before_len = Text.length before in
                        (* [search] searches the longest suffix of
                           [before] which is a prefix of [word]: *)
                        let rec search ptr idx =
                          if Text.equal_at ptr (Text.sub word 0 idx) then
                            loop { state with engine = { state.engine with Engine.mode = Engine.Edition(before ^ Text.sub word idx (word_len - idx), after) } }
                          else
                            match Text.next ptr with
                              | None -> fail (Failure "invalid completion")
                              | Some(ch, ptr) -> search ptr (idx - 1)
                        in
                        if word_len > before_len then
                          search (Text.pointer_l before) before_len
                        else
                          search (Text.pointer_at before (-word_len)) word_len
                    | _ ->
                        loop state
                end else
                  loop state

            | Clear_screen ->
                lwt () = clear_screen () in
                loop_refresh state

            | Refresh ->
                loop_refresh state

            | Accept_line ->
                return (state, `Accept)

            | Break ->
                return (state,`Interrupt)

            | command ->
                loop { state with engine = Engine.update ~engine_state:state.engine ~clipboard ~command () }

    in

    let result = with_raw_mode begin fun () ->

      (* Wait for edition to terminate *)
      lwt state, result = loop_refresh {
        render = Terminal.init;
        engine = React.S.value engine_state;
        box = Terminal.Box_none;
        prompt = React.S.value prompt;
        visible = true;
        old_states = [];
      } in

      (* Cleanup *)
      React.S.stop update_box;
      Lwt.cancel !last_read_command_thread;

      (* Do the last draw *)
      lwt () = printc (Terminal.last_draw
                         ~columns:(React.S.value columns)
                         ~render_state:state.render
                         ~engine_state:state.engine
                         ~prompt:state.prompt
                         ~map_text
                         ())
      in

      match result with
        | `Accept ->
            map_result (Engine.all_input state.engine)
        | `Interrupt ->
            fail Interrupt
    end in
    {
      result = result;
      send_command = (fun command -> push_event (Ev_command command));
      hide = (fun () ->
                let waiter, wakener = Lwt.wait () in
                push_event (Ev_hide wakener);
                waiter);
      show = (fun () ->
                let waiter, wakener = Lwt.wait () in
                push_event (Ev_show wakener);
                waiter);
    }

  (* +---------------------------------------------------------------+
     | Predefined instances                                          |
     +---------------------------------------------------------------+ *)

  let make_prompt prompt = React.S.value (prompt (React.S.const (Engine.init [])))

  let read_line ?history ?complete ?clipboard ?mode ?(prompt=default_prompt) () =
    if Unix.isatty Unix.stdin && Unix.isatty Unix.stdout then
      make ?history ?complete ?clipboard ?mode ~prompt ~map_result:return ()
    else
      fake (lwt () = write stdout (strip_styles (make_prompt prompt)) in
            Lwt_text.read_line stdin)

  let read_password ?clipboard ?(style:password_style=`text "*") ?prompt () =
    if Unix.isatty Unix.stdin && Unix.isatty Unix.stdout then
      let map_text = match style with
        | `text ch -> (fun txt -> Text.map (fun _ -> ch) txt)
        | `clear -> (fun x -> x)
        | `empty -> (fun _ -> "")
      and filter state = function
        | Backward_search ->
            (* Drop search commands *)
            return Nop
        | command ->
            return command
      in
      make ?clipboard ~map_text ~mode:`none ~filter ?prompt ~map_result:return ()
    else
      failwith "Lwt_read_line.read_password: not running in a terminal"

  let read_keyword ?history ?(case_sensitive=false) ?mode ?(prompt=default_prompt) ~values () =
    let compare = if case_sensitive then Text.compare else Text.icompare in
    let rec assoc key = function
      | [] -> None
      | (key', value) :: l ->
          if compare key key' = 0 then
            Some value
          else
            assoc key l
    in
    if Unix.isatty Unix.stdin && Unix.isatty Unix.stdout then
      let words = List.fold_left (fun acc (key, value) -> TextSet.add key acc) TextSet.empty values in
      let filter state = function
        | Accept_line ->
            let text = Engine.all_input state.engine in
            if List.exists (fun (key, value) -> compare key text = 0) values then
              return Accept_line
            else
              return Nop
        | command ->
            return command
      and map_result text = match assoc text values with
        | Some value ->
            return value
        | None ->
            assert false
      and complete (before, after) =
        return (complete "" before after words)
      in
      make ?history ?mode ~prompt ~filter ~map_result ~complete ()
    else
      fake (lwt () = write stdout (strip_styles (make_prompt prompt)) in
            lwt txt = Lwt_text.read_line stdin in
            match assoc txt values with
              | Some value ->
                  return value
              | None ->
                  fail (Failure "Lwt_read_line.read_keyword: invalid input"))

  let read_yes_no ?history ?mode ?prompt () =
    read_keyword ?history ?mode ?prompt ~values:[("yes", true); ("no", false)] ()
end

(* +-----------------------------------------------------------------+
   | Simple calls                                                    |
   +-----------------------------------------------------------------+ *)

let default_prompt = [Text "# "]

let read_line ?history ?complete ?clipboard ?mode ?(prompt=default_prompt) () =
  Control.result
    (Control.read_line ?history ?complete ?clipboard ?mode ~prompt:(fun _ -> React.S.const prompt) ())

let read_password ?clipboard ?style ?(prompt=default_prompt) () =
  Control.result
    (Control.read_password ?clipboard ?style ~prompt:(fun _ -> React.S.const prompt) ())

let read_keyword ?history ?case_sensitive ?mode ?(prompt=default_prompt) ~values () =
  Control.result
    (Control.read_keyword ?history ?case_sensitive ?mode ~prompt:(fun _ -> React.S.const prompt) ~values ())

let read_yes_no ?history ?mode ?(prompt=default_prompt) () =
  Control.result
    (Control.read_yes_no ?history ?mode ~prompt:(fun _ -> React.S.const prompt) ())

(* +-----------------------------------------------------------------+
   | History                                                         |
   +-----------------------------------------------------------------+ *)

let add_entry line history =
  if Text.strip line = "" then
    history
  else
    if (match history with [] -> false | x :: _ -> x = line) then
      history
    else
      line :: history

let escape line =
  Text.map (function
              | "\n" -> "\\n"
              | "\\" -> "\\\\"
              | ch -> ch) line

let unescape line =
  let buf = Buffer.create (String.length line) in
  let rec loop ptr = match Text.next ptr with
    | Some("\\", ptr) ->
        begin match Text.next ptr with
          | Some("\\", ptr) ->
              Buffer.add_string buf "\\";
              loop ptr
          | Some("n", ptr) ->
              Buffer.add_string buf "\n";
              loop ptr
          | Some(ch, ptr) ->
              Buffer.add_string buf "\\";
              Buffer.add_string buf ch;
              loop ptr
          | None ->
              Buffer.add_string buf "\\";
              Buffer.contents buf
        end
    | Some(ch, ptr) ->
        Buffer.add_string buf ch;
        loop ptr
    | None ->
        Buffer.contents buf
  in
  loop (Text.pointer_l line)

let rec load_lines ic acc =
  Lwt_io.read_line_opt ic >>= function
    | Some l ->
        load_lines ic (unescape l :: acc)
    | None ->
        return acc

let load_history name =
  if Sys.file_exists name then
    Lwt_io.with_file ~mode:Lwt_io.input name (fun ic -> load_lines ic [])
  else
    return []

let rec merge h1 h2 = match h1, h2 with
  | l1 :: h1, l2 :: h2 when l1 = l2 ->
      l1 :: merge h1 h2
  | _ ->
      h1 @ h2

let save_history name history =
  lwt on_disk_history = load_history name in
  Lwt_io.lines_to_file name (Lwt_stream.map escape (Lwt_stream.of_list (merge (List.rev on_disk_history) (List.rev history))))
