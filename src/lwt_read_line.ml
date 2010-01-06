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

type completion_mode = [ `classic | `real_time ]

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
  if TextSet.is_empty words then
    { comp_state = (before ^ word, after);
      comp_words = TextSet.empty }
  else
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

  let to_string = function
    | Char ch ->
        Printf.sprintf "Char %S" ch
    | Nop ->
        "nop"
    | Backward_delete_char ->
        "backward-delete-char"
    | Forward_delete_char ->
        "forward-delete-char"
    | Beginning_of_line ->
        "beginning-of-line"
    | End_of_line ->
        "end-of-line"
    | Complete ->
        "complete"
    | Meta_complete ->
        "meta-complete"
    | Kill_line ->
        "kill-line"
    | Accept_line ->
        "accept-line"
    | Backward_delete_word ->
        "backward-delete-word"
    | Forward_delete_word ->
        "forward-delete-word"
    | History_next ->
        "history-next"
    | History_previous ->
        "history-previous"
    | Break ->
        "break"
    | Clear_screen ->
        "clear-screen"
    | Insert ->
        "insert"
    | Refresh ->
        "refresh"
    | Backward_char ->
        "backward-char"
    | Forward_char ->
        "forward-char"
    | Set_mark ->
        "set-mark"
    | Paste ->
        "paste"
    | Copy ->
        "copy"
    | Cut ->
        "cut"
    | Uppercase ->
        "uppercase"
    | Lowercase ->
        "lowercase"
    | Capitalize ->
        "capitalize"
    | Backward_word ->
        "backward-word"
    | Forward_word ->
        "forward-word"
    | Complete_left ->
        "complete-left"
    | Complete_right ->
        "complete-right"
    | Complete_up ->
        "complete-up"
    | Complete_down ->
        "complete-down"
    | Backward_search ->
        "backward-search"
    | Complete_first ->
        "complete-first"
    | Complete_last ->
        "complete-last"

  let of_key = function
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
    | Key_control 'j' -> Accept_line
    | Key_control 'k' -> Kill_line
    | Key_control 'l' -> Clear_screen
    | Key_control 'm' -> Accept_line
    | Key_control 'n' -> Backward_char
    | Key_control 'p' -> Forward_char
    | Key_control 'r' -> Backward_search
    | Key_control 'w' -> Cut
    | Key_control 'y' -> Paste
    | Key_control '?' -> Backward_delete_char
    | Key "\027u" -> Uppercase
    | Key "\027l" -> Lowercase
    | Key "\027c" -> Capitalize
    | Key "\027Oc" | Key "\027[1;5C"-> Forward_word
    | Key "\027Od" | Key "\027[1;5D"-> Backward_word
    | Key ch when Text.length ch = 1 && Text.is_print ch -> Char ch
    | Key "\027\027[A" | Key "\027[1;3A" -> Complete_up
    | Key "\027\027[B" | Key "\027[1;3B" -> Complete_down
    | Key "\027\027[C" | Key "\027[1;3C" -> Complete_right
    | Key "\027\027[D" | Key "\027[1;3D" -> Complete_left
    | Key "\027\027[7~" | Key "\027[1;3H" -> Complete_first
    | Key "\027\027[8~" | Key "\027[1;3F" -> Complete_last
    | Key "\027\n" | Key "\194\141" -> Char "\n"
    | Key "\027\t" | Key "\194\137" -> Meta_complete
    | Key "\027w" | Key "\195\183" -> Copy
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
            let word = Text.lower word in
            let rec aux history = match history with
              | [] ->
                  []
              | phrase :: rest ->
                  if Text.contains (Text.lower phrase) word then
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
    lines : int;
    (* Number of lines printed on the screen *)
    length : int;
    (* Length in characters of the complete printed text: the prompt,
       the input before the cursor and the input after the cursor.*)
    height_before : int;
    (* The height of the complete text printed before the cursor: the
       prompt and the input before the cursor. *)
    box : bool;
    (* Tell whether a box is currently displayed *)
    display_start : int;
    (* For dynamic completion. It is the index of the first displayed word. *)
  }

  let init = { lines = 0; length = 0; height_before = 0; display_start = 0; box = false }

  type box =
    | Box_none
    | Box_empty
    | Box_words of text_set * int
    | Box_message of string

  let expand_returns ~columns ~text =
    (* [len] is the distance to the beginning of the line *)
    let rec loop len = function
      | [] ->
          []
      | Text text :: l ->
          let buf = Buffer.create (String.length text) in
          let len = Text.fold
            (fun ch len ->
               if len = columns then begin
                 (* We insert a newline character at the end of each
                    line. This is because some terminals behave
                    strangly when the window is resized. *)
                 Buffer.add_char buf '\n';
                 if ch <> "\n" then begin
                   Buffer.add_string buf ch;
                   1
                 end else
                   0
               end else
                 match ch with
                   | "\n" ->
                       for i = len to columns - 1 do
                         Buffer.add_char buf ' '
                       done;
                       Buffer.add_char buf '\n';
                       0
                   | ch ->
                       Buffer.add_string buf ch;
                       len + 1) text len in
          Text(Buffer.contents buf) :: loop len l
      | style :: l ->
          style :: loop len l
    in
    loop 0 text

  (* Compute the number of row taken by a text given a number of
     columns: *)
  let compute_height columns len =
    if len = 0 then
      0
    else
      (len - 1) / (columns + 1)

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

  let _draw columns render_state printed_before printed_total =
    let printed_before = expand_returns columns printed_before in
    let printed_total = expand_returns columns printed_total in

    (* The new rendering state: *)
    let new_render_state = {
      render_state with
        lines = compute_height columns (styled_length printed_total);
        height_before = compute_height columns (styled_length printed_before);
        length = styled_length printed_total;
    } in

    (* [true] iff the cursor is at the right margin. In this case we
       must print a '\n' to make it appeers at the beginning on the
       next line. *)
    let terminate_on_right_margin = (styled_length printed_before) mod columns = 0

    and remaining = max 0 (render_state.length - styled_length printed_total) in

    let text = List.flatten [
      (* Go back by the number of rows of the previous text: *)
      goto_beginning_of_line render_state.height_before;

      (* Prints everything: *)
      printed_total;

      (* Erase rests of the previous input (in case the input is
         smaller than the predecessor): *)
      [Text(String.make remaining ' ')];

      (* Go back again to the beginning of printed text: *)
      goto_beginning_of_line (compute_height columns (styled_length printed_total + remaining));

      (* Prints again the text before the cursor, to put the cursor at
         the right place: *)
      printed_before;

      if terminate_on_right_margin then [Text "\n"] else [];
    ] in

    if terminate_on_right_margin then
      (text, { new_render_state with height_before = new_render_state.height_before + 1 })
    else
      (text, new_render_state)

  (* Render the current state on the terminal, and returns the new
     terminal rendering state: *)
  let draw ~columns ?(map_text=fun x -> x) ?(box=Box_none) ~render_state ~engine_state ~prompt () =
    match engine_state.mode with
      | Search st ->
          let printed_before = Reset :: prompt @ [Reset; Text "(reverse-i-search)'"; Text st.search_word] in
          let printed_total = match st.search_history with
            | [] ->
                []
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
                printed_before @ [Text "': ";
                                  Text(Text.chunk (Text.pointer_l phrase) ptr_start);
                                  Underlined;
                                  Text(Text.chunk ptr_start ptr_end);
                                  Reset;
                                  Text(Text.chunk ptr_end (Text.pointer_r phrase))]
          in
          _draw columns render_state printed_before printed_total

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
          let printed_before = Reset :: prompt @ [Reset] @ before in

          match box with
            | Box_none ->
                _draw columns { render_state with box = false } printed_before (printed_before @ after)

            | Box_message message ->
                let bar = Text(Text.repeat (columns - 2) "─") in
                let message_len = Text.length message in
                let message = if message_len + 2 > columns then Text.sub message 0 (columns - 2) else message in
                _draw columns { render_state with box = true } printed_before
                  (printed_before @ after @
                     [Text "\n";
                      Text "┌"; bar; Text "┐";
                      Text "│"; Text message; Text(String.make (columns - 2 - message_len) ' '); Text "│";
                      Text "└"; bar; Text "┘"])

            | Box_empty ->
                let bar = Text(Text.repeat (columns - 2) "─") in
                _draw columns { render_state with box  = true } printed_before
                  (printed_before @ after @
                     [Text "\n";
                      Text "┌"; bar; Text "┐";
                      Text "│"; Text(Text.repeat (columns - 2) " "); Text "│";
                      Text "└"; bar; Text "┘"])

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
                let printed_total =
                  printed_before @ after @
                    [Text "\n";
                     Text "┌"; Text(make_bar "┬" (columns - 2) words); Text "┐";
                     Text "│"]
                  @ make_completion (position - display_start) (columns - 2) words
                  @ [Text "│";
                     Text "└"; Text(make_bar "┴" (columns - 2) words); Text "┘"] in

                _draw columns { render_state with display_start = display_start; box = true } printed_before printed_total

  let last_draw ~columns ?(map_text=fun x -> x) ~render_state ~engine_state ~prompt () =
    List.flatten [
      goto_beginning_of_line render_state.height_before;
      expand_returns
        columns
        (prompt @ [Reset; Text(map_text(all_input engine_state))]
         @ (if render_state.box then [Text "\n\n\n\n"] else []));
      if render_state.box then
        goto_beginning_of_line 3
      else
        [];
    ]

  let erase ~columns ~render_state () =
    List.flatten [
      goto_beginning_of_line render_state.height_before;
      expand_returns columns [Text(String.make render_state.lines '\n')];
      goto_beginning_of_line render_state.lines;
    ]
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
     | Helpers for predefined instances                              |
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

  type state = {
    render : Terminal.state;
    engine : Engine.state;
    box : Terminal.box;
    prompt : Lwt_term.styled_text;
  }

  type event =
    | Ev_command of Command.t
    | Ev_prompt of Lwt_term.styled_text
    | Ev_box of Terminal.box
    | Ev_completion of completion_result
    | Ev_screen_size_changed

  (* +---------------------------------------------------------------+
     | Read-line                                                     |
     +---------------------------------------------------------------+ *)

  let read_line ?(history=[]) ?(complete=no_completion) ?(clipboard=clipboard) ?(mode=`real_time) ~prompt () =
    if Unix.isatty Unix.stdin && Unix.isatty Unix.stdout then begin
      (* Signal holding the last engine state before waiting for a
         new command: *)
      let engine_state, set_engine_state = React.S.create (Engine.init history) in

      let prompt = prompt engine_state in

      let completion_thread = ref (return ()) in

      (*** Events ***)

      (* Events typed by the user: *)
      let user_events = Lwt_stream.from (fun () -> read_command () >|= (fun command -> Some(Ev_command command))) in

      (* Events sent by the program: *)
      let push_event, program_events = Lwt_stream.push_stream () in
      let push_event event = push_event (`Data event) in

      (* Screan resizing *)
      let size_events = Lwt_stream.of_event (React.E.stamp (React.S.changes Lwt_term.size) Ev_screen_size_changed) in

      (* Prompt events *)
      let prompt_events = Lwt_stream.of_event (React.E.map (fun prompt -> Ev_prompt prompt) (React.S.changes prompt)) in

      (* All events  *)
      let events = Lwt_stream.choose [user_events; program_events; size_events; prompt_events] in

      (*** Box for `real_time mode ***)

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
                       completion_thread := begin
                         lwt { comp_words = words } = complete edition_state in
                         push_event (Ev_box(Terminal.Box_words(words, 0)));
                         return ()
                       end)
                engine_state
          | `classic ->
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
            ()
        in
        lwt () = printc text in
        return { state with render = render_state }
      in

      (* - [prev] is the last displayed state
         - [state] is the current state *)
      let rec loop prev state =
        (* This may update the prompt: *)
        set_engine_state state.engine;
        let thread = Lwt_stream.next events in
        match Lwt.state thread with
          | Sleep ->
              (* Redraw screen if the event queue is empty *)
              lwt state = (if prev <> state then draw else return) state in
              lwt event = thread in
              process_event state event (loop state)

          | Return event ->
              process_event state event (loop prev)

          | Fail exn ->
              fail exn

      and loop_refresh state =
        set_engine_state state.engine;
        let thread = Lwt_stream.next events in
        match Lwt.state thread with
          | Sleep ->
              (* Redraw screen if the event queue is empty *)
              lwt state = draw state in
              lwt event = thread in
              process_event state event (loop state)

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

        | Ev_box box ->
            loop { state with box = box }

        | Ev_completion comp ->
            lwt () =
              if TextSet.cardinal comp.comp_words > 1 && mode = `classic then
                lwt () = write_char stdout "\n" in
                lwt () = print_words stdout (React.S.value Lwt_term.columns) (TextSet.elements comp.comp_words) in
                write_char stdout "\n";
              else
                return ()
            in
            loop { state with engine = { state.engine with Engine.mode = Engine.Edition comp.comp_state } }

        | Ev_command command ->
            (* Cancel completion on user input: *)
            Lwt.cancel !completion_thread;

            match command with
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
                  let state = { state with engine = Engine.reset state.engine } in
                  completion_thread := begin
                    lwt comp = complete (Engine.edition_state state.engine) in
                    push_event (Ev_completion comp);
                    return ()
                  end;
                  loop state

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
        } in

        (* Cleanup *)
        React.S.stop update_box;

        (* Do the last draw *)
        lwt () = printc (Terminal.last_draw
                           ~columns:(React.S.value columns)
                           ~render_state:state.render
                           ~engine_state:state.engine
                           ~prompt:state.prompt
                           ())
        in

        match result with
          | `Accept ->
              return (Engine.all_input state.engine)
          | `Interrupt ->
              fail Interrupt
      end in
      {
        result = result;
        send_command = (fun command -> push_event (Ev_command command));
        hide = return;
        show = return;
      };

    end else
      let prompt = React.S.value (prompt (React.S.const (Engine.init history))) in
      fake (lwt () = write stdout (strip_styles prompt) in
            Lwt_text.read_line stdin)
end

(* +-----------------------------------------------------------------+
   | Simple calls                                                    |
   +-----------------------------------------------------------------+ *)

let read_line ?history ?complete ?clipboard ?mode ~prompt () =
  Control.result
    (Control.read_line ?history ?complete ?clipboard ?mode ~prompt:(fun _ -> React.S.const prompt) ())

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

let save_history name history =
  with_file ~mode:Lwt_io.output name
    (fun oc ->
       Lwt_util.iter_serial
         (fun line -> write oc line >> write_char oc "\000")
         history)

let load_line ic =
  let buf = Buffer.create 42 in
  let rec loop () =
    read_char_opt ic >>= function
      | None | Some "\000" ->
          return (`Line(Buffer.contents buf))
      | Some ch ->
          Buffer.add_string buf ch;
          loop ()
  in
  read_char_opt ic >>= function
    | None -> return `End_of_file
    | Some "\000" -> return `Empty
    | Some ch -> Buffer.add_string buf ch; loop ()

let rec load_lines ic =
  load_line ic >>= function
    | `Line line ->
        lwt lines = load_lines ic in
        return (line :: lines)
    | `Empty ->
        load_lines ic
    | `End_of_file ->
        return []

let load_history name =
  match try Some(open_file ~mode:Lwt_io.input name) with _ -> None with
    | Some ic ->
        try_lwt
          load_lines ic
        finally
          close ic
    | None ->
        return []
