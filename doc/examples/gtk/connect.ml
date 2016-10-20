(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Program Connect
 * Copyright (C) 2011 Jérémie Dimino
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

(* A simple graphical telnet. *)

open Lwt.Infix

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

let show_error fmt =
  Printf.ksprintf
    (fun message ->
       let dialog = GWindow.message_dialog ~message_type:`ERROR ~buttons:GWindow.Buttons.ok ~message () in
       ignore (dialog#connect#response (function
                                          | `DELETE_EVENT -> ()
                                          | `OK -> dialog#destroy ()));
       dialog#show ())
    fmt

(* +-----------------------------------------------------------------+
   | Connection                                                      |
   +-----------------------------------------------------------------+ *)

(* Either [None] if we are not connected, either [Some (ic, oc,
   thread)] if we are connected. In this last case [thread] is the
   thread reading data from the connection. *)
let connection = ref None

(* Read continously data from [ic] and write them to [view]. *)
let read ic (view : GText.view) =
  let rec loop () =
    match%lwt Lwt_io.read_line_opt ic with
      | Some line ->
          view#buffer#insert ~iter:view#buffer#end_iter ~tag_names:["recv"] (line ^ "\n");
          loop ()
      | None ->
          view#buffer#insert ~iter:view#buffer#end_iter "end of connection\n";
          Lwt_io.close ic
  in
  try%lwt
    loop ()
  with Unix.Unix_error (error, _, _) ->
    show_error "reading error: %s" (Unix.error_message error);
    Lwt.return_unit

(* Function called when the user active the [connect] menu
   item. [view] is the text view used to display data received from
   the connection. *)
let connect (view : GText.view) =
  (* Create a popup for asking the address and port to connect to. *)
  let dialog = GWindow.dialog ~title:"connection" () in
  dialog#add_button_stock `OK `OK;
  dialog#add_button_stock `CANCEL `CANCEL;
  let hbox = GPack.hbox ~packing:dialog#vbox#add () in
  ignore (GMisc.label ~packing:hbox#add ~text:"host: " ());
  let host = GEdit.entry ~packing:hbox#add ~text:"127.0.0.1" () in
  ignore (GMisc.label ~packing:hbox#add ~text:" port: " ());
  let port = GEdit.spin_button ~digits:0 ~numeric:true ~packing:hbox#add () in
  port#adjustment#set_bounds ~lower:0. ~upper:(float max_int) ~step_incr:1. ();

  (* Thread waiting for the popup to be closed. *)
  let waiter, wakener = Lwt.wait () in

  (* Wakeup the thread when the popup is closed. *)
  ignore (dialog#connect#response (Lwt.wakeup wakener));

  dialog#show ();

  ignore (
    match%lwt waiter with
      | `DELETE_EVENT ->
          Lwt.return_unit
      | `CANCEL ->
          dialog#destroy ();
          Lwt.return_unit
      | `OK ->
          let host = host#text and port = int_of_float port#value in
          dialog#destroy ();
          try%lwt
            (* Resolve the address. *)
            let%lwt entry = Lwt_unix.gethostbyname host in
            if Array.length entry.Unix.h_addr_list = 0 then begin
              show_error "no address found for host %S" host;
              Lwt.return_unit
            end else begin
              let%lwt ic, oc = Lwt_io.open_connection (Unix.ADDR_INET (entry.Unix.h_addr_list.(0), port)) in
              (* Close the previous connection. *)
              let%lwt () =
                match !connection with
                  | None ->
                      Lwt.return_unit
                  | Some (ic, oc, thread) ->
                      Lwt.cancel thread;
                      try%lwt
                        Lwt_io.close ic <&> Lwt_io.close oc
                      with Unix.Unix_error (error, _, _) ->
                        show_error "cannot close the connection: %s" (Unix.error_message error);
                        Lwt.return_unit
              in
              (* Clear the buffer. *)
              view#buffer#delete view#buffer#start_iter view#buffer#end_iter;
              connection := Some (ic, oc, read ic view);
              Lwt.return_unit
            end
          with
            | Unix.Unix_error (error, _, _) ->
                show_error "cannot establish the connection: %s" (Unix.error_message error);
                Lwt.return_unit
            | Not_found ->
                show_error "host %S not found" host;
                Lwt.return_unit
  )

(* Send some data. *)
let write (view : GText.view) (entry : GEdit.entry) =
  let text = entry#text in
  entry#set_text "";
  match !connection with
    | Some (ic, oc, thread) ->
        view#buffer#insert ~iter:view#buffer#end_iter ~tag_names:["send"] (text ^ "\n");
        ignore (
          try%lwt
            Lwt_io.write_line oc text
          with Unix.Unix_error (error, _, _) ->
            show_error "cannot send line: %s" (Unix.error_message error);
            Lwt.return_unit
        )
    | None ->
        show_error "not connected"

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let%lwt () =
  (* Initializes GTK. *)
  ignore (GMain.init ~setlocale:false ());

  (* Integrate Lwt with Glib. *)
  Lwt_glib.install ();

  (* Create the UI. *)
  let window = GWindow.window ~title:"simple graphical telnet in OCaml with Lwt" ~allow_shrink:true ~width:640 ~height:480 () in
  let vbox = GPack.vbox ~packing:window#add () in

  (* Create the menu. *)
  let menu = GMenu.menu_bar ~packing:(vbox#pack ~expand:false) () in
  let menu_file = GMenu.menu ~packing:(GMenu.menu_item ~label:"File" ~packing:menu#add ())#set_submenu () in
  let menu_connect = GMenu.image_menu_item ~label:"Connect" ~packing:menu_file#add ~stock:`CONNECT () in
  ignore (GMenu.separator_item ~packing:menu_file#add ());
  let menu_quit = GMenu.image_menu_item ~label:"Quit" ~packing:menu_file#add ~stock:`QUIT () in

  (* The text view displaying inputs and outputs. *)
  let view =
    GText.view
      ~editable:false
      ~packing:(GBin.scrolled_window
                  ~hpolicy:`AUTOMATIC
                  ~vpolicy:`AUTOMATIC
                  ~packing:(GBin.frame
                              ~label:"log"
                              ~packing:vbox#add
                              ())#add
                  ())#add
      ()
  in

  ignore (view#buffer#create_tag ~name:"send" [`FOREGROUND "blue"]);
  ignore (view#buffer#create_tag ~name:"recv" [`FOREGROUND "#007f00"]);

  let hbox = GPack.hbox ~packing:(GBin.frame ~label:"input" ~packing:(vbox#pack ~expand:false) ())#add () in

  (* The entry for user input. *)
  let entry = GEdit.entry ~packing:hbox#add () in
  let send = GButton.button ~label:"send" ~packing:(hbox#pack ~expand:false) () in

  (* Try to use a monospace font. *)
  (try
     view#misc#modify_font_by_name "Monospace";
     entry#misc#modify_font_by_name "Monospace"
   with _ ->
     ());

  (* Thread waiting for the main window to be closed. *)
  let waiter, wakener = Lwt.wait () in

  (* Setup callbacks. *)
  ignore (window#connect#destroy (Lwt.wakeup wakener));
  ignore (menu_quit#connect#activate (Lwt.wakeup wakener));
  ignore (menu_connect#connect#activate (fun () -> connect view));
  ignore (entry#connect#activate (fun () -> write view entry));
  ignore (send#connect#clicked (fun () -> write view entry));

  window#show ();

  (* Wait for the main window to be closed. *)
  waiter
