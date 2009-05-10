(*
 * ex_gtk.ml
 * ---------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

(* Example using GTK.

   It display a counter which is incremented every second by a
   cooperative thread.
*)

(* The counter *)
let num = ref 0

let _ =
  (* Needed for using Lwt+GLib, it replaces GLib initialization: *)
  Lwt_glib.init ();

  (* Creates the windows, and connects standard events: *)
  let window = GWindow.window ~border_width:10 () in
  let _ = window#event#connect#delete ~callback:(fun _ -> false)
  and _ = window#connect#destroy ~callback:Lwt_glib.quit in

  (* Creates a box, the decrement button and the label: *)
  let box = GPack.vbox ~packing:window#add () in
  let button = GButton.button ~label:"decrement" ~packing:box#add () in
  let label = GMisc.label ~packing:box#add () in

  (* The cooperative thread which increment the counter every
     seconds: *)
  let rec loop _ =
    incr num;
    label#set_label (string_of_int !num);
    Lwt.bind (Lwt_unix.sleep 1.0) loop
  in

  (* Starts it, without waiting for the result: *)
  Lwt.ignore_result (loop ());

  (* Connects clicks on the decrement button: *)
  let _ = button#connect#clicked ~callback:(fun _ ->
                                              decr num;
                                              label#set_label (string_of_int !num)) in

  (* Display the main window of the application: *)
  window#show ();

  (* Main loop: *)
  GMain.Main.main ()
