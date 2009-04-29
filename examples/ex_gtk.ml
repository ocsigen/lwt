(*
 * ex_gtk.ml
 * ---------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

let num = ref 0

let _ =
  Lwt_glib.init ();
  let window = GWindow.window ~border_width:10 () in
  let _ = window#event#connect#delete ~callback:(fun _ -> true)
  and _ = window#connect#destroy ~callback:Lwt_glib.quit in

  let box = GPack.vbox ~packing:window#add () in
  let button = GButton.button ~label:"decrement" ~packing:box#add () in

  let label = GMisc.label ~packing:box#add () in

  let rec loop _ =
    incr num;
    label#set_label (string_of_int !num);
    Lwt.bind (Lwt_unix.sleep 1.0) loop
  in

  (* Start a thread which update the label every seconds *)
  Lwt.ignore_result (loop ());

  let _ = button#connect#clicked ~callback:(fun _ ->
                                              decr num;
                                              label#set_label (string_of_int !num)) in

  window#show ();
  GMain.Main.main ()
