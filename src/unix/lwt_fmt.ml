(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Lwt.Infix

[@@@alert "-trespassing"]

module Lwt_dls = Lwt_dls

[@@@alert "+trespassing"]

type formatter =
  { commit : unit -> unit Lwt.t
  ; fmt : Format.formatter
  ; per_domain : formatter Lwt_dls.t option
    (* [Some] on [stdout] and [stderr], which stand for one formatter per
       domain, like the channels they print to (see [Lwt_io.stdout]). A
       formatter queues
       what it prints, so it belongs to one domain, as the channel does. *)
  }

let[@inline] resolve ppft =
  match ppft.per_domain with None -> ppft | Some key -> Lwt_dls.get key

let write_pending ppft = (resolve ppft).commit ()

let flush ppft =
  let ppft = resolve ppft in
  Format.pp_print_flush ppft.fmt ();
  ppft.commit ()

let make_formatter ~commit ~fmt () = {commit; fmt; per_domain = None}
let get_formatter x = (resolve x).fmt

(** Stream formatter *)

type order = String of string * int * int | Flush

let make_stream () =
  let stream, push = Lwt_stream.create () in
  let out_string s i j = push @@ Some (String (s, i, j))
  and flush () = push @@ Some Flush in
  let fmt = Format.make_formatter out_string flush in
  (* Through [Lwt_gc], which runs the function on the loop that registered it:
     a plain finalizer may run on another domain once this one has gone, and
     pushing to the stream from there would raise inside the finalizer. *)
  Lwt_gc.finalise (fun _ -> push None; Lwt.return_unit) fmt;
  let commit () = Lwt.return_unit in
  stream, make_formatter ~commit ~fmt ()

(** Channel formatter *)

let write_order oc = function
  | String (s, i, j) -> Lwt_io.write_from_string_exactly oc s i j
  | Flush -> Lwt_io.flush oc

let rec write_orders oc queue =
  if Queue.is_empty queue
  then Lwt.return_unit
  else
    let o = Queue.pop queue in
    write_order oc o >>= fun () -> write_orders oc queue

let of_channel oc =
  let q = Queue.create () in
  let out_string s i j = Queue.push (String (s, i, j)) q
  and flush () = Queue.push Flush q in
  let fmt = Format.make_formatter out_string flush in
  let commit () = write_orders oc q in
  make_formatter ~commit ~fmt ()

(** Printing functions *)

let kfprintf k ppft fmt =
  let ppft = resolve ppft in
  Format.kfprintf (fun _ppf -> k ppft @@ ppft.commit ()) ppft.fmt fmt

let ikfprintf k ppft fmt =
  let ppft = resolve ppft in
  Format.ikfprintf (fun _ppf -> k ppft @@ Lwt.return_unit) ppft.fmt fmt

let fprintf ppft fmt = kfprintf (fun _ t -> t) ppft fmt
let ifprintf ppft fmt = ikfprintf (fun _ t -> t) ppft fmt

let per_domain_formatter make_one =
  let key = Lwt_dls.new_key make_one in
  let main = Lwt_dls.get key in
  {main with per_domain = Some key}

let stdout = per_domain_formatter (fun () -> of_channel Lwt_io.stdout)
let stderr = per_domain_formatter (fun () -> of_channel Lwt_io.stderr)
let printf fmt = fprintf stdout fmt
let eprintf fmt = fprintf stderr fmt
