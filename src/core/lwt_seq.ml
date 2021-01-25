(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Simon Cruanes                                          *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)



open Lwt.Syntax
open Lwt.Infix

type +'a node = Nil | Cons of 'a * 'a t

and 'a t = unit -> 'a node Lwt.t

let return_nil = Lwt.return Nil

let empty : 'a t = fun () -> return_nil

let return x : 'a t = fun () -> Lwt.return (Cons (x, empty))

let cons x t () = Lwt.return (Cons (x, t))

let rec append seq1 seq2 () =
  seq1 () >>= function
  | Nil -> seq2 ()
  | Cons (x, next) -> Lwt.return (Cons (x, append next seq2))

let rec map f seq () =
  seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      let+ x = f x in
      Cons (x, map f next)

let rec filter_map f seq () =
  seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) -> (
      let* x = f x in
      match x with
      | None -> filter_map f next ()
      | Some y -> Lwt.return (Cons (y, filter_map f next) ))

let rec filter f seq () =
  seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      let* ok = f x in
      if ok then Lwt.return (Cons (x, filter f next)) else filter f next ()

let rec flat_map f seq () =
  seq () >>= function
  | Nil -> return_nil
  | Cons (x, next) ->
      let* x = f x in
      flat_map_app f x next ()

(* this is [append seq (flat_map f tail)] *)
and flat_map_app f seq tail () =
  seq () >>= function
  | Nil -> flat_map f tail ()
  | Cons (x, next) -> Lwt.return (Cons (x, flat_map_app f next tail))

let fold_left f acc seq =
  let rec aux f acc seq =
    seq () >>= function
    | Nil -> Lwt.return acc
    | Cons (x, next) ->
        let* acc = f acc x in
        aux f acc next
  in
  aux f acc seq

let iter f seq =
  let rec aux seq =
    seq () >>= function
    | Nil -> Lwt.return_unit
    | Cons (x, next) ->
        let* () = f x in
        aux next
  in
  aux seq

let rec unfold f u () =
  let* x = f u in
  match x with
  | None -> return_nil
  | Some (x, u') -> Lwt.return (Cons (x, unfold f u'))

let rec of_list = function
  | [] -> empty
  | h :: t -> cons h (of_list t)

let rec to_list seq =
  seq () >>= function
  | Nil -> Lwt.return_nil
  | Cons (x, next) ->
    let+ l = to_list next in
    x :: l

let rec of_seq seq =
  try
    match seq () with
    | Seq.Nil -> empty
    | Seq.Cons (x, next) ->
      cons x (of_seq next)
  with exn ->
    fun () -> raise exn

let rec to_seq seq =
  Lwt.catch
    (fun () ->
      seq () >|= fun x ->
      `Result x )
    (fun exn ->
      Lwt.return (`Exn exn)
  ) >>= function
  | `Result Nil -> Lwt.return Seq.empty
  | `Result (Cons (x, next)) ->
      let+ next = to_seq next in
      fun () -> Seq.Cons (x, next)
  | `Exn exn ->
      Lwt.return (fun () -> raise exn)
