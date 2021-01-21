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

type +'a node = Nil | Cons of 'a * 'a t

and 'a t = unit -> 'a node Lwt.t

let empty : 'a t = fun () -> Lwt.return Nil

let return x : 'a t = fun () -> Lwt.return (Cons (x, empty))

let cons x t () = Lwt.return (Cons (x, t))

let rec append seq1 seq2 () =
  let* seq1 = seq1 () in
  match seq1 with
  | Nil -> seq2 ()
  | Cons (x, next) -> Lwt.return (Cons (x, append next seq2))

let rec map f seq () =
  let* seq = seq () in
  match seq with
  | Nil -> Lwt.return Nil
  | Cons (x, next) ->
      let+ x = f x in
      Cons (x, map f next)

let rec filter_map f seq () =
  let* seq = seq () in
  match seq with
  | Nil -> Lwt.return Nil
  | Cons (x, next) -> (
      let* x = f x in
      match x with
      | None -> filter_map f next ()
      | Some y -> Lwt.return (Cons (y, filter_map f next) ))

let rec filter f seq () =
  let* seq = seq () in
  match seq with
  | Nil -> Lwt.return Nil
  | Cons (x, next) ->
      let* ok = f x in
      if ok then Lwt.return (Cons (x, filter f next)) else filter f next ()

let rec flat_map f seq () =
  let* seq = seq () in
  match seq with
  | Nil -> Lwt.return Nil
  | Cons (x, next) ->
      let* x = f x in
      flat_map_app f x next ()

(* this is [append seq (flat_map f tail)] *)
and flat_map_app f seq tail () =
  let* seq = seq () in
  match seq with
  | Nil -> flat_map f tail ()
  | Cons (x, next) -> Lwt.return (Cons (x, flat_map_app f next tail))

let fold_left f acc seq =
  let rec aux f acc seq =
    let* seq = seq () in
    match seq with
    | Nil -> Lwt.return acc
    | Cons (x, next) ->
        let* acc = f acc x in
        aux f acc next
  in
  aux f acc seq

let iter f seq =
  let rec aux seq =
    let* seq = seq () in
    match seq with
    | Nil -> Lwt.return ()
    | Cons (x, next) ->
        let* () = f x in
        aux next
  in
  aux seq

let rec unfold f u () =
  let* x = f u in
  match x with
  | None -> Lwt.return Nil
  | Some (x, u') -> Lwt.return (Cons (x, unfold f u'))

let rec of_list = function
  | [] -> empty
  | h :: t -> cons h (of_list t)

let rec to_list seq =
  let* seq = seq () in
  match seq with
  | Nil -> Lwt.return []
  | Cons (x, next) ->
    let+ l = to_list next in
    x :: l
