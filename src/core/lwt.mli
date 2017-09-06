(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2005-2008 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *               2009-2012 Jérémie Dimino
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



type +'a t

type 'a u

val task : unit -> 'a t * 'a u

val wakeup_later : 'a u -> 'a -> unit

val wakeup_later_exn : 'a u -> exn -> unit

val return : 'a -> 'a t

val fail : exn -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

val finalize : (unit -> 'a t) -> (unit -> unit t) -> 'a t

val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t

val async : (unit -> 'a t) -> unit

val async_exception_hook : (exn -> unit) ref

val join : unit t list -> unit t

val pick : 'a t list -> 'a t

val choose : 'a t list -> 'a t

val npick : 'a t list -> 'a list t

val nchoose : 'a t list -> 'a list t

val nchoose_split : 'a t list -> ('a list * 'a t list) t

exception Canceled

val cancel : 'a t -> unit

val on_cancel : 'a t -> (unit -> unit) -> unit

val protected : 'a t -> 'a t

val no_cancel : 'a t -> 'a t

val wait : unit -> 'a t * 'a u

val map : ('a -> 'b) -> 'a t -> 'b t

val on_success : 'a t -> ('a -> unit) -> unit

val on_failure : 'a t -> (exn -> unit) -> unit

val on_termination : 'a t -> (unit -> unit) -> unit

val on_any : 'a t -> ('a -> unit) -> (exn -> unit) -> unit

module Infix : sig

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t

  val ( <&> ) : unit t -> unit t -> unit t

  val ( <?> ) : 'a t -> 'a t -> 'a t

  val (=<<) : ('a -> 'b t) -> 'a t -> 'b t

  val (=|<) : ('a -> 'b) -> 'a t -> 'b t
end

val return_unit : unit t

val return_none : 'a option t

val return_nil : 'a list t

val return_true : bool t

val return_false : bool t

type +'a result = ('a, exn) Result.result

val of_result : 'a result -> 'a t

val wakeup_later_result : 'a u -> 'a result -> unit

type 'a state =
  | Return of 'a
  | Fail of exn
  | Sleep

val state : 'a t -> 'a state

type 'a key

val new_key : unit -> 'a key

val get : 'a key -> 'a option

val with_value : 'a key -> 'a option -> (unit -> 'b) -> 'b

val wakeup : 'a u -> 'a -> unit

val wakeup_exn : 'a u -> exn -> unit

val wakeup_result : 'a u -> 'a result -> unit

val make_value : 'a -> 'a result
  [@@ocaml.deprecated
    " Use Result.Ok, which is the same as Ok since OCaml 4.03."]

val make_error : exn -> 'a result
  [@@ocaml.deprecated
    " Use Result.Error, which is the same as Error since OCaml 4.03."]

val waiter_of_wakener : 'a u -> 'a t

val add_task_r : 'a u Lwt_sequence.t -> 'a t

val add_task_l : 'a u Lwt_sequence.t -> 'a t

val pause : unit -> unit t

val wakeup_paused : unit -> unit

val paused_count : unit -> int

val register_pause_notifier : (int -> unit) -> unit

val wrap : (unit -> 'a) -> 'a t

val wrap1 : ('a -> 'b) -> 'a -> 'b t

val wrap2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c t
val wrap3 : ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd t
val wrap4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e t
val wrap5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f t
val wrap6 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g t
val wrap7 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h t

val return_some : 'a -> 'a option t

val return_ok : 'a -> ('a, _) Result.result t

val return_error : 'e -> (_, 'e) Result.result t

val fail_with : string -> 'a t

val fail_invalid_arg : string -> 'a t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val (>|=) : 'a t -> ('a -> 'b) -> 'b t

val ( <?> ) : 'a t -> 'a t -> 'a t

val ( <&> ) : unit t -> unit t -> unit t

val (=<<) : ('a -> 'b t) -> 'a t -> 'b t

val (=|<) : ('a -> 'b) -> 'a t -> 'b t

val is_sleeping : 'a t -> bool

val ignore_result : 'a t -> unit

(**/**)

val poll : 'a t -> 'a option

val apply : ('a -> 'b t) -> 'a -> 'b t

val backtrace_bind : (exn -> exn) -> 'a t -> ('a -> 'b t) -> 'b t
val backtrace_catch : (exn -> exn) -> (unit -> 'a t) -> (exn -> 'a t) -> 'a t
val backtrace_try_bind : (exn -> exn) -> (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
val backtrace_finalize : (exn -> exn) -> (unit -> 'a t) -> (unit -> unit t) -> 'a t

val abandon_wakeups : unit -> unit
