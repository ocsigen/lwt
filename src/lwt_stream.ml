(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_stream
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

let lazy_from_fun = Lazy.lazy_from_fun

exception Empty

type 'a node =
  | Cons of 'a * 'a lazy_list
  | Nil

and 'a lazy_list = 'a node Lwt.t Lazy.t

type 'a t = 'a lazy_list ref

let of_lazy_list l = ref l
let to_lazy_list s = !s

let make f = ref(Lazy.lazy_from_fun f)

let from f =
  let rec next _ =
    f () >|= function
      | Some x -> Cons(x, lazy_from_fun next)
      | None -> Nil
  in
  make next

let of_list l =
  let rec get = function
    | [] -> return Nil
    | x :: l -> return (Cons(x, lazy(get l)))
  in
  ref(lazy(get l))

let of_string s =
  let rec get i =
    if i = String.length s then
      return Nil
    else
      return (Cons(s.[i], lazy(get (i + 1))))
  in
  ref(lazy(get 0))

let of_channel ch = from (fun _ -> Lwt_io.peek_char ch)

let clone s = ref !s

let peek s = Lazy.force !s >|= function
  | Cons(x, _) -> Some x
  | Nil -> None

let npeek n s =
  let rec aux n l =
    if n <= 0 then
      return []
    else
      Lazy.force l >>= function
        | Cons(x, l) ->
            lwt l = aux (n - 1) l in
            return (x :: l)
        | Nil ->
            return []
  in
  aux 0 !s

let get s = Lazy.force !s >|= function
  | Cons(x, l) -> s := l; Some x
  | Nil -> None

let rec nget n s =
  if n <= 0 then
    return []
  else
    Lazy.force !s >>= function
      | Cons(x, l) ->
          s := l;
          lwt l = nget (n - 1) s in
          return (x :: l)
      | Nil ->
          return []

let rec get_while f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        if f x then begin
          s := l;
          lwt l = get_while f s in
          return (x :: l)
        end else
          return []
    | Nil ->
        return []

let rec get_while_s f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        f x >>= begin function
          | true ->
              s := l;
              lwt l = get_while_s f s in
              return (x :: l)
          | false ->
              return []
        end
    | Nil ->
        return []

let next s = Lazy.force !s >>= function
  | Cons(x, l) -> s := l; return x
  | Nil -> fail Empty

let junk s = Lazy.force !s >>= function
  | Cons(x, l) -> s := l; return ()
  | Nil -> return ()

let rec njunk n s =
  if n <= 0 then
    return ()
  else
    Lazy.force !s >>= function
      | Cons(x, l) ->
          s := l;
          njunk (n - 1) s
      | Nil ->
          return ()

let rec junk_while f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        if f x then begin
          s := l;
          junk_while f s
        end else
          return ()
    | Nil ->
        return ()

let rec junk_while_s f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        f x >>= begin function
          | true ->
              s := l;
              junk_while_s f s
          | false ->
              return ()
        end
    | Nil ->
        return ()

let is_empty s = Lazy.force !s >|= function
  | Cons _ -> false
  | Nil -> true

let map f s =
  let rec next _ =
    Lazy.force !s >>= function
      | Cons(x, l) ->
          s := l;
          let x = f x in
          return (Cons(x, lazy_from_fun next))
      | Nil ->
          return Nil
  in
  make next

let map_s f s =
  let rec next _ =
    Lazy.force !s >>= function
      | Cons(x, l) ->
          s := l;
          lwt x = f x in
          return (Cons(x, lazy_from_fun next))
      | Nil ->
          return Nil
  in
  make next

let filter f s =
  let rec next _ =
    Lazy.force !s >>= function
      | Cons(x, l) ->
          s := l;
          if f x then
            return (Cons(x, lazy_from_fun next))
          else
            next ()
      | Nil ->
          return Nil
  in
  make next

let filter_s f s =
  let rec next _ =
    Lazy.force !s >>= function
      | Cons(x, l) ->
          s := l;
          f x >>= begin function
            | true ->
                return (Cons(x, lazy_from_fun next))
            | false ->
                next ()
          end
      | Nil ->
          return Nil
  in
  make next

let filter_map f s =
  let rec next _ =
    Lazy.force !s >>= function
      | Cons(x, l) ->
          s := l;
          begin match f x with
            | Some x ->
                return (Cons(x, lazy_from_fun next))
            | None ->
                next ()
          end
      | Nil ->
          return Nil
  in
  make next

let filter_map_s f s =
  let rec next _ =
    Lazy.force !s >>= function
      | Cons(x, l) ->
          s := l;
          f x >>= begin function
            | Some x ->
                return (Cons(x, lazy_from_fun next))
            | None ->
                next ()
          end
      | Nil ->
          return Nil
  in
  make next

let rec fold f s acc =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        s := l;
        fold f s (f x acc)
    | Nil ->
        return acc

let rec fold_s f s acc =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        s := l;
        f x acc >>= fold_s f s
    | Nil ->
        return acc

let rec iter f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        s := l;
        f x;
        iter f s
    | Nil ->
        return ()

let rec iter_s f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        s := l;
        f x >> iter f s
    | Nil ->
        return ()

let rec iter_p f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        s := l;
        f x <&> iter f s
    | Nil ->
        return ()

let rec find f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        s := l;
        if f x then
          return (Some x)
        else
          find f s
    | Nil ->
        return None

let rec find_s f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        s := l;
        f x >>= begin function
          | true ->
              return (Some x)
          | false ->
              find_s f s
        end
    | Nil ->
        return None

let rec find_map f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        s := l;
        begin match f x with
          | Some x as n -> return n
          | None -> find_map f s
        end
    | Nil ->
        return None

let rec find_map_s f s =
  Lazy.force !s >>= function
    | Cons(x, l) ->
        s := l;
        f x >>= begin function
          | Some x as n -> return n
          | None -> find_map_s f s
        end
    | Nil ->
        return None

let rec combine s1 s2 =
  let rec next _ =
    lwt n1 = get s1 and n2 = get s2 in
    match n1, n2 with
      | Some x1, Some x2 ->
          return (Cons((x1, x2), lazy_from_fun next))
      | _ ->
          return Nil
  in
  make next

let split s = (map fst (clone s), map snd (clone s))

let partition f s = (filter f s, filter (fun x -> not (f x)) (clone s))
let partition_s f s = (filter_s f s, filter_s (fun x -> f x >|= not) (clone s))

let choose streams =
  let source s = (s, Lazy.force !s >|= fun n -> (s, n)) in
  let rec next = function
    | [] ->
        return Nil
    | streams ->
        Lwt.choose (List.map snd streams) >>= fun (s, n) ->
          let streams = List.remove_assq s streams in
          match n with
            | Cons(x, l) ->
                s := l;
                return (Cons(x, lazy(next (source s :: streams))))
            | Nil ->
                next streams
  in
  ref(lazy(next (List.rev_map source streams)))

let parse_utf8 s =
  let buf = String.create 4 in
  let rec next _ =
    Lazy.force !s >>= function
      | Cons(c, l) ->
          buf.[0] <- c;
          let n = Char.code c in
          if n land 0x80 = 0 then begin
            s := l;
            return (Cons(String.sub buf 0 1, lazy_from_fun next))
          end else if n land 0xe0 = 0xc0 then
            trail 0x80 (n land 0x1f) 2 1 l
          else if n land 0xf0 = 0xe0 then
            trail 0x800 (n land 0x0f) 3 1 l
          else if n land 0xf8 = 0xf0 then
            trail 0x10000 (n land 0x07) 4 1 l
          else
            fail (Failure "cannot decode utf-8: invalid start of sequence")
      | Nil ->
          return Nil
  and trail minimum acc len i l =
    if i = len then
      if acc < minimum then
        fail (Failure "cannot decode utf-8: overlong sequence")
      else begin
        s := l;
        return (Cons(String.sub buf 0 len, lazy_from_fun next))
      end
    else
      Lazy.force l >>= function
        | Cons(c, l) ->
            buf.[i] <- c;
            let n = Char.code c in
            if n land 0xc0 = 0x80 then
              trail minimum ((acc lsl 6) lor (n land 0x3f)) len (i + 1) l
            else
              fail (Failure "cannot decode utf-8: unterminated sequence")
        | Nil ->
            fail (Failure "cannot decode utf-8: unterminated sequence")
  in
  make next

let standard = of_channel Lwt_io.stdin
let standard_utf8 = parse_utf8 standard
