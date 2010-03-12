(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Interface Lwt_text
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
open Lwt_io

(* +-----------------------------------------------------------------+
   | Types and creation                                              |
   +-----------------------------------------------------------------+ *)

type coder =
  | Encoder of Encoding.encoder
  | Decoder of Encoding.decoder

type 'a channel = {
  channel : 'a Lwt_io.channel;
  encoding : Text.t;
  coder : coder;
  strict : bool;
}

type input_channel = Lwt_io.input channel
type output_channel = Lwt_io.output channel

let encoder = function
  | Encoder e -> e
  | Decoder _ -> assert false

let decoder = function
  | Encoder _ -> assert false
  | Decoder d -> d

let make ?(strict=false) ?(encoding=Encoding.system) ch =
  { channel = ch;
    encoding = encoding;
    strict = strict;
    coder = match Lwt_io.mode ch with
      | Input ->
          Decoder(Encoding.decoder encoding)
      | Output ->
          Encoder(Encoding.encoder(if strict then encoding else encoding ^ "//TRANSLIT")) }

let byte_channel ch = ch.channel
let encoding ch = ch.encoding

let close ch = Lwt_io.close ch.channel
let flush ch = Lwt_io.flush ch.channel

let atomic f ch = Lwt_io.atomic (fun ch' -> f { ch with channel = ch' }) ch.channel

let open_file ?buffer_size ?strict ?encoding ?flags ?perm ~mode name =
  make ?strict ?encoding (Lwt_io.open_file ?flags ?perm ~mode name)

let with_file ?buffer_size ?strict ?encoding ?flags ?perm ~mode name f =
  Lwt_io.with_file ?flags ?perm ~mode name (fun ch -> f (make ?strict ?encoding ch))

module Primitives =
struct
  (* +---------------------------------------------------------------+
     | Primitives for reading                                        |
     +---------------------------------------------------------------+ *)

  let rec read_char da strict decoder =
    let ptr = da.da_ptr and max = da.da_max in
    if ptr = max then
      da.da_perform () >>= function
        | 0 -> fail End_of_file
        | _ -> read_char da strict decoder
    else
      match Encoding.decode decoder da.da_buffer ptr (max - ptr) with
        | Encoding.Dec_ok(code, count) ->
            da.da_ptr <- ptr + count;
            return (Text.char code)
        | Encoding.Dec_need_more ->
            da.da_perform () >>= begin function
              | 0 ->
                  if strict then
                    fail (Failure "Lwt_text.read_char: unterminated multibyte sequence")
                  else begin
                    da.da_ptr <- ptr + 1;
                    return (Text.char (Char.code da.da_buffer.[ptr]))
                  end
              | _ ->
                  read_char da strict decoder
            end
        | Encoding.Dec_error ->
            if strict then
              fail (Failure "Lwt_text.read_char: unterminated multibyte sequence")
            else begin
              da.da_ptr <- ptr + 1;
              return (Text.char (Char.code da.da_buffer.[ptr]))
            end

  let read_char_opt da strict decoder =
    try_lwt
      read_char da strict decoder >|= fun ch -> Some ch
    with
      | End_of_file ->
          return None
      | exn ->
          fail exn

  let rec read_all da strict decoder buf =
    lwt ch = read_char da strict decoder in
    Buffer.add_string buf ch;
    read_all da strict decoder buf

  let rec read_count da strict decoder buf = function
    | 0 ->
        return (Buffer.contents buf)
    | n ->
        lwt ch = read_char da strict decoder in
        Buffer.add_string buf ch;
        read_count da strict decoder buf (n - 1)

  let read count da strict decoder = match count with
    | None ->
        let buf = Buffer.create 512 in
        begin
          try_lwt
            read_all da strict decoder buf
          with
            | End_of_file ->
                return (Buffer.contents buf)
        end
    | Some 0 ->
        return ""
    | Some 1 ->
        begin
          try_lwt
            read_char da strict decoder
          with
            | End_of_file ->
                return ""
        end
    | Some len ->
        let buf = Buffer.create len in
        begin
          try_lwt
            read_count da strict decoder buf len
          with
            | End_of_file ->
                return (Buffer.contents buf)
        end

  let read_line da strict decoder =
    let buf = Buffer.create 128 in
    let rec loop cr_read =
      try_bind (fun _ -> read_char da strict decoder)
        (function
           | "\n" ->
               return(Buffer.contents buf)
           | "\r" ->
               if cr_read then Buffer.add_char buf '\r';
               loop true
           | ch ->
               if cr_read then Buffer.add_char buf '\r';
               Buffer.add_string buf ch;
               loop false)
        (function
           | End_of_file ->
               if cr_read then Buffer.add_char buf '\r';
               return(Buffer.contents buf)
           | exn ->
               fail exn)
    in
    read_char da strict decoder >>= function
      | "\r" -> loop true
      | "\n" -> return ""
      | ch -> Buffer.add_string buf ch; loop false

  let read_line_opt da strict decoder =
    try_lwt
      read_line da strict decoder >|= fun ch -> Some ch
    with
      | End_of_file ->
          return None
      | exn ->
          fail exn

  (* +---------------------------------------------------------------+
     | Primitives for writing                                        |
     +---------------------------------------------------------------+ *)

  let rec write_code da encoder code =
    match Encoding.encode encoder da.da_buffer da.da_ptr (da.da_max - da.da_ptr) code with
      | Encoding.Enc_ok count ->
          da.da_ptr <- da.da_ptr + count;
          return ()
      | Encoding.Enc_need_more ->
          lwt _ = da.da_perform () in
          write_code da encoder code
      | Encoding.Enc_error ->
          fail (Failure "Lwt_text: cannot encode character")

  let byte str pos = Char.code (String.unsafe_get str pos)

  let next_code str i len =
    let n = byte str i in
    let rec trail j acc = function
      | 0 ->
          (j, acc)
      | count ->
          if j = len then
            (i + 1, n)
          else
            let m = byte str j in
            if m land 0xc0 = 0x80 then
              trail (j + 1) ((acc lsl 6) lor (m land 0x3f)) (count - 1)
            else
              (i + 1, n)
    in
    if n land 0x80 = 0 then
      (i + 1, n)
    else if n land 0xe0 = 0xc0 then
      trail (i + 1) (n land 0x1f) 1
    else if n land 0xf0 = 0xe0 then
      trail (i + 1) (n land 0x0f) 2
    else if n land 0xf8 = 0xf0 then
      trail (i + 1) (n land 0x07) 3
    else
      (i + 1, n)

  let write_char da strict encoder = function
    | "" ->
        fail (Invalid_argument "Lwt_text.write_char: empty text")
    | ch ->
        let _, code = next_code ch 0 (String.length ch) in
        write_code da encoder code

  let rec write_all da strict encoder str i len =
    if i = len then
      return ()
    else
      let i, code = next_code str i len in
      lwt () = write_code da encoder code in
      write_all da strict encoder str i len

  let write da strict encoder txt =
    write_all da strict encoder txt 0 (String.length txt)

  let write_line da strict encoder txt =
    lwt () = write_all da strict encoder txt 0 (String.length txt) in
    write_code da encoder 10
end

let read_char ic = direct_access ic.channel (fun da -> Primitives.read_char da ic.strict (decoder ic.coder))
let read_char_opt ic = direct_access ic.channel (fun da -> Primitives.read_char_opt da ic.strict (decoder ic.coder))
let read ?count ic = direct_access ic.channel (fun da -> Primitives.read count da ic.strict (decoder ic.coder))
let read_line ic = direct_access ic.channel (fun da -> Primitives.read_line da ic.strict (decoder ic.coder))
let read_line_opt ic = direct_access ic.channel (fun da -> Primitives.read_line_opt da ic.strict (decoder ic.coder))
let read_chars ic = Lwt_stream.from (fun _ -> read_char_opt ic)
let read_lines ic = Lwt_stream.from (fun _ -> read_line_opt ic)

let write_char oc x = direct_access oc.channel (fun da -> Primitives.write_char da oc.strict (encoder oc.coder) x)
let write_line oc x = direct_access oc.channel (fun da -> Primitives.write_line da oc.strict (encoder oc.coder) x)
let write oc x = direct_access oc.channel (fun da -> Primitives.write da oc.strict (encoder oc.coder) x)
let write_chars oc st = Lwt_stream.iter_s (write_char oc) st
let write_lines oc st = Lwt_stream.iter_s (write_line oc) st

let stdin = make Lwt_io.stdin
let stdout = make Lwt_io.stdout
let stderr = make Lwt_io.stderr
let null = make Lwt_io.null
let zero = make Lwt_io.zero

let fprint oc txt = write oc txt
let fprintl oc txt = write_line oc txt
let fprintf oc fmt = Printf.ksprintf (fun txt -> write oc txt) fmt
let fprintlf oc fmt = Printf.ksprintf (fun txt -> write_line oc txt) fmt

let print txt = write stdout txt
let printl txt = write_line stdout txt
let printf fmt = Printf.ksprintf print fmt
let printlf fmt = Printf.ksprintf printl fmt

let eprint txt = write stderr txt
let eprintl txt = write_line stderr txt
let eprintf fmt = Printf.ksprintf eprint fmt
let eprintlf fmt = Printf.ksprintf eprintl fmt

let ignore_close ch =
  ignore (close ch)

let make_stream f ic =
  Gc.finalise ignore_close ic;
  Lwt_stream.from (fun _ ->
                     try_lwt
                       f ic >|= fun x -> Some x
                     with
                       | End_of_file ->
                           lwt () = close ic in
                           return None)

let lines_of_file filename =
  make_stream read_line (open_file ~mode:input filename)

let lines_to_file filename lines =
  with_file ~mode:output filename (fun oc -> write_lines oc lines)

let chars_of_file filename =
  make_stream read_char (open_file ~mode:input filename)

let chars_to_file filename chars =
  with_file ~mode:output filename (fun oc -> write_chars oc chars)

let hexdump_stream oc stream = write_lines oc (Lwt_stream.hexdump stream)
let hexdump oc buf = hexdump_stream oc (Lwt_stream.of_string buf)
