(* OCaml promise library
 * http://www.ocsigen.org/lwt
 * Copyright (C) 2018 Gabriel Radanne
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

open Test
open Lwt.Infix

let testchan () =
  let b = Buffer.create 6 in
  let f buf ofs len =
    let bytes = Bytes.create len in
    Lwt_bytes.blit_to_bytes buf ofs bytes 0 len;
    Buffer.add_bytes b bytes;
    Lwt.return len
  in
  let oc = Lwt_io.make ~mode:Output f in
  let fmt = Lwt_fmt.of_channel oc in
  fmt, (fun () -> Buffer.contents b)

let suite = suite "lwt_fmt" [
    test "flushing" (fun () ->
        let fmt, f = testchan () in
        Lwt_fmt.fprintf fmt "%s%i%s%!" "bla" 3 "blo" >>= fun () ->
        Lwt.return (f () = {|bla3blo|})
      );
    test "with combinator" (fun () ->
        let fmt, f = testchan () in
        Lwt_fmt.fprintf fmt "%a%!" Format.pp_print_int 3 >>= fun () ->
        Lwt.return (f () = {|3|})
      );
    test "box" (fun () ->
        let fmt, f = testchan () in
        Lwt_fmt.fprintf fmt "@[<v2>%i@,%i@]%!" 1 2 >>= fun () ->
        Lwt.return (f () = "1\n  2")
      );
    test "boxsplit" (fun () ->
        let fmt, f = testchan () in
        Lwt_fmt.fprintf fmt "@[<v2>%i" 1 >>= fun () ->
        Lwt_fmt.fprintf fmt "@,%i@]" 2 >>= fun () ->
        Lwt_fmt.flush fmt >>= fun () ->
        Lwt.return (f () = "1\n  2")
      );
    test "box close with flush" (fun () ->
        let fmt, f = testchan () in
        Lwt_fmt.fprintf fmt "@[<v2>%i" 1 >>= fun () ->
        Lwt_fmt.fprintf fmt "@,%i" 2 >>= fun () ->
        Lwt_fmt.flush fmt >>= fun () ->
        Lwt.return (f () = "1\n  2")
      );

    test "stream"  (fun () ->
        let stream, fmt = Lwt_fmt.make_stream () in
        Lwt_fmt.fprintf fmt "@[<v2>%i@,%i@]%!" 1 2 >>= fun () ->
        Lwt.return (Lwt_stream.get_available stream = [
            String ("1", 0, 1);
            String ("\n", 0, 1);
            String  ("                                                                                ", 0, 2);
            String ("2", 0, 1);
            Flush])
      );
  ]
