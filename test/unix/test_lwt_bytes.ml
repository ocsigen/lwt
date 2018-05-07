(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test

let suite = suite "lwt_bytes" [
  test "create" begin fun () ->
    let len = 5 in
    let buff = Lwt_bytes.create len in
    let len' = Lwt_bytes.length buff in
    Lwt.return (len = len')
  end;
]
