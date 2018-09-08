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

  test "get/set" begin fun () ->
    let buff = Lwt_bytes.create 4 in
    let _ = Lwt_bytes.set buff 0 'a' in
    let _ = Lwt_bytes.set buff 1 'b' in
    let _ = Lwt_bytes.set buff 2 'c' in
    let check = Lwt_bytes.get buff 0 = 'a' &&
    Lwt_bytes.get buff 1 = 'b' &&
    Lwt_bytes.get buff 2 = 'c'
    in Lwt.return check
  end;

  test "get out of bounds" begin fun () ->
    let buff = Lwt_bytes.create 3 in
    try
      let _ = Lwt_bytes.get buff 150 in
      Lwt.return false
    with
    | Invalid_argument _ -> Lwt.return true
    | _ -> Lwt.return false
  end;

  test "set out of bounds" begin fun () ->
    let buff = Lwt_bytes.create 3 in
    try
    let _ = Lwt_bytes.set buff 150 in
    Lwt.return true
    with
    | Invalid_argument _ -> Lwt.return true
    | _ -> Lwt.return false
  end;

  test "unsafe_get/unsage_set" begin fun () ->
    let buff = Lwt_bytes.create 4 in
    let _ = Lwt_bytes.unsafe_set buff 0 'a' in
    let _ = Lwt_bytes.unsafe_set buff 1 'b' in
    let _ = Lwt_bytes.unsafe_set buff 2 'c' in
    let check = Lwt_bytes.unsafe_get buff 0 = 'a' &&
    Lwt_bytes.unsafe_get buff 1 = 'b' &&
    Lwt_bytes.unsafe_get buff 2 = 'c'
    in Lwt.return check
  end;

  test "of bytes" begin fun () ->
    let bytes = Bytes.of_string "abc" in
    let buff = Lwt_bytes.of_bytes bytes in
    let check = Lwt_bytes.get buff 0 = Bytes.get bytes 0 &&
                Lwt_bytes.get buff 1 = Bytes.get bytes 1 &&
                Lwt_bytes.get buff 2 = Bytes.get bytes 2
    in Lwt.return check
  end;

  test "of string" begin fun () ->
    let buff = Lwt_bytes.of_string "abc" in
    let check = Lwt_bytes.get buff 0 = 'a' &&
                Lwt_bytes.get buff 1 = 'b' &&
                Lwt_bytes.get buff 2 = 'c'
    in Lwt.return check
  end;

  test "to bytes" begin fun () ->
    let bytes = Bytes.of_string "abc" in
    let buff = Lwt_bytes.of_bytes bytes in
    let bytes' = Lwt_bytes.to_bytes buff in
    let check = Bytes.equal bytes bytes' in
    Lwt.return check
  end;

  test "to string" begin fun () ->
    let str = "abc" in
    let buff = Lwt_bytes.of_string str in
    let str' = Lwt_bytes.to_string buff in
    let check = str = str' in
    Lwt.return check
  end;
]
