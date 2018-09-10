(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Test

let bytes_equal (b1:Bytes.t) (b2:Bytes.t) = b1 = b2

let suite = suite "lwt_bytes" [
    test "create" begin fun () ->
      let len = 5 in
      let buff = Lwt_bytes.create len in
      let len' = Bigarray.Array1.dim buff in
      Lwt.return (len = len')
    end;

    test "get/set" begin fun () ->
      let buff = Lwt_bytes.create 4 in
      let () = Lwt_bytes.set buff 0 'a' in
      let () = Lwt_bytes.set buff 1 'b' in
      let () = Lwt_bytes.set buff 2 'c' in
      let check = Lwt_bytes.get buff 0 = 'a' &&
                  Lwt_bytes.get buff 1 = 'b' &&
                  Lwt_bytes.get buff 2 = 'c'
      in Lwt.return check
    end;

    test "get out of bounds : lower limit" begin fun () ->
      let buff = Lwt_bytes.create 3 in
      try
        let _ = Lwt_bytes.get buff (-1) in
        Lwt.return false
      with
      | Invalid_argument _ -> Lwt.return true
      | _ -> Lwt.return false
    end;

    test "get out of bounds : upper limit" begin fun () ->
      let buff = Lwt_bytes.create 3 in
      try
        let _ = Lwt_bytes.get buff 3 in
        Lwt.return false
      with
      | Invalid_argument _ -> Lwt.return true
      | _ -> Lwt.return false
    end;

    test "set out of bounds : lower limit" begin fun () ->
      let buff = Lwt_bytes.create 3 in
      try
        let () = Lwt_bytes.set buff (-1) 'a' in
        Lwt.return true
      with
      | Invalid_argument _ -> Lwt.return true
      | _ -> Lwt.return false
    end;

    test "set out of bounds : upper limit" begin fun () ->
      let buff = Lwt_bytes.create 3 in
      try
        let () = Lwt_bytes.set buff 3 'a' in
        Lwt.return true
      with
      | Invalid_argument _ -> Lwt.return true
      | _ -> Lwt.return false
    end;

    test "unsafe_get/unsafe_set" begin fun () ->
      let buff = Lwt_bytes.create 4 in
      let () = Lwt_bytes.unsafe_set buff 0 'a' in
      let () = Lwt_bytes.unsafe_set buff 1 'b' in
      let () = Lwt_bytes.unsafe_set buff 2 'c' in
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
      let check = bytes_equal bytes bytes' in
      Lwt.return check
    end;

    test "to string" begin fun () ->
      let str = "abc" in
      let buff = Lwt_bytes.of_string str in
      let str' = Lwt_bytes.to_string buff in
      let check = str = str' in
      Lwt.return check
    end;

    test "blit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      let () = Lwt_bytes.blit buf1 0 buf2 3 3 in
      let check = "abcabc" = Lwt_bytes.to_string buf2 in
      Lwt.return check
    end;

    test "blit source out of bounds: lower limit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      try
        let() = Lwt_bytes.blit buf1 (-1) buf2 3 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return true
      | _ -> Lwt.return false
    end;

    test "blit source out of bounds: upper limit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      try
        let() = Lwt_bytes.blit buf1 1 buf2 3 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return true
      | _ -> Lwt.return false
    end;

    test "blit destination out of bounds: lower limit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      try
        let() = Lwt_bytes.blit buf1 0 buf2 (-1) 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return true
      | _ -> Lwt.return false
    end;

    test "blit destination out of bounds: upper limit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      try
        let() = Lwt_bytes.blit buf1 0 buf2 4 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return true
      | _ -> Lwt.return false
    end;

    test "blit length out of bounds: lower limit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      try
        let() = Lwt_bytes.blit buf1 0 buf2 3 (-1) in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return true
      | _ -> Lwt.return false
    end;

    test "blit from bytes" begin fun () ->
      let bytes1 = Bytes.of_string "abc" in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      let () = Lwt_bytes.blit_from_bytes bytes1 0 buf2 3 3 in
      let check = "abcabc" = Lwt_bytes.to_string buf2 in
      Lwt.return check
    end;

    test "blit from bytes source out of bounds: lower limit" begin fun () ->
      let bytes1 = Bytes.of_string "abc" in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      try
        let () = Lwt_bytes.blit_from_bytes bytes1 (-1) buf2 3 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "blit from bytes source out of bounds: upper limit" begin fun () ->
      let bytes1 = Bytes.of_string "abc" in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      try
        let () = Lwt_bytes.blit_from_bytes bytes1 1 buf2 3 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "blit from bytes destination out of bounds: lower limit" begin fun () ->
      let bytes1 = Bytes.of_string "abc" in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      try
        let () = Lwt_bytes.blit_from_bytes bytes1 0 buf2 (-1) 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "blit from bytes destination out of bounds: upper limit" begin fun () ->
      let bytes1 = Bytes.of_string "abc" in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      try
        let () = Lwt_bytes.blit_from_bytes bytes1 0 buf2 4 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "blit from bytes length out of bounds: lower limit" begin fun () ->
      let bytes1 = Bytes.of_string "abc" in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      try
        let () = Lwt_bytes.blit_from_bytes bytes1 0 buf2 3 (-1) in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "blit to bytes" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let bytes2 = Bytes.of_string str2 in
      let () = Lwt_bytes.blit_to_bytes buf1 0 bytes2 3 3 in
      let check = "abcabc" = Bytes.to_string bytes2 in
      Lwt.return check
    end;

    test "blit to bytes source out of bounds: lower limit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let bytes2 = Bytes.of_string str2 in
      try
        let () = Lwt_bytes.blit_to_bytes buf1 (-1) bytes2 3 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "blit to bytes source out of bounds: upper limit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let bytes2 = Bytes.of_string str2 in
      try
        let () = Lwt_bytes.blit_to_bytes buf1 1 bytes2 3 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "blit to bytes destination out of bounds: lower limit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let bytes2 = Bytes.of_string str2 in
      try
        let () = Lwt_bytes.blit_to_bytes buf1 0 bytes2 (-1) 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "blit to bytes destination out of bounds: upper limit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let bytes2 = Bytes.of_string str2 in
      try
        let () = Lwt_bytes.blit_to_bytes buf1 0 bytes2 4 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "blit to bytes length out of bounds: lower limit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let bytes2 = Bytes.of_string str2 in
      try
        let () = Lwt_bytes.blit_to_bytes buf1 0 bytes2 3 (-1) in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "unsafe blit" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      let () = Lwt_bytes.unsafe_blit buf1 0 buf2 3 3 in
      let check = "abcabc" = Lwt_bytes.to_string buf2 in
      Lwt.return check
    end;

    test "unsafe blit from bytes" begin fun () ->
      let bytes1 = Bytes.of_string "abc" in
      let str2 = "abcdef" in
      let buf2 = Lwt_bytes.of_string str2 in
      let () = Lwt_bytes.unsafe_blit_from_bytes bytes1 0 buf2 3 3 in
      let check = "abcabc" = Lwt_bytes.to_string buf2 in
      Lwt.return check
    end;

    test "unsafe blit to bytes" begin fun () ->
      let str1 = "abc" in
      let buf1 = Lwt_bytes.of_string str1 in
      let str2 = "abcdef" in
      let bytes2 = Bytes.of_string str2 in
      let () = Lwt_bytes.unsafe_blit_to_bytes buf1 0 bytes2 3 3 in
      let check = "abcabc" = Bytes.to_string bytes2 in
      Lwt.return check
    end;

    test "proxy" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      let buf' = Lwt_bytes.proxy buf 3 3 in
      let check1 = "def" = Lwt_bytes.to_string buf' in
      let () = Lwt_bytes.set buf 3 'a' in
      let check2 = "aef" = Lwt_bytes.to_string buf' in
      Lwt.return (check1 && check2)
    end;

    test "proxy offset out of bounds: lower limit" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      try
      let _ = Lwt_bytes.proxy buf (-1) 3 in
      Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "proxy offset out of bounds: upper limit" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      try
      let _ = Lwt_bytes.proxy buf 4 3 in
      Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "proxy length out of bounds: lower limit" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      try
      let _ = Lwt_bytes.proxy buf 3 (-1) in
      Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "extract" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      let buf' = Lwt_bytes.extract buf 3 3 in
      let check = "def" = Lwt_bytes.to_string buf' in
      Lwt.return check
    end;

    test "extract offset out of bounds: lower limit" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      try
        let _ = Lwt_bytes.extract buf (-1) 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "extract offset out of bounds: upper limit" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      try
        let _ = Lwt_bytes.extract buf 4 3 in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "extract length out of bounds: lower limit" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      try
        let _ = Lwt_bytes.extract buf 3 (-1) in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "copy" begin fun () ->
      let str = "abc" in
      let buf = Lwt_bytes.of_string str in
      let buf' = Lwt_bytes.copy buf in
      let check = str = Lwt_bytes.to_string buf' in
      Lwt.return check
    end;

    test "fill" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      let () = Lwt_bytes.fill buf 3 3 'a' in
      let check = "abcaaa" = Lwt_bytes.to_string buf in
      Lwt.return check
    end;

    test "fill offset out of bounds: lower limit" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      try
        let () = Lwt_bytes.fill buf (-1) 3 'a' in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "fill offset out of bounds: upper limit" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      try
        let () = Lwt_bytes.fill buf 4 3 'a' in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "fill length out of bounds lower limit" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      try
        let () = Lwt_bytes.fill buf 3 (-1) 'a' in
        Lwt.return_false
      with
      | Invalid_argument _ -> Lwt.return_true
      | _ -> Lwt.return_false
    end;

    test "unsafe fill" begin fun () ->
      let str = "abcdef" in
      let buf = Lwt_bytes.of_string str in
      let () = Lwt_bytes.unsafe_fill buf 3 3 'a' in
      let check = "abcaaa" = Lwt_bytes.to_string buf in
      Lwt.return check
    end;
  ]
