
(* OCaml promise library
 * http://www.ocsigen.org/lwt
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

open Test

[@@@ocaml.warning "-3"]
module Lwt_sequence = Lwt_sequence
[@@@ocaml.warning "+3"]

let filled_sequence () =
  let s = Lwt_sequence.create () in
  let _ = Lwt_sequence.add_l (-1) s in
  let _ = Lwt_sequence.add_l (-2) s in
  let _ = Lwt_sequence.add_l (-3) s in
  let _ = Lwt_sequence.add_r 1 s in
  let _ = Lwt_sequence.add_r 2 s in
  let _ = Lwt_sequence.add_r 3 s in
  s

let filled_length = 6

let leftmost_value = (-3)

let rightmost_value = 3

let transfer_sequence () =
  let s = Lwt_sequence.create () in
  let _ = Lwt_sequence.add_r 4 s in
  let _ = Lwt_sequence.add_r 5 s in
  s

let transfer_length = 2

let empty_array = [||]

let l_filled_array = [|-3; -2; -1; 1; 2; 3|]

let r_filled_array = [|3; 2; 1; -1; -2; -3|]

let test_iter iter_f array_values seq =
  let index = ref 0 in
  Lwt.catch
   (fun () ->
    iter_f (fun v ->
              assert (v = array_values.(!index));
              index := (!index + 1)) seq;
    Lwt.return_true
   )
   (function _ -> Lwt.return_false)

let test_iter_node iter_f array_values seq =
  let index = ref 0 in
  Lwt.catch
   (fun () ->
    iter_f (fun n ->
              assert ((Lwt_sequence.get n) = array_values.(!index));
              index := (!index + 1)) seq;
    Lwt.return_true
   )
   (function _ -> Lwt.return_false)

let test_iter_rem iter_f array_values seq =
  let index = ref 0 in
  Lwt.catch
   (fun () ->
    iter_f (fun n ->
              assert ((Lwt_sequence.get n) = array_values.(!index));
              Lwt_sequence.remove n;
              index := (!index + 1)) seq;
    Lwt.return_true
   )
   (function _ -> Lwt.return_false)

let suite = suite "lwt_sequence" [

  test "create" begin fun () ->
    let s = Lwt_sequence.create () in
    let _ = assert (Lwt_sequence.is_empty s) in
    let len = Lwt_sequence.length s in
    Lwt.return (len = 0)
  end;

  test "add_l" begin fun () ->
    let s = Lwt_sequence.create () in
    let n = Lwt_sequence.add_l 1 s in
    let _ = assert ((Lwt_sequence.get n) = 1) in
    let len = Lwt_sequence.length s in
    Lwt.return (len = 1)
  end;

  test "add_r" begin fun () ->
    let s = Lwt_sequence.create () in
    let n = Lwt_sequence.add_r 1 s in
    let _ = assert ((Lwt_sequence.get n) = 1) in
    let len = Lwt_sequence.length s in
    Lwt.return (len = 1)
  end;

  test "take_l Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    Lwt.catch
      (fun () ->
        let _ = Lwt_sequence.take_l s in
        Lwt.return_false
      )
      (function
        | Lwt_sequence.Empty -> Lwt.return_true
        | _ -> Lwt.return_false
      )
  end;

  test "take_l" begin fun () ->
    let s = filled_sequence () in
    Lwt.catch
      (fun () ->
        let v = Lwt_sequence.take_l s in
        Lwt.return (leftmost_value = v)
      )
      (function
        | _ -> Lwt.return_false
      )
  end;

  test "take_r Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    Lwt.catch
      (fun () ->
        let _ = Lwt_sequence.take_r s in
        Lwt.return_false
      )
      (function
        | Lwt_sequence.Empty -> Lwt.return_true
        | _ -> Lwt.return_false
      )
  end;

  test "take_r" begin fun () ->
    let s = filled_sequence () in
    Lwt.catch
      (fun () ->
        let v = Lwt_sequence.take_r s in
        Lwt.return (rightmost_value = v)
      )
      (function
        | _ -> Lwt.return_false
      )
  end;

  test "take_opt_l Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    match Lwt_sequence.take_opt_l s with
    | None -> Lwt.return_true
    | _ -> Lwt.return_false
  end;

  test "take_opt_l" begin fun () ->
    let s = filled_sequence () in
    match Lwt_sequence.take_opt_l s with
    | None -> Lwt.return_false
    | Some v -> Lwt.return (leftmost_value = v)
  end;

  test "take_opt_r Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    match Lwt_sequence.take_opt_r s with
    | None -> Lwt.return_true
    | _ -> Lwt.return_false
  end;

  test "take_opt_r" begin fun () ->
    let s = filled_sequence () in
    match Lwt_sequence.take_opt_r s with
    | None -> Lwt.return_false
    | Some v -> Lwt.return (rightmost_value = v)
  end;

  test "transfer_l Empty" begin fun () ->
    let s = filled_sequence () in
    let ts = Lwt_sequence.create () in
    let _ = Lwt_sequence.transfer_l ts s in
    let len = Lwt_sequence.length s in
    Lwt.return (filled_length = len)
  end;

  test "transfer_l " begin fun () ->
    let s = filled_sequence () in
    let ts = transfer_sequence () in
    let _ = Lwt_sequence.transfer_l ts s in
    let len = Lwt_sequence.length s in
    let _ = assert ((filled_length + transfer_length) = len) in
    match Lwt_sequence.take_opt_l s with
    | None -> Lwt.return_false
    | Some v -> Lwt.return (4 = v)
  end;

  test "transfer_r Empty" begin fun () ->
    let s = filled_sequence () in
    let ts = Lwt_sequence.create () in
    let _ = Lwt_sequence.transfer_r ts s in
    let len = Lwt_sequence.length s in
    Lwt.return (filled_length = len)
  end;

  test "transfer_r " begin fun () ->
    let s = filled_sequence () in
    let ts = transfer_sequence () in
    let _ = Lwt_sequence.transfer_r ts s in
    let len = Lwt_sequence.length s in
    let _ = assert ((filled_length + transfer_length) = len) in
    match Lwt_sequence.take_opt_r s with
    | None -> Lwt.return_false
    | Some v -> Lwt.return (5 = v)
  end;

  test "iter_l Empty" begin fun () ->
    test_iter Lwt_sequence.iter_l empty_array (Lwt_sequence.create ())
  end;

  test "iter_l" begin fun () ->
    test_iter Lwt_sequence.iter_l l_filled_array (filled_sequence ())
  end;

  test "iter_r Empty" begin fun () ->
    test_iter Lwt_sequence.iter_r empty_array (Lwt_sequence.create ())
  end;

  test "iter_r" begin fun () ->
    test_iter Lwt_sequence.iter_r r_filled_array (filled_sequence ())
  end;

  test "iter_node_l Empty" begin fun () ->
    test_iter_node Lwt_sequence.iter_node_l empty_array (Lwt_sequence.create ())
  end;

  test "iter_node_l" begin fun () ->
    test_iter_node Lwt_sequence.iter_node_l l_filled_array (filled_sequence ())
  end;

  test "iter_node_r Empty" begin fun () ->
    test_iter_node Lwt_sequence.iter_node_r empty_array (Lwt_sequence.create ())
  end;

  test "iter_node_r" begin fun () ->
    test_iter_node Lwt_sequence.iter_node_r r_filled_array (filled_sequence ())
  end;

  test "iter_node_l with removal" begin fun () ->
    test_iter_rem Lwt_sequence.iter_node_l l_filled_array (filled_sequence ())
  end;

  test "iter_node_r with removal" begin fun () ->
    test_iter_rem Lwt_sequence.iter_node_r r_filled_array (filled_sequence ())
  end;

  test "fold_l" begin fun () ->
    let acc = Lwt_sequence.fold_l (fun v e -> v * e) (filled_sequence ()) 1 in
    Lwt.return (acc = (-36))
  end;

  test "fold_l Empty" begin fun () ->
    let acc = Lwt_sequence.fold_l (fun v e -> v * e) (Lwt_sequence.create ()) 1 in
    Lwt.return (acc = 1)
  end;

  test "fold_r" begin fun () ->
    let acc = Lwt_sequence.fold_r (fun v e -> v * e) (filled_sequence ()) 1 in
    Lwt.return (acc = (-36))
  end;

  test "fold_r Empty" begin fun () ->
    let acc = Lwt_sequence.fold_r (fun v e -> v * e) (Lwt_sequence.create ()) 1 in
    Lwt.return (acc = 1)
  end;

  test "find_node_opt_l Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    match Lwt_sequence.find_node_opt_l (fun v -> v = 1) s with
    | None -> Lwt.return_true
    | _ -> Lwt.return_false
  end;

  test "find_node_opt_l not found " begin fun () ->
    let s = transfer_sequence () in
    match Lwt_sequence.find_node_opt_l (fun v -> v = 1) s with
    | None -> Lwt.return_true
    | _ -> Lwt.return_false
  end;

  test "find_node_opt_l" begin fun () ->
    let s = filled_sequence () in
    match Lwt_sequence.find_node_opt_l (fun v -> v = 1) s with
    | None -> Lwt.return_false
    | Some n -> if ((Lwt_sequence.get n) = 1) then Lwt.return_true
      else Lwt.return_false
  end;

  test "find_node_opt_r Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    match Lwt_sequence.find_node_opt_r (fun v -> v = 1) s with
    | None -> Lwt.return_true
    | _ -> Lwt.return_false
  end;

  test "find_node_opt_r not found " begin fun () ->
    let s = transfer_sequence () in
    match Lwt_sequence.find_node_opt_r (fun v -> v = 1) s with
    | None -> Lwt.return_true
    | _ -> Lwt.return_false
  end;

  test "find_node_opt_r" begin fun () ->
    let s = filled_sequence () in
    match Lwt_sequence.find_node_opt_r (fun v -> v = 1) s with
    | None -> Lwt.return_false
    | Some n -> if ((Lwt_sequence.get n) = 1) then Lwt.return_true
      else Lwt.return_false
  end;

  test "find_node_l Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    Lwt.catch
    (fun () -> let n = Lwt_sequence.find_node_l (fun v -> v = 1) s in
       if ((Lwt_sequence.get n) = 1) then Lwt.return_false
       else Lwt.return_false
    )
    (function
      | Not_found -> Lwt.return_true
      | _ -> Lwt.return_false
    )
  end;

  test "find_node_l" begin fun () ->
    let s = filled_sequence () in
    Lwt.catch
    (fun () -> let n = Lwt_sequence.find_node_l (fun v -> v = 1) s in
       if ((Lwt_sequence.get n) = 1) then Lwt.return_true
       else Lwt.return_false
    )
    (function _ -> Lwt.return_false)
  end;

  test "find_node_r Empty" begin fun () ->
    let s = Lwt_sequence.create () in
    Lwt.catch
    (fun () -> let n = Lwt_sequence.find_node_r (fun v -> v = 1) s in
       if ((Lwt_sequence.get n) = 1) then Lwt.return_false
       else Lwt.return_false
    )
    (function
      | Not_found -> Lwt.return_true
      | _ -> Lwt.return_false
    )
  end;

  test "find_node_r" begin fun () ->
    let s = filled_sequence () in
    Lwt.catch
    (fun () -> let n = Lwt_sequence.find_node_r (fun v -> v = 1) s in
       if ((Lwt_sequence.get n) = 1) then Lwt.return_true
       else Lwt.return_false
    )
    (function _ -> Lwt.return_false)
  end;

  test "set" begin fun () ->
    let s = filled_sequence () in
    match Lwt_sequence.find_node_opt_l (fun v -> v = 1) s with
    | None -> Lwt.return_false
    | Some n -> let _ = Lwt_sequence.set n 10 in
      let data = [|-3; -2; -1; 10; 2; 3|] in
      test_iter Lwt_sequence.iter_l data s
  end;
]
