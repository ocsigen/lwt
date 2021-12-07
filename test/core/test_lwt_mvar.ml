(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)

open Lwt.Infix
open Test

let state_is = Lwt.debug_state_is

let suite =
  suite "lwt_mvar"
    [
      test "basic take" (fun () ->
          let x = Lwt_mvar.create 0 in
          let y = Lwt_mvar.take x in
          state_is (Lwt.Return 0) y);
      test "take_available (full)" (fun () ->
          let x = Lwt_mvar.create 0 in
          let y = Lwt_mvar.take_available x in
          Lwt.return (y = Some 0));
      test "take_available (empty)" (fun () ->
          let x = Lwt_mvar.create_empty () in
          let y = Lwt_mvar.take_available x in
          Lwt.return (y = None));
      test "take_available (twice)" (fun () ->
          let x = Lwt_mvar.create 0 in
          let (_ : int option) = Lwt_mvar.take_available x in
          let y = Lwt_mvar.take_available x in
          Lwt.return (y = None));
      test "is_empty (full)" (fun () ->
          let x = Lwt_mvar.create 0 in
          let y = Lwt_mvar.is_empty x in
          Lwt.return (not y));
      test "is_empty (empty)" (fun () ->
          let x = Lwt_mvar.create_empty () in
          let y = Lwt_mvar.is_empty x in
          Lwt.return y);
      test "blocking put" (fun () ->
          let x = Lwt_mvar.create 0 in
          let y = Lwt_mvar.put x 1 in
          Lwt.return (Lwt.state y = Lwt.Sleep));
      test "put-take" (fun () ->
          let x = Lwt_mvar.create_empty () in
          let _ = Lwt_mvar.put x 0 in
          let y = Lwt_mvar.take x in
          state_is (Lwt.Return 0) y);
      test "take-put" (fun () ->
          let x = Lwt_mvar.create 0 in
          let _ = Lwt_mvar.take x in
          let y = Lwt_mvar.put x 1 in
          state_is (Lwt.Return ()) y);
      test "enqueued writer" (fun () ->
          let x = Lwt_mvar.create 1 in
          let y = Lwt_mvar.put x 2 in
          let z = Lwt_mvar.take x in
          state_is (Lwt.Return ()) y >>= fun y_correct ->
          state_is (Lwt.Return 1) z >>= fun z_correct ->
          Lwt.return (y_correct && z_correct));
      test "writer cancellation" (fun () ->
          let y = Lwt_mvar.create 1 in
          let r1 = Lwt_mvar.put y 2 in
          Lwt.cancel r1;
          Lwt.return
            (Lwt.state (Lwt_mvar.take y) = Lwt.Return 1
            && Lwt.state (Lwt_mvar.take y) = Lwt.Sleep));
    ]
