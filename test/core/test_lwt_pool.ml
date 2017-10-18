(* OCaml promise library
 * http://www.ocsigen.org/lwt
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

exception Dummy_error

let suite =
  suite "lwt_pool" [

    test "basic create-use"
      (fun () ->
         let gen = fun () -> Lwt.return () in
         let p = Lwt_pool.create 1 gen in
         Lwt.return (Lwt.state (Lwt_pool.use p Lwt.return) = Lwt.Return ())
      );

    test "creator exception"
      (fun () ->
         let gen = fun () -> Lwt.fail Dummy_error in
         let p = Lwt_pool.create 1 gen in
         let u = Lwt_pool.use p (fun _ -> Lwt.return 0) in
         Lwt.return (Lwt.state u = Lwt.Fail Dummy_error)
      );

    test "pool elements are reused"
      (fun () ->
         let gen = (fun () -> let n = ref 0 in Lwt.return n) in
         let p = Lwt_pool.create 1 gen in
         let _ = Lwt_pool.use p (fun n -> n := 1; Lwt.return !n) in
         let u2 = Lwt_pool.use p (fun n -> Lwt.return !n) in
         Lwt.return (Lwt.state u2 = Lwt.Return 1)
      );

    test "pool elements are validated when returned"
      (fun () ->
         let gen = (fun () -> let n = ref 0 in Lwt.return n) in
         let v l = Lwt.return (!l = 0) in
         let p = Lwt_pool.create 1 ~validate:v gen in
         let _ = Lwt_pool.use p (fun n -> n := 1; Lwt.return !n) in
         let u2 = Lwt_pool.use p (fun n -> Lwt.return !n) in
         Lwt.return (Lwt.state u2 = Lwt.Return 0)
      );

    test "validation exceptions are propagated to users"
      (fun () ->
         let c = Lwt_condition.create () in
         let gen = (fun () -> let l = ref 0 in Lwt.return l) in
         let v l = if !l = 0 then Lwt.return true else Lwt.fail Dummy_error in
         let p = Lwt_pool.create 1 ~validate:v gen in
         let u1 = Lwt_pool.use p (fun l -> l := 1; Lwt_condition.wait c) in
         let u2 = Lwt_pool.use p (fun l -> Lwt.return !l) in
         let () = Lwt_condition.signal c "done" in
         Lwt.return (Lwt.state u1 = Lwt.Return "done"
                     && Lwt.state u2 = Lwt.Fail Dummy_error)
      );

    test "multiple creation"
      (fun () ->
         let gen = (fun () -> let n = ref 0 in Lwt.return n) in
         let p = Lwt_pool.create 2 gen in
         let _ = Lwt_pool.use p (fun n -> n := 1; Lwt.pause ()) in
         let u2 = Lwt_pool.use p (fun n -> Lwt.return !n) in
         Lwt.return (Lwt.state u2 = Lwt.Return 0)
      );

    test "users of an empty pool will wait"
      (fun () ->
         let gen = (fun () -> Lwt.return 0) in
         let p = Lwt_pool.create 1 gen in
         let _ = Lwt_pool.use p (fun _ -> Lwt.pause ()) in
         let u2 = Lwt_pool.use p Lwt.return in
         Lwt.return (Lwt.state u2 = Lwt.Sleep)
      );

    test "on check, good elements are retained"
      (fun () ->
         let gen = (fun () -> let n = ref 1 in Lwt.return n) in
         let c = (fun x f -> f (!x > 0)) in
         let p = Lwt_pool.create 1 ~check: c gen in
         let _ = Lwt_pool.use p (fun n -> n := 2; Lwt.fail Dummy_error) in
         let u2 = Lwt_pool.use p (fun n -> Lwt.return !n) in
         Lwt.return (Lwt.state u2 = Lwt.Return 2)
      );

    test "on check, bad elements are disposed of and replaced"
      (fun () ->
         let gen = (fun () -> let n = ref 1 in Lwt.return n) in
         let check = (fun n f -> f (!n > 0)) in
         let disposed = ref false in
         let dispose _ = disposed := true; Lwt.return_unit in
         let p = Lwt_pool.create 1 ~check ~dispose gen in
         let task = (fun n -> incr n; Lwt.return !n) in
         let _ = Lwt_pool.use p (fun n -> n := 0; Lwt.fail Dummy_error) in
         let u2 = Lwt_pool.use p task in
         Lwt.return (Lwt.state u2 = Lwt.Return 2 && !disposed)
      );

    test "clear disposes of all elements"
      (fun () ->
         let gen = (fun () -> let n = ref 1 in Lwt.return n) in
         let count = ref 0 in
         let dispose _ = incr count; Lwt.return_unit in
         let p = Lwt_pool.create 2 ~dispose gen in
         let u = Lwt_pool.use p (fun _ -> Lwt.pause ()) in
         let _ = Lwt_pool.use p (fun _ -> Lwt.return_unit) in
         let _ = Lwt_pool.clear p in
         Lwt.bind u (fun () -> Lwt.return (!count = 2))
      );

    test "waiter are notified on replacement"
      (fun () ->
         let c = Lwt_condition.create () in
         let gen = (fun () -> let l = ref 0 in Lwt.return l) in
         let v l = if !l = 0 then Lwt.return true else Lwt.fail Dummy_error in
         let p = Lwt_pool.create 1 ~validate:v gen in
         let u1 = Lwt_pool.use p (fun l -> l := 1; Lwt_condition.wait c) in
         let u2 = Lwt_pool.use p (fun l -> Lwt.return !l) in
         let u3 = Lwt_pool.use p (fun l -> Lwt.return !l) in
         let () = Lwt_condition.signal c "done" in
         Lwt.return (Lwt.state u1 = Lwt.Return "done"
                     && Lwt.state u2 = Lwt.Fail Dummy_error
                     && Lwt.state u3 = Lwt.Return 0)
      );

    test "waiter are notified on replacement exception"
      (fun () ->
         let c = Lwt_condition.create () in
         let k = ref true in
         let gen = (fun () ->
           if !k then
             let l = ref 0 in Lwt.return l
           else
             Lwt.fail Dummy_error
         ) in
         let v l = if !l = 0 then Lwt.return true else Lwt.fail Dummy_error in
         let p = Lwt_pool.create 1 ~validate:v gen in
         let u1 = Lwt_pool.use p (fun l -> l := 1; k:= false; Lwt_condition.wait c) in
         let u2 = Lwt_pool.use p (fun l -> Lwt.return !l) in
         let u3 = Lwt_pool.use p (fun l -> Lwt.return !l) in
         let () = Lwt_condition.signal c "done" in
         Lwt.return (Lwt.state u1 = Lwt.Return "done"
                     && Lwt.state u2 = Lwt.Fail Dummy_error
                     && Lwt.state u3 = Lwt.Fail Dummy_error)
      );

    test "check and validate can be used together"
      (fun () ->
         let gen = (fun () -> let l = ref 0 in Lwt.return l) in
         let v l = Lwt.return (!l > 0) in
         let c l f = f (!l > 1) in
         let cond = Lwt_condition.create() in
         let p = Lwt_pool.create 1 ~validate:v ~check:c gen in
         let _ = Lwt_pool.use p (fun l -> l := 1; Lwt_condition.wait cond) in
         let _ = Lwt_pool.use p (fun l -> l := 2; Lwt.fail Dummy_error) in
         let u3 = Lwt_pool.use p (fun l -> Lwt.return !l) in
         let () = Lwt_condition.signal cond "done" in
         Lwt.return (Lwt.state u3 = Lwt.Return 2)
      );

    test "verify default check behavior"
      (fun () ->
         let gen = (fun () -> let l = ref 0 in Lwt.return l) in
         let cond = Lwt_condition.create() in
         let p = Lwt_pool.create 1 gen in
         let _ = Lwt_pool.use p (fun l ->
           Lwt.bind (Lwt_condition.wait cond)
             (fun _ -> l:= 1; Lwt.fail Dummy_error)) in
         let u2 = Lwt_pool.use p (fun l -> Lwt.return !l) in
         let () = Lwt_condition.signal cond "done" in
         Lwt.return (Lwt.state u2 = Lwt.Return 1)
      );
  ]
