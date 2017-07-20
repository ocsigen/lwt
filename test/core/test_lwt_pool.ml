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
         let gen = fun () -> Lwt.return [1; 2; 3] in
         let p = Lwt_pool.create 1 gen in
         let x = Lwt_pool.use p (fun l -> Lwt.return (List.length l)) in
         Lwt.return (Lwt.state x = Lwt.Return 3)
      );

    test "creator exception"
      (fun () ->
         let gen = fun () -> Lwt.fail Dummy_error in
         let p = Lwt_pool.create 1 gen in
         let f = fun () -> Lwt_pool.use p (fun _ -> Lwt.return 0) in
         let h =
           (fun exn ->
              if exn = Dummy_error
              then Lwt.return 1
              else Lwt.return 2) in
         let c = Lwt.catch f h in
         Lwt.return (Lwt.state c = Lwt.Return 1)
      );

    test "pool elements are reused"
      (fun () ->
         let gen = (fun () -> let l = ref [] in Lwt.return l) in
         let p = Lwt_pool.create 1 gen in
         let _ = Lwt_pool.use p (fun l -> l := 1 :: !l; Lwt.return (List.length !l)) in
         let u2 = Lwt_pool.use p (fun l -> Lwt.return (List.length !l)) in
         Lwt.return (Lwt.state u2 = Lwt.Return 1)
      );

    test "validation"
      (fun () ->
         let gen = (fun () -> let l = ref [] in Lwt.return l) in
         let v l = Lwt.return (List.length !l = 0) in
         let p = Lwt_pool.create 1 ~validate:v gen in
         let _ = Lwt_pool.use p (fun l -> l := 1 :: !l; Lwt.return (List.length !l)) in
         let u2 = Lwt_pool.use p (fun l -> Lwt.return (List.length !l)) in
         Lwt.return (Lwt.state u2 = Lwt.Return 0)
      );

     test "failed validation"
      (fun () ->
         let gen = (fun () -> let l = ref [] in Lwt.return l) in
         let v l =
           if List.length! l == 0
           then Lwt.return true
           else Lwt.fail Dummy_error
         in
         let p = Lwt_pool.create 1 ~validate:v gen in
         let u1 = Lwt_pool.use p (fun l -> l := 1 :: !l; Lwt.return (List.length !l)) in
         let _ = Lwt_pool.use p (fun l -> Lwt.return (List.length !l)) in
         Lwt.return (Lwt.state u1 = Lwt.Return 1) (*  && Lwt.state u2 = Lwt.Return 0) *)
      );

    test "multiple creation"
      (fun () ->
         let gen = (fun () -> let l = ref [] in Lwt.return l) in
         let p = Lwt_pool.create 2 gen in
         let _ = Lwt_pool.use p (fun l -> l := 1 :: !l; Lwt.pause ()) in
         let u2 = Lwt_pool.use p (fun l -> Lwt.return (List.length !l)) in
         Lwt.return (Lwt.state u2 = Lwt.Return 0)
      );

    test "limited creation of pool elements"
      (fun () ->
         let gen = (fun () -> let l = [] in Lwt.return l) in
         let p = Lwt_pool.create 1 gen in
         let _ = Lwt_pool.use p (fun _ -> Lwt.pause ()) in
         let u2 = Lwt_pool.use p (fun l -> Lwt.return (List.length l)) in
         Lwt.return (Lwt.state u2 = Lwt.Sleep)
      );

    test "check, elt ok"
      (fun () ->
         let gen = (fun () -> let l = ref [] in Lwt.return l) in
         let c = (fun x f -> f (List.length !x > 0)) in
         let p = Lwt_pool.create 1 ~check: c gen in
         let task = (fun l -> l := 1 :: !l; Lwt.return (List.length !l)) in
         let _ = Lwt_pool.use p (fun l -> l := 1 :: !l; Lwt.fail Dummy_error) in
         let u2 = Lwt_pool.use p task in
         Lwt.return (Lwt.state u2 = Lwt.Return 2)
      );

    test "checked, elt bad"
      (fun () ->
         let gen = (fun () -> let l = ref [] in Lwt.return l) in
         let c = (fun x f -> f (List.length !x = 0)) in
         let p = Lwt_pool.create 1 ~check: c gen in
         let task = (fun l -> l := 1 :: !l; Lwt.return (List.length !l)) in
         let _ = Lwt_pool.use p (fun l -> l := 1 :: !l; Lwt.fail Dummy_error) in
         let u2 = Lwt_pool.use p task in
         let u3 = Lwt_pool.use p task in (* this can go *)
         Lwt.return (Lwt.state u2 = Lwt.Return 1 && Lwt.state u3 = Lwt.Return 2)
      );
  ]
