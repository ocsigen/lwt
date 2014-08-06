(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Ppx_lwt
 * Copyright (C) 2014 Gabriel Radanne, Peter Zotov.
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

(** Ppx syntax extension for lwt *)


(** {2 Ppx extensions}

   This extension add the following extensions and attributes :

   - lwt-binding:

   {[
     let%lwt ch = get_char stdin in
     code
   ]}

   is the same as [bind (get_char stdin) (fun ch -> code)]

   Moreover it supports parallel binding:

   {[
     let%lwt x = do_something1 ()
     and y = do_something2 in
     code
   ]}

   will let [do_something1 ()] and [do_something2 ()] runs then
   bind their result to [x] and [y]. It is the same as:

   {[
     let t1 = do_something1
     and t2 = do_something2 in
     bind t1 (fun x -> bind t2 (fun y -> code))
   ]}

   - exception catching:

   {[
     try%lwt
       <expr>
   ]},

   {[
     try%lwt
       <expr>
     with
       <branches>
   ]},

   {[
     try%lwt
       <expr>
     with [%finally] -> <expr>
   ]}

   and

   {[
     try%lwt
       <expr>
     with
       <branches>
       | [%finally] -> <expr>
   ]}

   For example:

   {[
     try%lwt
       f x
     with
       | Failure msg ->
           prerr_endline msg;
           return ()
   ]}

   is expanded to:

   {[
     catch (fun () -> f x)
       (function
         | Failure msg ->
             prerr_endline msg;
             return ()
         | exn ->
             Lwt.fail exn)
   ]}

   Note that the [exn -> Lwt.fail exn] branch is automatically added
   when needed.

   The construction [try%lwt <expr>] just catch regular exception
   into lwt exception. i.e. it is the same as [catch (fun _ -> <expr>) fail].

   - assertion:

   {[
     assert%lwt <expr>
   ]}

   - for loop:

   {[
     for%lwt i = <expr> to <expr> do
       <expr>
     done
   ]}

   and:

   {[
     for%lwt i = <expr> downto <expr> do
       <expr>
     done
   ]}

   - while loop:

   {[
     while%lwt <expr> do
       <expr>
     done
   ]}

   - pattern matching:

   {[
     match%lwt <expr> with
       | <patt_1> -> <expr_1>
           ...
       | <patt_n> -> <expr_n>
   ]}

   - exception raising:

   For all other expression, this construction
   {[
     [%lwt <expr>]
   ]}

   is expanded to:
   {[
     Lwt.catch (fun () -> <expr>) Lwt.fail
   ]}

   It allows to encode the old [raise_lwt <e>] as [[%lwt raise <e>]].

   {2 Debug}

   By default, the debug mode is enabled. This means that the [backtrace] version of the [bind], [finalize] and [catch] functions are used, which will enable proper backtraces for Lwt exceptions.

   The debug mode can be disabled with the option [-no-debug].

   {2 Sequence}

   It is also possible to sequence lwt operation with the [>>] operator:
   {[
     write stdio "Hello, " >> write stdio "world!"
   ]}

   By default, each operation must return [unit Lwt.t]. This constraint can be
   lifted with the option [-no-strict-sequence]. The operator can be disabled
   with the option [-no-sequence].


   {2 Logging}

   The syntax extension will replace expression of the form:

   {[
     Lwt_log.info_f ~section "x = %d" x
   ]}

   by

   {[
     if Lwt_log.Section.level section <= Lwt_log.Info then
       Lwt_log.info_f ~section "x = %d" x
     else
       return ()
   ]}

   Notes:

   - The application must be complete. For example: [Log.info "%d"]
   will make compilation fail.

   - Debug messages are removed if the option [-no-debug] is passed.

   - The log syntax extension can be disabled with the option [-no-log].




*)
