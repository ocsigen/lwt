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

(** Ppx syntax extension for Lwt *)


(** {2 Ppx extensions}

    This Ppx extension adds various syntactic shortcut for lwt programming.
    It needs OCaml >= 4.02 and {{:https://github.com/alainfrisch/ppx_tools}ppx_tools}.

    To use it, simply use the ocamlfind package [lwt.ppx].

   This extension adds the following syntax:

   - lwt-binding:

   {[
let%lwt ch = get_char stdin in
code
   ]}

   is the same as [bind (get_char stdin) (fun ch -> code)].

   Moreover, it supports parallel binding:

   {[
let%lwt x = do_something1 ()
and y = do_something2 in
code
   ]}

   will run [do_something1 ()] and [do_something2 ()], then
   bind their results to [x] and [y]. It is the same as:

   {[
let t1 = do_something1
and t2 = do_something2 in
bind t1 (fun x -> bind t2 (fun y -> code))
   ]}

   - exception catching:

   {[
try%lwt
  <expr>
with
  <branches>
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

   - finalizer:

   {[
     (<expr>) [%finally <expr>]
   ]}

   You can use [[%lwt.finally ...]] instead of [[%finally ...]].


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

   Exception cases are also supported:

   {[
match%lwt <expr> with
  | exception <exn> -> <expr_1>
  | <patt_2> -> <expr_2>
      ...
  | <patt_n> -> <expr_n>
   ]}

   - conditional:

   {[
if%lwt <expr> then
  <expr_1>
else
  <expr_2>
   ]}

   and

   {[
     if%lwt <expr> then <expr_1>
   ]}

   - exception raising:

   For all other expression, the construct
   {[
     [%lwt <expr>]
   ]}

   is expanded to:
   {[
     Lwt.catch (fun () -> <expr>) Lwt.fail
   ]}

   It allows to encode the old [raise_lwt <e>] as [[%lwt raise <e>]], and offers a convenient way to interact with non-Lwt code.

   {2 Debug}

   By default, the debug mode is enabled. This means that the [backtrace] versions of the [bind], [finalize] and [catch] functions are used, enabling proper backtraces for the Lwt exceptions.

   The debug mode can be disabled with the option [-no-debug]:

   {v

$ ocamlfind ocamlc -package lwt.ppx \
    -ppxopt lwt.ppx,-no-debug -linkpkg -o foo foo.ml
 v}

   {2 Sequence}

   It is also possible to sequence Lwt operations with the [>>] operator:
   {[
     write stdout "Hello, " >> write stdout "world!"
   ]}

   By default, each operation must return [unit Lwt.t]. This constraint can be
   lifted with the option [-no-strict-sequence]. The operator can be disabled
   with the option [-no-sequence].

   If you are mixing `>>` and `;`, you need to use parentheses or `begin`/`end`
   to get the result you expect:

   {[
     write stdout "Hello, " >> (ignore (); write stdout "world!")
   ]}

   Note that unlike [>>=], [>>] is not an OCaml value. it is a piece of syntax
   added by the ppx rewriter - i.e., you cannot refer to [(>>)].

   {2 Logging}

   The logging syntax extension is enabled with [-log].
   It will replace expressions of the form:

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

*)
