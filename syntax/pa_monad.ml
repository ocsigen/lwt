(* name:          pa_monad.ml
 * synopsis:      Haskell-like "do" for monads
 * authors:       Jacques Carette and Oleg Kiselyov,
 *                based in part of work of Lydia E. Van Dijk
 * last revision: Thu Nov 13 09:27:46 UTC 2008
 * ocaml version: 3.11
 *
 * Copyright (C) 2006-2008  J. Carette, L. E. van Dijk, O. Kiselyov
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


(** {2 Syntax Extension to Support Monads}

This module extends OCaml's syntax by a Haskell-like "[do]"-notation
particularly suited for the work with monads.

By the nature of the translation process (at pre-processing time,
before compilation) it cannot be guaranteed that the result code
actually obeys the three fundamental laws for all monads:
+ [bind (return x) f]  is identical to  [f x]
+ [bind m return]      is identical to  [m]
+ [bind (bind m f) g]  is identical to  [bind m (fun x -> bind (f x) g)]

where [bind] and [return] are user defined functions.  Incidentally,
in Haskell, too, it is entirely the responsibility of the programmer
to make sure that [bind] and [return] are implemented for a particular
Monad do indeed obey the above laws.


{2 Conversion Rules}

{3 Grammar informally}
We support four different constructs to introduce monadic
expressions.
- [perform exp]
- [perform exp1; exp2]
- [perform x <-- exp1; exp2]
- [perform let x = foo in exp]

which is almost literally the grammar of the Haskell's
"[do]"-notation, with the differences that Haskell uses "[do]" and
"[<-]" where we use "[perform]" and "[<--]".

We support not only [let x = foo in ...] expressions but arbitrarily
complex [let]-expressions, including [let rec] and [let module].


{4 Extended Forms}
The actual bind function of the monad defaults to "[bind]" and the
match-failure function to "[failwith]".  The latter is only used for
refutable patterns; see below.  To select different functions, use the
extended forms of "[perform]".

{b Expression:} Use the given expression as "[bind]"-function and
apply the default match-failure function ([failwith]) where necessary.
{[
        perform with exp1 in exp2
        perform with exp1 in exp3; exp4
        perform with exp1 in x <-- exp2; exp3
        perform with exp in let x = foo in exp
]}
Use the first given expression ([exp1]) as "[bind]"-function and the
second ([exp2]) as match-failure function.
{[
        perform with exp1 and exp2 in exp3
        perform with exp1 and exp2 in exp3; exp4
        perform with exp1 and exp2 in x <-- exp3; exp4
        perform with exp1 and exp2 in let x = foo in exp1
]}

{b Module:} Use the function named "[bind]" from module "[Mod]".  In
addition use the module's "[failwith]"-function in refutable patterns.
{[
        perform with module Mod in exp2
        perform with module Mod in exp3; exp4
        perform with module Mod in x <-- exp2; exp3
        perform with module Mod in let x = foo in exp
]}


{4 Refutable Patterns}
An irrefutable pattern is either:
- A variable,
- The wildcard "[_]",
- The constructor "[()]",
- A tuple with irrefutable patterns,
- A record with irrefutable patterns, or
- An irrefutable pattern with a type constraint.

Any other pattern is refutable.

Why do we need this distinction?  Well, the expression
{[
        perform x <-- exp1; exp2
]}
expands to
{[
        bind exp2 (fun x -> exp1)
]}
where pattern match can never fail as "[x]" can take any value.  This
is an example of an irrefutable pattern.  No catch-all branch is
required here.  Compare this with
{[
        perform 1 <-- exp1; exp2
]}
which expands to
{[
        bind exp2 (fun 1 -> exp1 | _ -> failwith "pattern match")
]}
As the match can fail -- "[1]" being a refutable pattern in this
position -- we must add a second branch that catches the remaining
values.  The user is free to override the "[failwith]" function with
her own version.

Refer to the thread on the Haskell mailing list concerning the topic
of {{:http://www.haskell.org/pipermail/haskell/2006-January/017237.html}
refutable patterns} and an excerpt from an earlier
{{:http://www.cs.chalmers.se/~rjmh/Haskell/Messages/Decision.cgi?id=2}
discussion} on the same issue.


{3 Grammar formally}
Formally the grammar of [pa_monad] can be specified as follows.
{[
        "perform" ["with" <user-function-spec> "in"] <perform-body>

        <user-function-spec> ::=
                  EXPR ["and" EXPR]
                | "module" MODULE-NAME

        <perform-body> ::=
                  <LET-FORM> <perform-body>
                | EXPR
                | <binding> ";" <perform-body>
                | "rec" <binding> ["and" <binding> [...]] ";" <perform-body>

        <binding> ::= PATTERN "<--" EXPR
]}
where
- [EXPR] is an OCaml expression {i expr} as defined in
{{:http://caml.inria.fr/pub/docs/manual-ocaml/expr.html} Section 6.7, "Expressions"},
of the OCaml manual,
- [MODULE-NAME] a {i module-name}
({{:http://caml.inria.fr/pub/docs/manual-ocaml/manual011.html} Sec. 6.3, "Names"}),
- [LET-FORM] is any of the [let], [let rec], or [let module] {i let-forms}
({{:http://caml.inria.fr/pub/docs/manual-ocaml/expr.html} Sec. 6.7, "Expressions"}), and
- [PATTERN] a {i pattern}
({{:http://caml.inria.fr/pub/docs/manual-ocaml/patterns.html} Sec. 6.6, "Patterns"}).

The "[rec]" keyword allows for a recursive binding in
{[
        "rec" PATTERN "<--" EXPR
        "and" PATTERN "<--" EXPR
        ...
        "and" PATTERN "<--" EXPR ";"
]}
The syntax extension groups all bindings in a "[rec]"-"[and]", but
it does not group consecutive "[rec]"-bindings.  This grouping is
sometimes called segmentation.

{b Example:} Define a recursive group of bindings consisting of three
patterns ([PATTERN1]-[PATTERN3]) and expressions ([EXPR1]-[EXPR3]), a
non-recursive binding [PATTERN4]/[EXPR4], and finally a single
recursive binding [PATTERN5]/[EXPR5]:
{[
        "rec" PATTERN1 "<--" EXPR1
        "and" PATTERN2 "<--" EXPR2
        "and" PATTERN3 "<--" EXPR3 ";"
              PATTERN4 "<--" EXPR4 ";"
        "rec" PATTERN5 "<--" EXPR5 ";"
]}
Please consult
{{:http://caml.inria.fr/pub/docs/manual-ocaml/manual021.html} Section
7.3, "Recursive definitions of values"} of the Manual for valid
recursive definitions of values, as the only allowed [PATTERN] in the
recursive case is a [NAME].  Similarly stringent restrictions apply to
[EXPR].

The theoretical aspects of recursive monadic bindings can be found in:
Levent Erkök and John Launchbury, "A Recursive do for Haskell".

{b Formal Types of [bind] and [failwith]}

For any ['a monad] the expansion uses the functions "[bind]" and
"[failwith]" with the signatures
{[
        val bind: 'a monad -> ('a -> 'b monad) -> 'b monad
        val failwith: string -> 'a monad
]}
unless overridden by the user.  Analogously, the signatures of modules
used in the "[with module]"-form must enclose
{[
        sig
          type 'a monad
          val bind: 'a monad -> ('a -> 'b monad) -> 'b monad
          val failwith: string -> 'a monad
        end
]}
Note that although a proper monad requires a [return] function, the
translation itself does not need it.


{3 Semantics (as re-writing into the core language)}
In this section, we abbreviate irrefutable patterns with [ipat] and
refutable patterns with [rpat].
{[
        perform exp1                 ===>  exp1
        perform ipat <-- exp; rest   ===>  bind exp (fun ipat -> perform rest)
        perform rpat <-- exp; rest   ===>  bind exp (fun rpat -> perform rest
                                                         | _ -> failwith "pattern match")
        perform let ... in rest      ===>  let ... in perform rest
        perform exp; rest            ===>  bind exp (fun _ -> perform rest)

        perform with bexp in body
                ===> perform body
                        where bind is substituted with bexp

        perform with bexp and fexp in body
                ===> perform body
                        where bind is substituted with bexp and
                              failwith is substituted with fexp

        perform with module Mod in body
                ===> perform body
                        where bind is substituted with Mod.bind and
                              failwith with Mod.failwith
]}


{4 Implementation Notes And Design Decisions}
It is be possible to use "[<-]" instead of "[<--]".  In that case, the
similarity to the "[do]" notation of Haskell will be complete.
However, if the program has [_ <- exp] outside of [perform], this will
be accepted by the parser (and create an incomprehensible error later
on).  It is better to use a dedicated symbol "[<--]", so if the user
abuses it, the error should be clear right away.

The major difficulty with the [perform] notation is that it cannot
truly be parsed by an LR-grammar.  Indeed, to figure out if we should
start parsing <perform-body> as an expression or a pattern, we have to
parse it as a pattern and check for the "[<--]" delimiter.  If it is
not there, we should {e backtrack} and parse it again as an
expression.  Furthermore, [a <-- b] (or [a <- b]) can also be parsed
as an expression.  However, some patterns, for example [_ <-- exp],
cannot be parsed as an expression.

It is possible (via some kind of flag) to avoid parsing [_ <-- exp]
outside of [perform].  But this becomes quite complex and unreliable.
To record a particular expression [patt <-- exp] in the AST, we use
the node
{[
    <:expr< let [(patt, exp)] in $lid:"<--"$ >>
]}
If the construction [_ <-- exp] is used by mistake, we get an error
message about an unbound identifier "[<--]", which is our intention.


{2 Known Issues}

- Sum types are assumed to have more than one constructor, thus always
  yield refutable patterns.  This is, if you define
  {[
        type t = T
  ]}
  and later use
  {[
        perform T <-- T; ...
  ]}
  you get "Warning U: this match case is unused." which is not deserved.

- Aliases in patterns are not supported yet.  Code like
  {[
        perform
          ((x, y, z) as tuple) <-- 1, 2, 3;
          ...
  ]}
  blows the extension out of the water.  As a matter of fact, it is
  not clear that this should be supported at all: patterns with
  aliases are not "simple patterns" (see {i pa_o.ml}).  For example,
  patterns with aliases cannot be used in [fun pattern -> ...].  Thus,
  at present monadic bindings should include only those patterns that
  are permissible in [fun].  And perhaps this is the optimal decision.

- The recursive form "[rec ... <-- ...]" is not implemented completely.
  It lacks support for a (user-specified) fix-point function.  See
  for example Erkök and Launchbury's "A Recursive do for Haskell".
 *)


open Camlp4.PreCast
open Syntax


(** [failure_text]

    This is the text that accompanies a match failure of a refutable
    pattern. *)
let failure_text = "pattern match"


(** [default_bind_expr _loc]

    This is the default expression for the "bind" function. *)
let default_bind_expr (_loc: Ast.Loc.t): Ast.expr =
  <:expr< bind >>


(** [default_failure_fun_expr _loc]

    This is the expression for the default "failwith" function. *)
let default_failure_fun_expr (_loc: Ast.Loc.t): Ast.expr =
  <:expr< failwith >>


(** [default_failure_expr _loc]

    This is the expression for the default "failwith" function
    ({!Pa_monad.default_failure_fun_expr}) after the
    {!Pa_monad.failure_text} has been applied. *)
let default_failure_expr (_loc: Ast.Loc.t): Ast.expr =
  let fun_expr = default_failure_fun_expr _loc
  and text_expr = <:expr< $str:failure_text$ >> in
    <:expr< $fun_expr$ $text_expr$ >>


(** [exp_to_patt _loc an_expression]

    Convert [an_expression] to a (simple) pattern, if we "accidentally" parse
    a pattern as an expression. *)
(* The code is based on [pattern_eq_expression] in {i pa_fstream.ml}. *)
let rec exp_to_patt (_loc: Ast.Loc.t) (an_expression: Ast.expr): Ast.patt =
  match an_expression with
      <:expr< $int:s$ >> -> <:patt< $int:s$ >> (* integer constant *)
    | <:expr< $chr:c$ >> -> <:patt< $chr:c$ >> (* character constant *)
    | <:expr< $str:s$ >> -> <:patt< $str:s$ >> (* string constant *)
    | <:expr< $lid:b$ >> -> <:patt< $lid:b$ >> (* local variable *)
    | <:expr< $uid:b$ >> -> <:patt< $uid:b$ >> (* variable of other module *)
    | <:expr< $e1$ $e2$ >> ->                  (* function application *)
      let p1 = exp_to_patt _loc e1
      and p2 = exp_to_patt _loc e2 in
        <:patt< $p1$ $p2$ >>
    | <:expr< ($tup:e$) >> ->                  (* tuple *)
      let p = exp_to_patt _loc e in
        <:patt< ($tup:p$) >>
    | <:expr< $e1$, $e2$ >> ->
      let p1 = exp_to_patt _loc e1
      and p2 = exp_to_patt _loc e2 in
        <:patt< $p1$, $p2$ >>
    | <:expr< { $rec_binding:r$ } >> ->        (* record *)
      let p = recbinding_to_patt _loc r in
        <:patt< { $p$ } >>
    | <:expr< ($e$ : $t$) >> ->                (* type restriction *)
      let p = exp_to_patt _loc e in
        <:patt< ($p$ : $t$) >>
    | _ ->
      Loc.raise _loc
        (Stream.Error "exp_to_patt: this expression is not yet supported")
(** [recbinding_to_pattrec _loc an_exp_record]

    Convert [an_exp_record] to a pattern matching a record. *)
and recbinding_to_patt (_loc: Ast.Loc.t) (an_exp_record: Ast.rec_binding): Ast.patt =
  match an_exp_record with
      <:rec_binding< >> -> <:patt< >>
    | <:rec_binding< $i$ = $e$ >> ->
      let p = exp_to_patt _loc e in
        <:patt< $i$ = $p$ >>
    | <:rec_binding< $b1$ ; $b2$ >> ->
        let p1 = recbinding_to_patt _loc b1
        and p2 = recbinding_to_patt _loc b2 in
          <:patt< $p1$; $p2$ >>
    | <:rec_binding< $anti:_$ >> ->
      Loc.raise _loc
        (Stream.Error "recbinding_to_patt: antiquotations are not yet supported")


(** [patt_to_exp _loc a_pattern]

    Convert [a_pattern] to an expression, if we must reuse it in a
    different semantic position. *)
let rec patt_to_exp (_loc: Ast.Loc.t) (a_pattern: Ast.patt): Ast.expr =
  match a_pattern with
      <:patt< $int:s$ >> -> <:expr< $int:s$ >> (* integer constant *)
    | <:patt< $chr:c$ >> -> <:expr< $chr:c$ >> (* character constant *)
    | <:patt< $str:s$ >> -> <:expr< $str:s$ >> (* string constant *)
    | <:patt< $lid:b$ >> -> <:expr< $lid:b$ >> (* local variable *)
    | <:patt< $uid:b$ >> -> <:expr< $uid:b$ >> (* variable of other module *)
    | <:patt< $e1$ $e2$ >> ->                  (* function application *)
      let p1 = patt_to_exp _loc e1
      and p2 = patt_to_exp _loc e2 in
        <:expr< $p1$ $p2$ >>
    | <:patt< ($tup:p$) >> ->                  (* tuple *)
      let e = patt_to_exp _loc p in
        <:expr< ($tup:e$) >>
    | <:patt< $p1$, $p2$ >> ->
      let e1 = patt_to_exp _loc p1
      and e2 = patt_to_exp _loc p2 in
        <:expr< $e1$, $e2$ >>
    | <:patt< { $r$ } >> ->
      <:expr< { $rec_binding:patt_to_recbinding _loc r$ } >>
    | <:patt< ($e$ : $t$) >> ->                (* type restriction *)
      let p = patt_to_exp _loc e in
        <:expr< ($p$ : $t$) >>
    | _ ->
      Loc.raise _loc
        (Stream.Error "patt_to_exp: this pattern is not yet supported")
(** [patt_to_recbinding _loc a_pattern]

    Convert [a_pattern] to a recursive binding. *)
and patt_to_recbinding (_loc: Ast.Loc.t) (a_pattern: Ast.patt): Ast.rec_binding =
  match a_pattern with
      <:patt< >> -> <:rec_binding< >>
    | <:patt< $i$ = $p$ >> ->
      let p = patt_to_exp _loc p in
        <:rec_binding< $i$ = $p$ >>
    | <:patt< $p1$ ; $p2$ >> ->
        let b1 = patt_to_recbinding _loc p1
        and b2 = patt_to_recbinding _loc p2 in
          <:rec_binding< $b1$; $b2$ >>
    | <:patt< $anti:_$ >> ->
      Loc.raise _loc
        (Stream.Error "patt_to_recbinding: antiquotation are not yet supported")
    | _ ->
      Loc.raise _loc
        (Stream.Error "patt_to_recbinding: never reached")


(** [is_irrefutable_pattern a_pattern]

    Answer whether [a_pattern] is irrefutable.

    Implementation Note: In OCaml 3.10.0 the function
    [Ast.is_irrefut_patt] is buggy.  Thus, we must use our own
    implementation. *)
let rec is_irrefutable_pattern (a_pattern: Ast.patt): bool =
  match a_pattern with
      <:patt< () >> -> true             (* unit *)
    | <:patt< ( $p$ : $_$ ) >> ->       (* type constraint *)
      is_irrefutable_pattern p
    | <:patt< ( $p1$ as $_p2$ ) >> ->   (* alias *)
      is_irrefutable_pattern p1
    | <:patt< { $r$ } >> ->             (* record *)
      is_irrefutable_pattern r
    | <:patt< $_$ = $p$ >> ->           (* field in a record *)
      is_irrefutable_pattern p
    | <:patt< $r1$; $r2$ >> ->          (* sum of fields *)
        is_irrefutable_pattern r1 && is_irrefutable_pattern r2
    | <:patt< $t1$, $t2$ >> ->          (* sum in a tuple *)
      is_irrefutable_pattern t1 && is_irrefutable_pattern t2
    | <:patt< ($tup:t$) >> ->           (* tuple *)
      is_irrefutable_pattern t
    | <:patt< $lid:_$ >> -> true        (* variable *)
    | <:patt< _ >> -> true              (* wildcard *)
    | _ -> false


(** [tuplify_expr _loc an_expression_list]

    Convert [an_expression_list] to a tuple of expressions. *)
let tuplify_expr (_loc: Ast.Loc.t) (an_expression_list: Ast.expr list): Ast.expr =
  match an_expression_list with
      [] -> Loc.raise _loc (Stream.Error "tuplify_expr: empty expression list")
    | x :: [] -> x
    | _ -> <:expr< ($tup:Ast.exCom_of_list an_expression_list$) >>


(** [tuplify_patt _loc a_pattern_list]

    Convert [a_pattern_list] to a tuple of patterns. *)
let tuplify_patt (_loc: Ast.Loc.t) (a_pattern_list: Ast.patt list): Ast.patt =
  match a_pattern_list with
      [] -> Loc.raise _loc (Stream.Error "tuplify_patt: empty pattern list")
    | x :: [] -> x
    | _ -> <:patt< ($tup:Ast.paCom_of_list a_pattern_list$) >>


(** [convert _loc a_perform_body a_bind_function a_fail_function]

    Convert all expressions of [a_perform_body] inside [perform] into
    core OCaml.  Use [a_bind_function] as the monad's "bind"-function,
    and [a_fail_function] as the "failure"-function. *)
let convert
    (_loc: Ast.Loc.t)
    (a_perform_body: Ast.expr)
    (a_bind_function: Ast.expr)
    (a_fail_function: Ast.expr): Ast.expr =
  let rec loop _loc a_perform_body =
    match a_perform_body with
        <:expr< let $rec:_$ $_$ in $lid:"<--"$ >> ->
          Loc.raise _loc
            (Stream.Error "convert: monadic binding cannot be last in a \"perform\" body")
      | <:expr< let $rec:r$ $binding:bs$ in $body$ >> ->
        let body' = loop _loc body in
          <:expr< let $rec:r$ $binding:bs$ in $body'$ >>
      | <:expr< let module $m$ = $mb$ in $body$ >> ->
        let body' = loop _loc body in
          <:expr< let module $m$ = $mb$ in $body'$ >>
      | <:expr< do { $e$ } >> ->
         let b1, b2, bs =
           match Ast.list_of_expr e [] with
               b1 :: b2 :: bs -> b1, b2, bs
             | _ -> assert false in
         let do_rest () =
           loop _loc
             (match bs with
                 [] -> b2
               | _  -> <:expr< do { $list:(b2 :: bs)$ } >>)
         and do_merge a_body =
           loop _loc <:expr< do { $list:(a_body :: b2 :: bs)$ } >> in
             begin
               match b1 with
                   (* monadic binding *)
                   <:expr< let $p$ = $e$ in $lid:"<--"$ >> ->
                     if is_irrefutable_pattern p then
                       <:expr< $a_bind_function$ $e$ (fun $p$ -> $do_rest ()$) >>
                     else
                       <:expr< $a_bind_function$
                               $e$
                               (fun [$p$ -> $do_rest ()$
                                     | _ -> $a_fail_function$ ]) >>
                   (* recursive monadic binding *)
                 | <:expr< let rec $binding:b$ in $lid:"<--"$ >> ->
                   let pattern_list = List.map fst (Ast.pel_of_binding b) in
                   let patterns = tuplify_patt _loc pattern_list
                   and patt_as_exp =
                     tuplify_expr
                       _loc
                       (List.map (fun x -> patt_to_exp _loc x) pattern_list)
                   in
                     List.iter
                       (fun p ->
                         if not (is_irrefutable_pattern p) then
                           Loc.raise _loc
                             (Stream.Error
                                 ("convert: refutable patterns and " ^
                                     "recursive bindings do not go together")))
                       pattern_list;
                     <:expr< let rec $binding:b$ in
                               $a_bind_function$
                                 $patt_as_exp$
                                 (fun $patterns$ -> $do_rest ()$) >>
                 | (* map through the regular let *)
                   <:expr< let $rec:r$ $binding:bs$ in $body$ >> ->
                   <:expr< let $rec:r$ $binding:bs$ in $do_merge body$ >>
                 | <:expr< let module $m$ = $mb$ in $body$ >> ->
                   <:expr< let module $m$ = $mb$ in $do_merge body$ >>
                 | _ -> <:expr< $a_bind_function$ $b1$ (fun _ -> $do_rest ()$) >>
             end
      | any_body -> any_body
  in loop _loc a_perform_body


(** [qualify _loc a_module_ident a_function_expression]

    Append [a_function_expression] to the module name given in
    [a_module_ident], this is, qualify [a_function_expression] by
    [a_module_ident].  Fail if [a_module_ident] is not a valid
    module name. *)
let qualify
    (_loc: Ast.Loc.t)
    (a_module_ident: Ast.ident)
    (a_function_expression: Ast.expr): Ast.expr =
  let mod_expr = <:expr< $id:a_module_ident$ >> in
    <:expr< $mod_expr$ . $a_function_expression$ >>


(* Here we have to do the same nasty trick that Camlp4 uses and even
 * mentions in its documentation (viz. 'horrible hack' in pa_o.ml).  We
 * see if we can expect [patt <--] to succeed.  Here [patt] is a simple
 * pattern and it definitely does not parse as an expression.
 * Rather than resorting to unlimited lookahead and emulating the
 * Pcaml.patt LEVEL simple grammar, we do it the other way around: We
 * make sure that a pattern can always be parsed as an expression and
 * declare "[_]" a valid identifier!  If you attempt to use it,
 * you will get an undefined identifier anyway, so it is safe. *)

EXTEND Gram
    GLOBAL: expr;

    expr: LEVEL "top"
    [
      [ "perform"; "with"; "module"; monad_module = uid; "in";
        perform_body = expr LEVEL ";" ->
          let qualified_fail_expr =
            qualify _loc monad_module (default_failure_fun_expr _loc) in
            convert _loc
              perform_body
              (qualify _loc monad_module (default_bind_expr _loc))
              <:expr< $qualified_fail_expr$ $str:failure_text$ >> ]
    |
      [ "perform"; "with"; bind_fun = expr;
                           fail_fun = OPT opt_failure_expr; "in";
        perform_body = expr LEVEL ";" ->
          convert _loc
            perform_body
            bind_fun
            (match fail_fun with
                 None -> default_failure_expr _loc
               | Some f -> <:expr< $f$ $str:failure_text$ >>) ]
    |
      [ "perform";
        perform_body = expr LEVEL ";" ->
          convert _loc
            perform_body
            (default_bind_expr _loc)
            (default_failure_expr _loc) ]
    ] ;

    uid:
    [
     [i = LIST1 a_UIDENT SEP "." ->
       let rec uid_to_ident = function
           [a]-> <:ident< $uid:a$ >>
         | a :: b -> <:ident< $uid:a$.$uid_to_ident b$ >>
         | [] -> assert false
       in
         uid_to_ident i]
    ];

    opt_failure_expr:
    [
      [ "and"; fail_fun = expr -> fail_fun ]
    ] ;

    recursive_monadic_binding:
    [
      [ e1 = expr LEVEL "simple"; "<--"; e2 = expr LEVEL "top" ->
         <:binding< $exp_to_patt _loc e1$ = $e2$ >>
      ]
    ] ;

    expr: BEFORE "apply"
    [ NONA
      [ "rec"; binding_list = LIST1 recursive_monadic_binding SEP "and" ->
         let bind = Ast.biAnd_of_list binding_list in
           <:expr< let rec $binding:bind$ in $lid:"<--"$ >> ]
    |
      [ e1 = SELF; "<--"; e2 = expr LEVEL "top" ->
        let p1 = exp_to_patt _loc e1 in
          <:expr< let $p1$ =  $exp:e2$ in $lid:"<--"$ >> ]
    ] ;

    (* The difference between the expression and patterns is just [_].
     * So, we make [_] identifier. *)
    expr: LEVEL "simple"
    [
      [ "_" -> <:expr< $lid:"_"$ >> ]
    ] ;

END;
