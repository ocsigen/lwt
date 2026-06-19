
# Module `Lwt_ppx.Ppx_lwt`

Ppx syntax extension for Lwt


### Ppx extensions

This Ppx extension adds various syntactic shortcut for lwt programming. It needs [ppx\_tools](https://github.com/ocaml-ppx/ppx_tools).

To use it, simply use the ocamlfind package `lwt_ppx`.

This extension adds the following syntax:


#### Lwt binding

```ocaml
let%lwt ch = get_char stdin in
code
```
is the same as `bind (get_char stdin) (fun ch -> code)`.

Moreover, it supports parallel binding:

```ocaml
let%lwt x = do_something1 ()
and y = do_something2 in
code
```
will run `do_something1 ()` and `do_something2 ()`, then bind their results to `x` and `y`. It is the same as:

```ocaml
let t1 = do_something1
and t2 = do_something2 in
bind t1 (fun x -> bind t2 (fun y -> code))
```
Due to a [bug](https://github.com/ocaml/ocaml/issues/7758) in the OCaml parser, if you'd like to put a type constraint on the variable, please write

```ocaml
let (foo : int) = do_something in
code
```
Not using parentheses will confuse the OCaml parser.


#### Sequencing promises

```ocaml
<expr1>;%lwt
<expr2>
```
For example:

```ocaml
Lwt_io.printl "Hello,";%lwt
Lwt_io.printl "world!"
```
is expanded to:

```ocaml
bind (Lwt_io.printl "Hello,") (fun () ->
      Lwt_io.printl "world!")
```

#### Exception handling

```ocaml
try%lwt
  <expr>
with
  <branches>
```
For example:

```ocaml
try%lwt
  f x
with
  | Failure msg ->
      prerr_endline msg;
      return ()
```
is expanded to:

```ocaml
catch (fun () -> f x)
  (function
    | Failure msg ->
        prerr_endline msg;
        return ()
    | exn ->
        Lwt.reraise exn)
```
Note that the `exn -> Lwt.reraise exn` branch is automatically added when needed.


#### Finalizer

```ocaml
  (<expr>) [%finally <expr>]
```
You can use `[%lwt.finally ...]` instead of `[%finally ...]`.


#### Assertion

```ocaml
  assert%lwt <expr>
```

#### For loop

```ocaml
for%lwt i = <expr> to <expr> do
  <expr>
done
```
and:

```ocaml
for%lwt i = <expr> downto <expr> do
  <expr>
done
```

#### While loop

```ocaml
while%lwt <expr> do
  <expr>
done
```

#### Pattern matching

```ocaml
match%lwt <expr> with
  | <patt_1> -> <expr_1>
      ...
  | <patt_n> -> <expr_n>
```
Exception cases are also supported:

```ocaml
match%lwt <expr> with
  | exception <exn> -> <expr_1>
  | <patt_2> -> <expr_2>
      ...
  | <patt_n> -> <expr_n>
```

#### Conditional

```ocaml
if%lwt <expr> then
  <expr_1>
else
  <expr_2>
```
and

```ocaml
  if%lwt <expr> then <expr_1>
```
```ocaml
class mapper : Ppxlib.Ast_traverse.map
```