
# Module `Lwt.Infix`

This module provides several infix operators for making programming with Lwt more convenient.

To use it, open `Lwt.Infix`.

Of the operators declared in this module, only `>|=` is recommended for new code. The only other commonly-used operator is `>>=`.

```ocaml
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
```
`p >>= f` is the same as [`Lwt.bind`](./Lwt.md#val-bind)` p f`. It requires `Lwt.Infix` to be opened in scope:

```ocaml
open Lwt.Infix

let () =
  Lwt_main.run
    (Lwt_io.(read_line stdin) >>= Lwt_io.printl)

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
```
It is recommended to use the PPX `let%lwt` syntax instead. This operator is the next-best choice. It is frequently found while reading existing Lwt code.

```ocaml
val (>|=) : 'a t -> ('a -> 'b) -> 'b t
```
`p >|= f` is the same as [`Lwt.map`](./Lwt.md#val-map)` f p`. It requires `Lwt.Infix` to be opened in scope.

```ocaml
open Lwt.Infix

let () =
  Lwt_main.run
    (Lwt_io.(read_line stdin) >|= ignore)

(* ocamlfind opt -linkpkg -thread -package lwt.unix code.ml && ./a.out *)
```
```ocaml
val (<&>) : unit t -> unit t -> unit t
```
`p1 <&> p2` is the same as [`Lwt.join`](./Lwt.md#val-join)` [p1; p2]`. It requires `Lwt.Infix` to be opened in scope.

Unlike with [`Lwt.bind`](./Lwt.md#val-bind) and [`Lwt.map`](./Lwt.md#val-map), there are no problems with explicit [`Lwt.join`](./Lwt.md#val-join) syntax, so using this operator is not recommended.

```ocaml
val (<?>) : 'a t -> 'a t -> 'a t
```
`p1 <?> p2` is the same as [`Lwt.choose`](./Lwt.md#val-choose)` [p1; p2]`. It requires `Lwt.Infix` to be opened in scope.

Unlike with [`Lwt.bind`](./Lwt.md#val-bind) and [`Lwt.map`](./Lwt.md#val-map), there are no problems with explicit [`Lwt.choose`](./Lwt.md#val-choose) syntax, so using this operator is not recommended.

Furthermore, most users actually need [`Lwt.pick`](./Lwt.md#val-pick) instead of [`Lwt.choose`](./Lwt.md#val-choose).

```ocaml
val (=<<) : ('a -> 'b t) -> 'a t -> 'b t
```
`f =<< p` is the same as [`Lwt.bind`](./Lwt.md#val-bind)` p f`. It requires `Lwt.Infix` to be opened in scope.

This operator is obscure and its use is discouraged. It is the same as `p >>= f`.

```ocaml
val (=|<) : ('a -> 'b) -> 'a t -> 'b t
```
`f =|< p` is the same as [`Lwt.map`](./Lwt.md#val-map)` f p`. It requires `Lwt.Infix` to be opened in scope.

This operator is obscure and its use is discouraged. It is the same as `p >|= f`.

```ocaml
module Let_syntax : sig ... end
```
This module provides support for [ppx\_let](https://github.com/janestreet/ppx_let).
