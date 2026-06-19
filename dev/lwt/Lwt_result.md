
# Module `Lwt_result`

Explicit error handling

since 2\.6.0
This module provides helpers for values of type `('a, 'b) result Lwt.t`. The module is experimental and may change in the future.

```ocaml
type (+'a, +'b) t = ('a, 'b) Stdlib.result Lwt.t
```
```ocaml
val return : 'a -> ('a, _) t
```
```ocaml
val fail : 'b -> (_, 'b) t
```
```ocaml
val lift : ('a, 'b) Stdlib.result -> ('a, 'b) t
```
```ocaml
val ok : 'a Lwt.t -> ('a, _) t
```
```ocaml
val error : 'b Lwt.t -> (_, 'b) t
```
since 5\.6.0
```ocaml
val catch : (unit -> 'a Lwt.t) -> ('a, exn) t
```
`catch x` behaves like `return y` if `x ()` evaluates to `y`, and like `fail e` if `x ()` raises `e`

```ocaml
val get_exn : ('a, exn) t -> 'a Lwt.t
```
`get_exn` is the opposite of [`catch`](./#val-catch): it unwraps the result type, returning the value in case of success, calls [`Lwt.fail`](./Lwt.md#val-fail) in case of error.

```ocaml
val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
```
```ocaml
val map_error : ('e1 -> 'e2) -> ('a, 'e1) t -> ('a, 'e2) t
```
since 5\.6.0
```ocaml
val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
```
```ocaml
val bind_error : ('a, 'e1) t -> ('e1 -> ('a, 'e2) t) -> ('a, 'e2) t
```
since 5\.6.0
```ocaml
val bind_lwt : ('a, 'e) t -> ('a -> 'b Lwt.t) -> ('b, 'e) t
```
```ocaml
val bind_lwt_error : ('a, 'e1) t -> ('e1 -> 'e2 Lwt.t) -> ('a, 'e2) t
```
since 5\.6.0
```ocaml
val bind_result : ('a, 'e) t -> ('a -> ('b, 'e) Stdlib.result) -> ('b, 'e) t
```
```ocaml
val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
```
`Lwt.both p_1 p_2` returns a promise that is pending until *both* promises `p_1` and `p_2` become *resolved*. If only `p_1` is `Error e`, the promise is resolved with `Error e`, If only `p_2` is `Error e`, the promise is resolved with `Error e`, If both `p_1` and `p_2` resolve with `Error _`, the promise is resolved with the error that occurred first.

```ocaml
val iter : ('a -> unit Lwt.t) -> ('a, 'e) t -> unit Lwt.t
```
`iter f r` is `f v` if `r` is a promise resolved with `Ok v`, and [`Lwt.return_unit`](./Lwt.md#val-return_unit) otherwise.

since Lwt 5.6.0
```ocaml
val iter_error : ('e -> unit Lwt.t) -> ('a, 'e) t -> unit Lwt.t
```
`iter_error f r` is `f v` if `r` is a promise resolved with `Error v`, and [`Lwt.return_unit`](./Lwt.md#val-return_unit) otherwise.

since Lwt 5.6.0
```ocaml
module Infix : sig ... end
```
```ocaml
module Let_syntax : sig ... end
```
```ocaml
module Syntax : sig ... end
```
```ocaml
val (>|=) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
```
```ocaml
val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
```

#### Deprecated

```ocaml
val map_err : ('e1 -> 'e2) -> ('a, 'e1) t -> ('a, 'e2) t
```
deprecated Alias to map\_error since 5.6.0.
```ocaml
val bind_lwt_err : ('a, 'e1) t -> ('e1 -> 'e2 Lwt.t) -> ('a, 'e2) t
```
deprecated Alias to bind\_lwt\_error since 5.6.0.