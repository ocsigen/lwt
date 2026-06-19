
# Module `Lwt.Syntax`


#### Let syntax


## Monadic syntax

```ocaml
val (let*) : 'a t -> ('a -> 'b t) -> 'b t
```
Syntax for [`bind`](./Lwt.md#val-bind).

```ocaml
val (and*) : 'a t -> 'b t -> ('a * 'b) t
```
Syntax for [`both`](./Lwt.md#val-both).


## Applicative syntax

```ocaml
val (let+) : 'a t -> ('a -> 'b) -> 'b t
```
Syntax for [`map`](./Lwt.md#val-map).

```ocaml
val (and+) : 'a t -> 'b t -> ('a * 'b) t
```
Syntax for [`both`](./Lwt.md#val-both).
