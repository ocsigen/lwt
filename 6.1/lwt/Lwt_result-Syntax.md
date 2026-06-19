
# Module `Lwt_result.Syntax`


#### Let syntax


## Monadic syntax

```ocaml
val (let*) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
```
Syntax for [`bind`](./Lwt_result.md#val-bind).

```ocaml
val (and*) : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
```
Syntax for [`both`](./Lwt_result.md#val-both).


## Applicative syntax

```ocaml
val (let+) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
```
Syntax for [`map`](./Lwt_result.md#val-map).

```ocaml
val (and+) : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
```
Syntax for [`both`](./Lwt_result.md#val-both).
