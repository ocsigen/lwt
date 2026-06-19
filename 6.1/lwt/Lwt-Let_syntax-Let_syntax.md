
# Module `Let_syntax.Let_syntax`

```ocaml
val return : 'a -> 'a t
```
See [`Lwt.return`](./Lwt.md#val-return).

```ocaml
val map : 'a t -> f:('a -> 'b) -> 'b t
```
See [`Lwt.map`](./Lwt.md#val-map).

```ocaml
val bind : 'a t -> f:('a -> 'b t) -> 'b t
```
See [`Lwt.bind`](./Lwt.md#val-bind).

```ocaml
val both : 'a t -> 'b t -> ('a * 'b) t
```
See [`Lwt.both`](./Lwt.md#val-both).

```ocaml
module Open_on_rhs : sig ... end
```