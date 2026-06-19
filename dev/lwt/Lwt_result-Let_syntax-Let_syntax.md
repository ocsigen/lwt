
# Module `Let_syntax.Let_syntax`

```ocaml
val return : 'a -> ('a, _) t
```
See [`Lwt_result.return`](./Lwt_result.md#val-return).

```ocaml
val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
```
See [`Lwt_result.map`](./Lwt_result.md#val-map).

```ocaml
val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
```
See [`Lwt_result.bind`](./Lwt_result.md#val-bind).

```ocaml
val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
```
See [`Lwt_result.both`](./Lwt_result.md#val-both).

```ocaml
module Open_on_rhs : sig ... end
```