
# Module `Lwt_engine.Ev_backend`

```ocaml
type t
```
```ocaml
val default : t
```
```ocaml
val select : t
```
```ocaml
val poll : t
```
```ocaml
val epoll : t
```
```ocaml
val kqueue : t
```
```ocaml
val devpoll : t
```
```ocaml
val port : t
```
```ocaml
val equal : t -> t -> bool
```
```ocaml
val pp : Stdlib.Format.formatter -> t -> unit
```