
# Class `Versioned.libev_1`

Old version of [`Lwt_engine.libev`](./Lwt_engine-class-libev.md). The current [`Lwt_engine.libev`](./Lwt_engine-class-libev.md) allows selecting the libev back end.

deprecated Use Lwt\_engine.libev.
since 2\.7.0
```ocaml
inherit t
```
```ocaml
val loop : ev_loop
```
```ocaml
method backend : Ev_backend.t
```
```ocaml
method loop : ev_loop
```