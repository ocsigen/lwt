
# Module `Lwt_engine.Versioned`

```ocaml
class libev_1 : object ... end
```
Old version of [`Lwt_engine.libev`](./Lwt_engine-class-libev.md). The current [`Lwt_engine.libev`](./Lwt_engine-class-libev.md) allows selecting the libev back end.

```ocaml
class libev_2 : ?backend:Ev_backend.t -> unit -> object ... end
```
Since Lwt 3\.0.0, this is just an alias for [`Lwt_engine.libev`](./Lwt_engine-class-libev.md).
