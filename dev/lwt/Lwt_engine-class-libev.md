
# Class `Lwt_engine.libev`

Engine based on libev. If not compiled with libev support, the creation of the class will raise [`Lwt_sys.Not_available`](./Lwt_sys.md#exception-Not_available).

```ocaml
inherit t
```
```ocaml
method backend : Ev_backend.t
```
The backend picked by libev.

```ocaml
val loop : ev_loop
```
The libev loop used for this engine.

```ocaml
method loop : ev_loop
```
Returns `loop`.
