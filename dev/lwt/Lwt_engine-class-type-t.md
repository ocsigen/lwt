
# Class type `Lwt_engine.t`

Type of engines.

```ocaml
inherit abstract
```
```ocaml
method id : engine_id
```
```ocaml
method iter : bool -> unit
```
```ocaml
method private cleanup : unit
```
```ocaml
method private register_readable : Unix.file_descr ->
  (unit -> unit) ->
  unit Stdlib.Lazy.t
```
```ocaml
method private register_writable : Unix.file_descr ->
  (unit -> unit) ->
  unit Stdlib.Lazy.t
```
```ocaml
method private register_timer : float ->
  bool ->
  (unit -> unit) ->
  unit Stdlib.Lazy.t
```