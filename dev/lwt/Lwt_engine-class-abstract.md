
# Class `Lwt_engine.abstract`

Abstract class for engines.

```ocaml
method virtual id : engine_id
```
identifies the engine

```ocaml
method destroy : unit
```
Destroy the engine, remove all its events and free its associated resources.

```ocaml
method transfer : abstract -> unit
```
`transfer engine` moves all events from the current engine to `engine`. Note that timers are reset in the destination engine, i.e. if a timer with a delay of 2 seconds was registered 1 second ago it will occur in 2 seconds in the destination engine.


### Event loop methods

```ocaml
method virtual iter : bool -> unit
```
```ocaml
method fork : unit
```
```ocaml
method on_readable : Unix.file_descr -> (event -> unit) -> event
```
```ocaml
method on_writable : Unix.file_descr -> (event -> unit) -> event
```
```ocaml
method on_timer : float -> bool -> (event -> unit) -> event
```
```ocaml
method fake_io : Unix.file_descr -> unit
```
```ocaml
method readable_count : int
```
```ocaml
method writable_count : int
```
```ocaml
method timer_count : int
```
```ocaml
method forwards_signal : int -> bool
```

### Backend methods

Notes:

- the callback passed to register methods is of type `unit -> unit` and not `event -> unit`
- register methods return a lazy value which unregisters the event when forced
```ocaml
method private virtual cleanup : unit
```
Cleanup resources associated with the engine.

```ocaml
method private virtual register_readable : Unix.file_descr ->
  (unit -> unit) ->
  unit Stdlib.Lazy.t
```
```ocaml
method private virtual register_writable : Unix.file_descr ->
  (unit -> unit) ->
  unit Stdlib.Lazy.t
```
```ocaml
method private virtual register_timer : float ->
  bool ->
  (unit -> unit) ->
  unit Stdlib.Lazy.t
```