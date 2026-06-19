
# Module `Lwt_engine`

Lwt unix main loop engine


### Events

```ocaml
type event
```
Type of events. An event represent a callback registered to be called when some event occurs.

```ocaml
val stop_event : event -> unit
```
`stop_event event` stops the given event.

```ocaml
val fake_event : event
```
Event which does nothing when stopped.


### Event loop functions

```ocaml
val iter : bool -> unit
```
`iter block` performs one iteration of the main loop. If `block` is `true` the function must block until one event becomes available, otherwise it should just check for available events and return immediately.

```ocaml
val on_readable : Unix.file_descr -> (event -> unit) -> event
```
`on_readable fd f` calls `f` each time `fd` becomes readable.

```ocaml
val on_writable : Unix.file_descr -> (event -> unit) -> event
```
`on_readable fd f` calls `f` each time `fd` becomes writable.

```ocaml
val on_timer : float -> bool -> (event -> unit) -> event
```
`on_timer delay repeat f` calls `f` one time after `delay` seconds. If `repeat` is `true` then `f` is called each `delay` seconds, otherwise it is called only one time.

```ocaml
val readable_count : unit -> int
```
Returns the number of events waiting for a file descriptor to become readable.

```ocaml
val writable_count : unit -> int
```
Returns the number of events waiting for a file descriptor to become writable.

```ocaml
val timer_count : unit -> int
```
Returns the number of registered timers.

```ocaml
val fake_io : Unix.file_descr -> unit
```
Simulates activity on the given file descriptor.

```ocaml
val fork : unit -> unit
```
Called internally by Lwt\_unix.fork to make sure we don't get strange behaviour

```ocaml
val forwards_signal : int -> bool
```
`forwards_signal signum` is `true` if the engine will call [`Lwt_unix.handle_signal`](./Lwt_unix.md#val-handle_signal) when signal `signum` occurs. In this case, Lwt will not install its own signal handler.

Normally, this just returns `false`, but when Lwt is used in combination with other IO libraries, this allows sharing e.g. the SIGCHLD handler.


### Engines

An engine represents a set of functions used to register different kinds of callbacks for different kinds of events.

```ocaml
type engine_id = ..
```
```ocaml
val id : unit -> engine_id
```
```ocaml
class virtual abstract : object ... end
```
Abstract class for engines.

```ocaml
class type  t = object ... end
```
Type of engines.


### Predefined engines

```ocaml
type ev_loop
```
```ocaml
module Ev_backend : sig ... end
```
Type of libev loops.

```ocaml
type engine_id += 
  | Engine_id__libev of Ev_backend.t
```
```ocaml
class libev : ?backend:Ev_backend.t -> unit -> object ... end
```
Engine based on libev. If not compiled with libev support, the creation of the class will raise [`Lwt_sys.Not_available`](./Lwt_sys.md#exception-Not_available).

```ocaml
type engine_id += 
  | Engine_id__select
```
Engine based on `Unix.select`.

```ocaml
type engine_id += 
  | Engine_id__poll
```
```ocaml
class select : t
```
```ocaml
class virtual select_based : object ... end
```
Abstract class for engines based on a select-like function.

```ocaml
class virtual poll_based : object ... end
```
Abstract class for engines based on a poll-like function.


### The current engine

```ocaml
val get : unit -> t
```
`get ()` returns the engine currently in use.

```ocaml
val set : ?transfer:bool -> ?destroy:bool -> t -> unit
```
`set ?transfer ?destroy engine` replaces the current engine by the given one.

If `transfer` is `true` (the default) all events from the current engine are transferred to the new one.

If `destroy` is `true` (the default) then the current engine is destroyed before being replaced.

```ocaml
module Versioned : sig ... end
```