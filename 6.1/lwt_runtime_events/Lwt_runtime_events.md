
# Module `Lwt_runtime_events`

This is the runtime-events library for lwt. It contains lwt-specific definition of events (so that programs can be written to consume the events lwt produces) and the function to emit them (so that lwt can emit them). This library is not intended for emitting events by hand.

```ocaml
type Runtime_events.User.tag += 
  | Paused_count
```
Counter event indicating the number of paused promises, i.e., the number of calls to `Lwt.pause` which have not yet been resolved.

```ocaml
val paused_count : int Runtime_events.User.t
```
```ocaml
val emit_paused_count : int -> unit
```
```ocaml
type Runtime_events.User.tag += 
  | Scheduler_call
```
Span event indicating that `Lwt_main.run` has been called and hasn't returned yet, i.e., when the Lwt scheduler is running.

```ocaml
val sch_call : Runtime_events.Type.span Runtime_events.User.t
```
```ocaml
val emit_sch_call_begin : unit -> unit
```
```ocaml
val emit_sch_call_end : unit -> unit
```
```ocaml
type Runtime_events.User.tag += 
  | Scheduler_lap
```
Punctual event indicating that the scheduler is performing one loop.

```ocaml
val sch_lap : unit Runtime_events.User.t
```
```ocaml
val emit_sch_lap : unit -> unit
```
```ocaml
type Runtime_events.User.tag += 
  | Unix_job_count
```
Counter event indicating the number of unix I/O jobs, i.e., the number of calls to `Lwt_unix.*` which have not yet been resolved.

```ocaml
val unix_job_count : int Runtime_events.User.t
```
```ocaml
val emit_job_count : int -> unit
```
```ocaml
module Trace : sig ... end
```