
# Module `Lwt_rte`

All the functions below are wrappers around the ones found in `Lwt_runtime_events`. The wrappers conditionally call the correspoding wrapped function or do nothing, depending if the `Lwt_runtime_events` library is available.

Note that `Lwt_runtime_events` is only installable with OCaml 5\.1+

```ocaml
val emit_paused_count : int -> unit
```
```ocaml
val emit_sch_call_begin : unit -> unit
```
```ocaml
val emit_sch_call_end : unit -> unit
```
```ocaml
val emit_sch_lap : unit -> unit
```
```ocaml
val emit_job_count : int -> unit
```
```ocaml
type span = 
  | Begin
  | End
```
```ocaml
val emit_trace : span -> string option -> string -> int -> unit
```