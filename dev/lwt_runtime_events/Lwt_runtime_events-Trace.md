
# Module `Lwt_runtime_events.Trace`

```ocaml
type t = {
  kind : Runtime_events.Type.span;
  context : string option;
  filename : string;
  line : int;
}
```
```ocaml
val t : t Runtime_events.Type.t
```
```ocaml
type Runtime_events.User.tag += 
  | T
```
Span event indicating that a promise is taking time to resolve. The event indicates the location of the bind for the promise. These events are automatically emitted by code produced by the lwt\_ppx syntax extension.

```ocaml
val span : t Runtime_events.User.t
```
```ocaml
val emit : t -> unit
```