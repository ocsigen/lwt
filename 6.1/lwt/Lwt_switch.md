
# Module `Lwt_switch`

Lwt switches

Switch has two goals:

- being able to free multiple resources at the same time,
- offer a better alternative than always returning an id to free some resource.
For example, consider the following interface:

```ocaml
  type id

  val free : id -> unit Lwt.t

  val f : unit -> id Lwt.t
  val g : unit -> id Lwt.t
  val h : unit -> id Lwt.t
```
Now you want to call `f`, `g` and `h` in parallel. You can simply do:

```ocaml
  lwt idf = f () and idg = g () and idh = h () in
  ...
```
However, one may want to handle possible failures of `f ()`, `g ()` and `h ()`, and disable all allocated resources if one of these three threads fails. This may be hard since you have to remember which one failed and which one returned correctly.

Now if we change the interface a little bit:

```ocaml
  val f : ?switch : Lwt_switch.t -> unit -> id Lwt.t
  val g : ?switch : Lwt_switch.t -> unit -> id Lwt.t
  val h : ?switch : Lwt_switch.t -> unit -> id Lwt.t
```
the code becomes:

```ocaml
  Lwt_switch.with_switch (fun switch ->
    lwt idf = f ~switch ()
    and idg = g ~switch ()
    and idh = h ~switch () in
    ...
  )
```
```ocaml
type t
```
Type of switches.

```ocaml
val create : unit -> t
```
`create ()` creates a new switch.

```ocaml
val with_switch : (t -> 'a Lwt.t) -> 'a Lwt.t
```
`with_switch fn` is `fn switch`, where `switch` is a fresh switch that is turned off when the callback thread finishes (whether it succeeds or fails).

since 2\.6.0
```ocaml
val is_on : t -> bool
```
`is_on switch` returns `true` if the switch is currently on, and `false` otherwise.

```ocaml
val turn_off : t -> unit Lwt.t
```
`turn_off switch` turns off the switch. It calls all registered hooks, waits for all of them to terminate, then returns. If one of the hooks failed, it will fail with the exception raised by the hook. If the switch is already off, it does nothing.

```ocaml
exception Off
```
Exception raised when trying to add a hook to a switch that is already off.

```ocaml
val check : t option -> unit
```
`check switch` does nothing if `switch` is `None` or contains an switch that is currently on, and raises [`Off`](./#exception-Off) otherwise.

```ocaml
val add_hook : t option -> (unit -> unit Lwt.t) -> unit
```
`add_hook switch f` registers `f` so it will be called when [`turn_off`](./#val-turn_off) is invoked. It does nothing if `switch` is `None`. If `switch` contains an switch that is already off then [`Off`](./#exception-Off) is raised.

```ocaml
val add_hook_or_exec : t option -> (unit -> unit Lwt.t) -> unit Lwt.t
```
`add_hook_or_exec switch f` is the same as [`add_hook`](./#val-add_hook) except that if the switch is already off, `f` is called immediately.
