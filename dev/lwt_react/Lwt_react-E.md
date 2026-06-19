
# Module `Lwt_react.E`

```ocaml
type 'a t = 'a React.event
```
```ocaml
val never : 'a React.event
```
```ocaml
val create : unit -> 'a React.event * (?step:React.step -> 'a -> unit)
```
```ocaml
val retain : 'a React.event -> (unit -> unit) -> [ `R of unit -> unit ]
```
```ocaml
val stop : ?strong:bool -> 'a React.event -> unit
```
```ocaml
val equal : 'a React.event -> 'a React.event -> bool
```
```ocaml
val trace : 
  ?iff:bool React.signal ->
  ('a -> unit) ->
  'a React.event ->
  'a React.event
```
```ocaml
val once : 'a React.event -> 'a React.event
```
```ocaml
val drop_once : 'a React.event -> 'a React.event
```
```ocaml
val app : ('a -> 'b) React.event -> 'a React.event -> 'b React.event
```
```ocaml
val map : ('a -> 'b) -> 'a React.event -> 'b React.event
```
```ocaml
val stamp : 'b React.event -> 'a -> 'a React.event
```
```ocaml
val filter : ('a -> bool) -> 'a React.event -> 'a React.event
```
```ocaml
val fmap : ('a -> 'b option) -> 'a React.event -> 'b React.event
```
```ocaml
val diff : ('a -> 'a -> 'b) -> 'a React.event -> 'b React.event
```
```ocaml
val changes : ?eq:('a -> 'a -> bool) -> 'a React.event -> 'a React.event
```
```ocaml
val on : bool React.signal -> 'a React.event -> 'a React.event
```
```ocaml
val when_ : bool React.signal -> 'a React.event -> 'a React.event
```
```ocaml
val dismiss : 'b React.event -> 'a React.event -> 'a React.event
```
```ocaml
val until : 'a React.event -> 'b React.event -> 'b React.event
```
```ocaml
val accum : ('a -> 'a) React.event -> 'a -> 'a React.event
```
```ocaml
val fold : ('a -> 'b -> 'a) -> 'a -> 'b React.event -> 'a React.event
```
```ocaml
val select : 'a React.event list -> 'a React.event
```
```ocaml
val merge : ('a -> 'b -> 'a) -> 'a -> 'b React.event list -> 'a React.event
```
```ocaml
val switch : 'a React.event -> 'a React.event React.event -> 'a React.event
```
```ocaml
val fix : ('a React.event -> 'a React.event * 'b) -> 'b
```
```ocaml
val l1 : ('a -> 'b) -> 'a React.event -> 'b React.event
```
```ocaml
val l2 : ('a -> 'b -> 'c) -> 'a React.event -> 'b React.event -> 'c React.event
```
```ocaml
val l3 : 
  ('a -> 'b -> 'c -> 'd) ->
  'a React.event ->
  'b React.event ->
  'c React.event ->
  'd React.event
```
```ocaml
val l4 : 
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  'a React.event ->
  'b React.event ->
  'c React.event ->
  'd React.event ->
  'e React.event
```
```ocaml
val l5 : 
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a React.event ->
  'b React.event ->
  'c React.event ->
  'd React.event ->
  'e React.event ->
  'f React.event
```
```ocaml
val l6 : 
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a React.event ->
  'b React.event ->
  'c React.event ->
  'd React.event ->
  'e React.event ->
  'f React.event ->
  'g React.event
```
```ocaml
module Option : sig ... end
```

### Lwt-specific utilities

```ocaml
val with_finaliser : (unit -> unit) -> 'a event -> 'a event
```
`with_finaliser f e` returns an event `e'` which behave as `e`, except that `f` is called when `e'` is garbage collected.

```ocaml
val next : 'a event -> 'a Lwt.t
```
`next e` returns the next occurrence of `e`.

Avoid trying to create an “asynchronous loop” by calling `next e` again in a callback attached to the promise returned by `next e`:

- The callback is called within the React update step, so calling `next e` within it will return a promise that is fulfilled with the same value as the current occurrence.
- If you instead arrange for the React update step to end (for example, by calling `Lwt.pause ()` within the callback), multiple React update steps may occur before the callback calls `next e` again, so some occurrences can be effectively “lost.”
To robustly asynchronously process occurrences of `e` in a loop, use `to_stream e`, and repeatedly call [`Lwt_stream.next`](./../lwt/Lwt_stream.md#val-next) on the resulting stream.

```ocaml
val limit : (unit -> unit Lwt.t) -> 'a event -> 'a event
```
`limit f e` limits the rate of `e` with `f`.

For example, to limit the rate of an event to 1 per second you can use: `limit (fun () -> Lwt_unix.sleep 1.0) event`.

```ocaml
val from : (unit -> 'a Lwt.t) -> 'a event
```
`from f` creates an event which occurs each time `f ()` returns a value. If `f` raises an exception, the event is just stopped.

```ocaml
val to_stream : 'a event -> 'a Lwt_stream.t
```
Creates a stream holding all values occurring on the given event

```ocaml
val of_stream : 'a Lwt_stream.t -> 'a event
```
`of_stream stream` creates an event which occurs each time a value is available on the stream.

If updating the event causes an exception at any point during the update step, the exception is passed to `!`[`Lwt.async_exception_hook`](./../lwt/Lwt.md#val-async_exception_hook), which terminates the process by default.

```ocaml
val delay : 'a event Lwt.t -> 'a event
```
`delay promise` is an event which does not occur until `promise` resolves. Then it behaves as the event returned by `promise`.

```ocaml
val keep : 'a event -> unit
```
`keep e` keeps a reference to `e` so it will never be garbage collected.


### Threaded versions of React transformation functions

The following functions behave as their `React` counterpart, except that they take functions that may yield.

As usual the `_s` suffix is used when calls are serialized, and the `_p` suffix is used when they are not.

Note that `*_p` functions may not preserve event order.

```ocaml
val app_s : ('a -> 'b Lwt.t) event -> 'a event -> 'b event
```
```ocaml
val app_p : ('a -> 'b Lwt.t) event -> 'a event -> 'b event
```
```ocaml
val map_s : ('a -> 'b Lwt.t) -> 'a event -> 'b event
```
```ocaml
val map_p : ('a -> 'b Lwt.t) -> 'a event -> 'b event
```
```ocaml
val filter_s : ('a -> bool Lwt.t) -> 'a event -> 'a event
```
```ocaml
val filter_p : ('a -> bool Lwt.t) -> 'a event -> 'a event
```
```ocaml
val fmap_s : ('a -> 'b option Lwt.t) -> 'a event -> 'b event
```
```ocaml
val fmap_p : ('a -> 'b option Lwt.t) -> 'a event -> 'b event
```
```ocaml
val diff_s : ('a -> 'a -> 'b Lwt.t) -> 'a event -> 'b event
```
```ocaml
val accum_s : ('a -> 'a Lwt.t) event -> 'a -> 'a event
```
```ocaml
val fold_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event -> 'a event
```
```ocaml
val merge_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b event list -> 'a event
```
```ocaml
val run_s : 'a Lwt.t event -> 'a event
```
```ocaml
val run_p : 'a Lwt.t event -> 'a event
```