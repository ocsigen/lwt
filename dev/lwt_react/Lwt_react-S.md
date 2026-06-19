
# Module `Lwt_react.S`

```ocaml
type 'a t = 'a React.signal
```
```ocaml
val const : 'a -> 'a React.signal
```
```ocaml
val create : 
  ?eq:('a -> 'a -> bool) ->
  'a ->
  'a React.signal * (?step:React.step -> 'a -> unit)
```
```ocaml
val value : 'a React.signal -> 'a
```
```ocaml
val retain : 'a React.signal -> (unit -> unit) -> [ `R of unit -> unit ]
```
```ocaml
val eq_fun : 'a React.signal -> ('a -> 'a -> bool) option
```
```ocaml
val stop : ?strong:bool -> 'a React.signal -> unit
```
```ocaml
val equal : 
  ?eq:('a -> 'a -> bool) ->
  'a React.signal ->
  'a React.signal ->
  bool
```
```ocaml
val trace : ?iff:bool t -> ('a -> unit) -> 'a React.signal -> 'a React.signal
```
```ocaml
val hold : ?eq:('a -> 'a -> bool) -> 'a -> 'a React.event -> 'a React.signal
```
```ocaml
val app : 
  ?eq:('b -> 'b -> bool) ->
  ('a -> 'b) React.signal ->
  'a React.signal ->
  'b React.signal
```
```ocaml
val map : 
  ?eq:('b -> 'b -> bool) ->
  ('a -> 'b) ->
  'a React.signal ->
  'b React.signal
```
```ocaml
val filter : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> bool) ->
  'a ->
  'a React.signal ->
  'a React.signal
```
```ocaml
val fmap : 
  ?eq:('b -> 'b -> bool) ->
  ('a -> 'b option) ->
  'b ->
  'a React.signal ->
  'b React.signal
```
```ocaml
val diff : ('a -> 'a -> 'b) -> 'a React.signal -> 'b React.event
```
```ocaml
val changes : 'a React.signal -> 'a React.event
```
```ocaml
val sample : 
  ('b -> 'a -> 'c) ->
  'b React.event ->
  'a React.signal ->
  'c React.event
```
```ocaml
val on : 
  ?eq:('a -> 'a -> bool) ->
  bool React.signal ->
  'a ->
  'a React.signal ->
  'a React.signal
```
```ocaml
val when_ : 
  ?eq:('a -> 'a -> bool) ->
  bool React.signal ->
  'a ->
  'a React.signal ->
  'a React.signal
```
```ocaml
val dismiss : 
  ?eq:('a -> 'a -> bool) ->
  'b React.event ->
  'a ->
  'a React.signal ->
  'a React.signal
```
```ocaml
val accum : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> 'a) React.event ->
  'a ->
  'a React.signal
```
```ocaml
val fold : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> 'b -> 'a) ->
  'a ->
  'b React.event ->
  'a React.signal
```
```ocaml
val merge : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> 'b -> 'a) ->
  'a ->
  'b React.signal list ->
  'a React.signal
```
```ocaml
val switch : 
  ?eq:('a -> 'a -> bool) ->
  'a React.signal React.signal ->
  'a React.signal
```
```ocaml
val fix : 
  ?eq:('a -> 'a -> bool) ->
  'a ->
  ('a React.signal -> 'a React.signal * 'b) ->
  'b
```
```ocaml
val l1 : 
  ?eq:('b -> 'b -> bool) ->
  ('a -> 'b) ->
  'a React.signal ->
  'b React.signal
```
```ocaml
val l2 : 
  ?eq:('c -> 'c -> bool) ->
  ('a -> 'b -> 'c) ->
  'a React.signal ->
  'b React.signal ->
  'c React.signal
```
```ocaml
val l3 : 
  ?eq:('d -> 'd -> bool) ->
  ('a -> 'b -> 'c -> 'd) ->
  'a React.signal ->
  'b React.signal ->
  'c React.signal ->
  'd React.signal
```
```ocaml
val l4 : 
  ?eq:('e -> 'e -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  'a React.signal ->
  'b React.signal ->
  'c React.signal ->
  'd React.signal ->
  'e React.signal
```
```ocaml
val l5 : 
  ?eq:('f -> 'f -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a React.signal ->
  'b React.signal ->
  'c React.signal ->
  'd React.signal ->
  'e React.signal ->
  'f React.signal
```
```ocaml
val l6 : 
  ?eq:('g -> 'g -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a React.signal ->
  'b React.signal ->
  'c React.signal ->
  'd React.signal ->
  'e React.signal ->
  'f React.signal ->
  'g React.signal
```
```ocaml
module Bool : sig ... end
```
```ocaml
module Int : sig ... end
```
```ocaml
module Float : sig ... end
```
```ocaml
module Pair : sig ... end
```
```ocaml
module Option : sig ... end
```
```ocaml
module Compare : sig ... end
```
```ocaml
module type EqType = sig ... end
```
```ocaml
module type S = sig ... end
```
```ocaml
module Make (Eq : EqType) : sig ... end
```
```ocaml
module Special : sig ... end
```

### Monadic interface

```ocaml
val return : 'a -> 'a signal
```
Same as `const`.

```ocaml
val bind : 
  ?eq:('b -> 'b -> bool) ->
  'a signal ->
  ('a -> 'b signal) ->
  'b signal
```
`bind ?eq s f` is initially `f x` where `x` is the current value of `s`. Each time `s` changes to a new value `y`, `bind signal f` is set to `f y`, until the next change of `signal`.

```ocaml
val bind_s : 
  ?eq:('b -> 'b -> bool) ->
  'a signal ->
  ('a -> 'b signal Lwt.t) ->
  'b signal Lwt.t
```
Same as [`bind`](./#val-bind) except that `f` returns a promise. Calls to `f` are serialized.


### Lwt-specific utilities

```ocaml
val with_finaliser : (unit -> unit) -> 'a signal -> 'a signal
```
`with_finaliser f s` returns a signal `s'` which behaves as `s`, except that `f` is called when `s'` is garbage collected.

```ocaml
val limit : 
  ?eq:('a -> 'a -> bool) ->
  (unit -> unit Lwt.t) ->
  'a signal ->
  'a signal
```
`limit f s` limits the rate of `s` update with `f`.

For example, to limit it to 1 per second, you can use: `limit (fun () -> Lwt_unix.sleep 1.0) s`.

```ocaml
val keep : 'a signal -> unit
```
`keep s` keeps a reference to `s` so it will never be garbage collected.


### Threaded versions of React transformation functions

The following functions behave as their `React` counterpart, except that they take functions that may yield.

The `_s` suffix means that calls are serialized.

```ocaml
val app_s : 
  ?eq:('b -> 'b -> bool) ->
  ('a -> 'b Lwt.t) signal ->
  'a signal ->
  'b signal Lwt.t
```
```ocaml
val map_s : 
  ?eq:('b -> 'b -> bool) ->
  ('a -> 'b Lwt.t) ->
  'a signal ->
  'b signal Lwt.t
```
```ocaml
val filter_s : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> bool Lwt.t) ->
  'a ->
  'a signal ->
  'a signal Lwt.t
```
```ocaml
val fmap_s : 
  ?eq:('b -> 'b -> bool) ->
  ('a -> 'b option Lwt.t) ->
  'b ->
  'a signal ->
  'b signal Lwt.t
```
```ocaml
val diff_s : ('a -> 'a -> 'b Lwt.t) -> 'a signal -> 'b event
```
```ocaml
val sample_s : ('b -> 'a -> 'c Lwt.t) -> 'b event -> 'a signal -> 'c event
```
```ocaml
val accum_s : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> 'a Lwt.t) event ->
  'a ->
  'a signal
```
```ocaml
val fold_s : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> 'b -> 'a Lwt.t) ->
  'a ->
  'b event ->
  'a signal
```
```ocaml
val merge_s : 
  ?eq:('a -> 'a -> bool) ->
  ('a -> 'b -> 'a Lwt.t) ->
  'a ->
  'b signal list ->
  'a signal Lwt.t
```
```ocaml
val l1_s : 
  ?eq:('b -> 'b -> bool) ->
  ('a -> 'b Lwt.t) ->
  'a signal ->
  'b signal Lwt.t
```
```ocaml
val l2_s : 
  ?eq:('c -> 'c -> bool) ->
  ('a -> 'b -> 'c Lwt.t) ->
  'a signal ->
  'b signal ->
  'c signal Lwt.t
```
```ocaml
val l3_s : 
  ?eq:('d -> 'd -> bool) ->
  ('a -> 'b -> 'c -> 'd Lwt.t) ->
  'a signal ->
  'b signal ->
  'c signal ->
  'd signal Lwt.t
```
```ocaml
val l4_s : 
  ?eq:('e -> 'e -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e Lwt.t) ->
  'a signal ->
  'b signal ->
  'c signal ->
  'd signal ->
  'e signal Lwt.t
```
```ocaml
val l5_s : 
  ?eq:('f -> 'f -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f Lwt.t) ->
  'a signal ->
  'b signal ->
  'c signal ->
  'd signal ->
  'e signal ->
  'f signal Lwt.t
```
```ocaml
val l6_s : 
  ?eq:('g -> 'g -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g Lwt.t) ->
  'a signal ->
  'b signal ->
  'c signal ->
  'd signal ->
  'e signal ->
  'f signal ->
  'g signal Lwt.t
```
```ocaml
val run_s : ?eq:('a -> 'a -> bool) -> 'a Lwt.t signal -> 'a signal Lwt.t
```