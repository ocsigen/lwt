
# Module `S.Make`


## Parameters

```ocaml
module Eq : EqType
```

## Signature

```ocaml
type 'a v = 'a Eq.t
```
```ocaml
val create : 'a v -> 'a v React.signal * (?step:React.step -> 'a v -> unit)
```
```ocaml
val equal : 'a v React.signal -> 'a v React.signal -> bool
```
```ocaml
val hold : 'a v -> 'a v React.event -> 'a v React.signal
```
```ocaml
val app : ('a -> 'b v) React.signal -> 'a React.signal -> 'b v React.signal
```
```ocaml
val map : ('a -> 'b v) -> 'a React.signal -> 'b v React.signal
```
```ocaml
val filter : ('a v -> bool) -> 'a v -> 'a v React.signal -> 'a v React.signal
```
```ocaml
val fmap : ('a -> 'b v option) -> 'b v -> 'a React.signal -> 'b v React.signal
```
```ocaml
val when_ : bool React.signal -> 'a v -> 'a v React.signal -> 'a v React.signal
```
```ocaml
val dismiss : 'b React.event -> 'a v -> 'a v React.signal -> 'a v React.signal
```
```ocaml
val accum : ('a v -> 'a v) React.event -> 'a v -> 'a v React.signal
```
```ocaml
val fold : ('a v -> 'b -> 'a v) -> 'a v -> 'b React.event -> 'a v React.signal
```
```ocaml
val merge : 
  ('a v -> 'b -> 'a v) ->
  'a v ->
  'b React.signal list ->
  'a v React.signal
```
```ocaml
val switch : 'a v React.signal React.signal -> 'a v React.signal
```
```ocaml
val bind : 'b React.signal -> ('b -> 'a v React.signal) -> 'a v React.signal
```
```ocaml
val fix : 'a v -> ('a v React.signal -> 'a v React.signal * 'b) -> 'b
```
```ocaml
val l1 : ('a -> 'b v) -> 'a React.signal -> 'b v React.signal
```
```ocaml
val l2 : 
  ('a -> 'b -> 'c v) ->
  'a React.signal ->
  'b React.signal ->
  'c v React.signal
```
```ocaml
val l3 : 
  ('a -> 'b -> 'c -> 'd v) ->
  'a React.signal ->
  'b React.signal ->
  'c React.signal ->
  'd v React.signal
```
```ocaml
val l4 : 
  ('a -> 'b -> 'c -> 'd -> 'e v) ->
  'a React.signal ->
  'b React.signal ->
  'c React.signal ->
  'd React.signal ->
  'e v React.signal
```
```ocaml
val l5 : 
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f v) ->
  'a React.signal ->
  'b React.signal ->
  'c React.signal ->
  'd React.signal ->
  'e React.signal ->
  'f v React.signal
```
```ocaml
val l6 : 
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g v) ->
  'a React.signal ->
  'b React.signal ->
  'c React.signal ->
  'd React.signal ->
  'e React.signal ->
  'f React.signal ->
  'g v React.signal
```