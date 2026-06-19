
# Module `S.Pair`

```ocaml
val pair : 
  ?eq:(('a * 'b) -> ('a * 'b) -> bool) ->
  'a React.signal ->
  'b React.signal ->
  ('a * 'b) React.signal
```
```ocaml
val fst : ?eq:('a -> 'a -> bool) -> ('a * 'b) React.signal -> 'a React.signal
```
```ocaml
val snd : ?eq:('a -> 'a -> bool) -> ('b * 'a) React.signal -> 'a React.signal
```