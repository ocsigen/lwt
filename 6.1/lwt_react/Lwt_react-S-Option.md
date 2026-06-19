
# Module `S.Option`

```ocaml
val none : 'a option React.signal
```
```ocaml
val some : 'a React.signal -> 'a option React.signal
```
```ocaml
val value : 
  ?eq:('a -> 'a -> bool) ->
  default:[ `Always of 'a React.signal | `Init of 'a React.signal ] ->
  'a option React.signal ->
  'a React.signal
```