
# Module `S.Bool`

```ocaml
val zero : bool React.signal
```
```ocaml
val one : bool React.signal
```
```ocaml
val not : bool React.signal -> bool React.signal
```
```ocaml
val (&&) : bool React.signal -> bool React.signal -> bool React.signal
```
```ocaml
val (||) : bool React.signal -> bool React.signal -> bool React.signal
```
```ocaml
val edge : bool React.signal -> bool React.event
```
```ocaml
val rise : bool React.signal -> unit React.event
```
```ocaml
val fall : bool React.signal -> unit React.event
```
```ocaml
val flip : bool -> 'a React.event -> bool React.signal
```