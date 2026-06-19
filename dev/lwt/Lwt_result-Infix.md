
# Module `Lwt_result.Infix`

```ocaml
val (>|=) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
```
```ocaml
val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
```