
# Module `Lwt_list`

List helpers

Note: this module use the same naming convention as [`Lwt_stream`](./Lwt_stream.md).


### List iterators

```ocaml
val iter_s : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t
```
```ocaml
val iter_p : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t
```
```ocaml
val iteri_s : (int -> 'a -> unit Lwt.t) -> 'a list -> unit Lwt.t
```
```ocaml
val iteri_p : (int -> 'a -> unit Lwt.t) -> 'a list -> unit Lwt.t
```
```ocaml
val map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
```
```ocaml
val map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
```
```ocaml
val mapi_s : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
```
```ocaml
val mapi_p : (int -> 'a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
```
```ocaml
val rev_map_s : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
```
```ocaml
val rev_map_p : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
```
```ocaml
val fold_left_s : ('a -> 'b -> 'a Lwt.t) -> 'a -> 'b list -> 'a Lwt.t
```
```ocaml
val fold_right_s : ('a -> 'b -> 'b Lwt.t) -> 'a list -> 'b -> 'b Lwt.t
```

### List scanning

```ocaml
val for_all_s : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t
```
```ocaml
val for_all_p : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t
```
```ocaml
val exists_s : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t
```
```ocaml
val exists_p : ('a -> bool Lwt.t) -> 'a list -> bool Lwt.t
```

### List searching

```ocaml
val find_s : ('a -> bool Lwt.t) -> 'a list -> 'a Lwt.t
```
```ocaml
val filter_s : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t
```
```ocaml
val filter_p : ('a -> bool Lwt.t) -> 'a list -> 'a list Lwt.t
```
```ocaml
val filter_map_s : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t
```
```ocaml
val filter_map_p : ('a -> 'b option Lwt.t) -> 'a list -> 'b list Lwt.t
```
```ocaml
val partition_s : ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t
```
```ocaml
val partition_p : ('a -> bool Lwt.t) -> 'a list -> ('a list * 'a list) Lwt.t
```