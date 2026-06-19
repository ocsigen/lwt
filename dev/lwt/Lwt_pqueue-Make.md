
# Module `Lwt_pqueue.Make`

Generates priority queue types from ordered types.


## Parameters

```ocaml
module Ord : OrderedType
```

## Signature

```ocaml
type elt = Ord.t
```
Type of elements contained in the priority queue.

```ocaml
type t
```
Type of priority queues.

```ocaml
val empty : t
```
The empty priority queue. Contains no elements.

```ocaml
val is_empty : t -> bool
```
`is_empty q` evaluates to `true` iff `q` is empty.

```ocaml
val add : elt -> t -> t
```
`add e q` evaluates to a new priority queue, which contains all the elements of `q`, and the additional element `e`.

```ocaml
val union : t -> t -> t
```
`union q q'` evaluates to a new priority queue, which contains all the elements of both `q` and `q'`.

```ocaml
val find_min : t -> elt
```
`find_min q` evaluates to the minimum element of `q` if it is not empty, and raises `Not_found` otherwise.

```ocaml
val lookup_min : t -> elt option
```
`lookup_min q` evaluates to `Some e`, where `e` is the minimum element of `q`, if `q` is not empty, and evaluates to `None` otherwise.

```ocaml
val remove_min : t -> t
```
`remove_min q` evaluates to a new priority queue, which contains all the elements of `q` except for its minimum element. Raises `Not_found` if `q` is empty.

```ocaml
val size : t -> int
```
`size q` evaluates to the number of elements in `q`.
