
# Module `Lwt_pqueue`

Functional priority queues (deprecated).

A priority queue maintains, in the abstract sense, a set of elements in order, and supports fast lookup and removal of the first (“minimum”) element. This is used in Lwt for organizing threads that are waiting for timeouts.

The priority queues in this module preserve “duplicates”: elements that compare equal in their order.

deprecated This module is an internal implementation detail of Lwt, and may be removed from the API at some point in the future. For alternatives, see, for example: Heaps by Jean-Cristophe Filliatre, containers, Batteries, or psq.
```ocaml
module type OrderedType = sig ... end
```
Signature pairing an element type with an ordering function.

```ocaml
module type S = sig ... end
```
Signature of priority queues.

```ocaml
module Make (Ord : OrderedType) : S with type elt = Ord.t
```
Generates priority queue types from ordered types.
