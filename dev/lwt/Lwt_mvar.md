
# Module `Lwt_mvar`

Mailbox variables

“Mailbox” variables implement a synchronising variable, used for communication between concurrent threads.

```ocaml
type 'a t
```
The type of a mailbox variable. Mailbox variables are used to communicate values between threads in a synchronous way. The type parameter specifies the type of the value propagated from `put` to `take`.

```ocaml
val create : 'a -> 'a t
```
`create v` creates a new mailbox variable containing value `v`.

```ocaml
val create_empty : unit -> 'a t
```
`create ()` creates a new empty mailbox variable.

```ocaml
val put : 'a t -> 'a -> unit Lwt.t
```
`put mvar value` puts a value into a mailbox variable. This value will remain in the mailbox until `take` is called to remove it. If the mailbox is not empty, the current thread will block until it is emptied.

```ocaml
val take : 'a t -> 'a Lwt.t
```
`take mvar` will take any currently available value from the mailbox variable. If no value is currently available, the current thread will block, awaiting a value to be `put` by another thread.

```ocaml
val take_available : 'a t -> 'a option
```
`take_available mvar` immediately takes the value from `mvar` without blocking, returning `None` if the mailbox is empty.

since 3\.2.0
```ocaml
val is_empty : 'a t -> bool
```
`is_empty mvar` indicates if `put mvar` can be called without blocking.

since 3\.2.0