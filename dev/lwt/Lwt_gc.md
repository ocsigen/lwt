
# Module `Lwt_gc`

Interaction with the garbage collector

This module offers a convenient way to add a finaliser launching a thread to a value, without having to use `Lwt_unix.run` in the finaliser.

```ocaml
val finalise : ('a -> unit Lwt.t) -> 'a -> unit
```
`finalise f x` ensures `f x` is evaluated after `x` has been garbage collected. If `f x` yields, then Lwt will wait for its termination at the end of the program.

Note that `f x` is not called at garbage collection time, but later in the main loop.

```ocaml
val finalise_or_exit : ('a -> unit Lwt.t) -> 'a -> unit
```
`finalise_or_exit f x` call `f x` when `x` is garbage collected or (exclusively) when the program exits.
