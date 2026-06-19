
# Module type `Lwt_throttle.S`

```ocaml
type key
```
```ocaml
type t
```
```ocaml
val create : rate:int -> max:int -> n:int -> t
```
Creates a rate limiter.

parameter rate Maximum number of promise resolutions per second, per channel.
parameter max Maximum number of pending promises allowed at once, over all channels.
parameter n Initial size of the internal channel hash table. This should be approximately the number of different channels that will be used.
```ocaml
val wait : t -> key -> bool Lwt.t
```
`Lwt_throttle.wait limiter channel` returns a new promise associated with the given rate limiter and channel.

If the maximum number of pending promises for `limiter` has *not* been reached, the promise starts pending. It will be resolved with `true` at some future time, such that the rate limit of `limiter` is not exceeded, with respect to other promises in the same `channel`.

If the maximum number of pending promises has been reached, the returned promise is already resolved with `false`.
