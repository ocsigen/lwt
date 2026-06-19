
# Module `Lwt_throttle`

Rate limiters.

A rate limiter allows generating sets of promises that will be resolved in the future, at a maximum rate of N promises per second.

The rate limiters in this module support multiple *channels*, each given a different key by the user. The rate limit applies to each channel independently.

```ocaml
module type S = sig ... end
```
```ocaml
module Make (H : Stdlib.Hashtbl.HashedType) : S with type key = H.t
```