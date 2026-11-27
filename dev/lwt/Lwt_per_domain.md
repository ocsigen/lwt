# Module `Lwt_per_domain`

Values with one instance per domain.

An `Lwt_mutex.t`, an `Lwt_pool.t` or any other value holding Lwt state belongs to the domain that created it, and using it from another domain raises [`Lwt.Domain_crossing`](./Lwt.md#exception-Domain_crossing). A value created when a module is initialized therefore belongs to the main domain: a library that keeps a mutex or a pool at top level works with one domain and fails with several. This module is the way to keep such a value at top level and still have one per domain:

```ocaml
  let lock = Lwt_per_domain.make Lwt_mutex.create

  let with_resource f = Lwt_mutex.with_lock (Lwt_per_domain.get lock) f
```
A domain that calls `Lwt_main.run` again later finds the instances it made before, in the state its last run left them. With a single domain, which is always the case on OCaml 4\.14 and under js\_of\_ocaml, there is a single instance.

```ocaml
type 'a t
```
A value with one instance per domain.

```ocaml
val make : (unit -> 'a) -> 'a t
```
`make init` is a value whose instance in each domain is made by `init ()`, on that domain, the first time it calls [`get`](./#val-get). `init` runs at most once per domain: if it raises an exception, every later `get` on that domain raises the same exception.

Call `make` at top level only. Each call takes a slot in every domain, for the lifetime of the program, so a call per connection or per request would leak.

```ocaml
val get : 'a t -> 'a
```
`get t` is the calling domain's instance of `t`.
