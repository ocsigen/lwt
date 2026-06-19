
# Module `Lwt_unix.IO_vectors`

Sequences of buffer slices for [`writev`](./Lwt_unix.md#val-writev).

```ocaml
type t
```
Mutable sequences of I/O vectors. An I/O vector describes a slice of a `bytes` or `Bigarray` buffer. Each I/O vector is a triple containing a reference to the buffer, an offset into the buffer where the slice begins, and the length of the slice.

```ocaml
type _bigarray =
  (char, Stdlib.Bigarray.int8_unsigned_elt, Stdlib.Bigarray.c_layout)
    Stdlib.Bigarray.Array1.t
```
Type abbreviation equivalent to [`Lwt_bytes.t`](./Lwt_bytes.md#type-t). Do not use this type name directly; use [`Lwt_bytes.t`](./Lwt_bytes.md#type-t) instead.

```ocaml
val create : unit -> t
```
Creates an empty I/O vector sequence.

```ocaml
val append_bytes : t -> bytes -> int -> int -> unit
```
`append_bytes vs buffer offset length` appends a slice of the `bytes` buffer `buffer` beginning at `offset` and with length `length` to the I/O vector sequence `vs`.

```ocaml
val append_bigarray : t -> _bigarray -> int -> int -> unit
```
`append_bigarray vs buffer offset length` appends a slice of the `Bigarray` buffer `buffer` beginning at `offset` and with length `length` to the I/O vector sequence `vs`.

```ocaml
val drop : t -> int -> unit
```
`drop vs n` adjusts the I/O vector sequence `vs` so that it no longer includes its first `n` bytes.

```ocaml
val is_empty : t -> bool
```
`is_empty vs` is `true` if and only if `vs` has no I/O vectors, or all I/O vectors in `vs` have zero bytes.

```ocaml
val byte_count : t -> int
```
`byte_count vs` is the total number of bytes in `vs`.

since 4\.2.0
```ocaml
val system_limit : int option
```
Some systems limit the number of I/O vectors that can be passed in a single call to their `writev` or `readv` system calls. On those systems, if the limit is `n`, this value is equal to `Some n`. On systems without such a limit, the value is equal to `None`.

Unless you need atomic I/O operations, you can ignore this limit. The Lwt binding automatically respects it internally. See [`Lwt_unix.writev`](./Lwt_unix.md#val-writev).

A typical limit is 1024 vectors.
