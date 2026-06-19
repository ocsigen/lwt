
# Class type `Lwt_stream.bounded_push`

Type of sources for bounded push-streams.

```ocaml
method size : int
```
Size of the stream.

```ocaml
method resize : int -> unit
```
Change the size of the stream queue. Note that the new size can smaller than the current stream queue size.

It raises `Stdlib.Invalid_argument` if `size < 0`.

```ocaml
method push : 'a -> unit Lwt.t
```
Pushes a new element to the stream. If the stream is full then it will block until one element is consumed. If another thread is already blocked on `push`, it raises `Lwt_stream.Full`.

```ocaml
method close : unit
```
Closes the stream. Any thread currently blocked on a call to the `push` method fails with `Lwt_stream.Closed`.

```ocaml
method count : int
```
Number of elements in the stream queue.

```ocaml
method blocked : bool
```
Is a thread is blocked on a call to the `push` method?

```ocaml
method closed : bool
```
Is the stream closed?

```ocaml
method set_reference : 'a. 'a -> unit
```
Set the reference to an external source.
