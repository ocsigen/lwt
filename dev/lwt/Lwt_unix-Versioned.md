
# Module `Lwt_unix.Versioned`

Versioned variants of APIs undergoing breaking changes.

```ocaml
val bind_1 : file_descr -> sockaddr -> unit
```
Old version of [`Lwt_unix.bind`](./Lwt_unix.md#val-bind). The current [`Lwt_unix.bind`](./Lwt_unix.md#val-bind) evaluates to a promise, because the internal `bind(2)` system call can block if the given socket is a Unix domain socket.

deprecated Use Lwt\_unix.bind.
since 2\.7.0
```ocaml
val bind_2 : file_descr -> sockaddr -> unit Lwt.t
```
Since Lwt 3\.0.0, this is just an alias for [`Lwt_unix.bind`](./Lwt_unix.md#val-bind).

deprecated Use Lwt\_unix.bind.
since 2\.7.0
```ocaml
val recv_msg_2 : 
  socket:file_descr ->
  io_vectors:IO_vectors.t ->
  (int * Unix.file_descr list) Lwt.t
```
Since Lwt 5\.0.0, this is an alias for [`Lwt_unix.recv_msg`](./Lwt_unix.md#val-recv_msg).

deprecated Use Lwt\_unix.recv\_msg.
since 4\.3.0
```ocaml
val send_msg_2 : 
  socket:file_descr ->
  io_vectors:IO_vectors.t ->
  fds:Unix.file_descr list ->
  int Lwt.t
```
Since Lwt 5\.0.0, this is an alias for [`Lwt_unix.send_msg`](./Lwt_unix.md#val-send_msg).

deprecated Use Lwt\_unix.send\_msg.
since 4\.3.0