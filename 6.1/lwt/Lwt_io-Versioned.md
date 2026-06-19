
# Module `Lwt_io.Versioned`

Versioned variants of APIs undergoing breaking changes.

```ocaml
val establish_server_1 : 
  ?fd:Lwt_unix.file_descr ->
  ?buffer_size:int ->
  ?backlog:int ->
  Unix.sockaddr ->
  ((input_channel * output_channel) -> unit) ->
  server
```
Old version of [`Lwt_io.establish_server`](./Lwt_io.md#val-establish_server). The current [`Lwt_io.establish_server`](./Lwt_io.md#val-establish_server) automatically closes channels passed to the callback, and notifies the caller when the server's listening socket is bound.

deprecated Use Lwt\_io.establish\_server\_with\_client\_address.
since 2\.7.0
```ocaml
val establish_server_2 : 
  ?fd:Lwt_unix.file_descr ->
  ?buffer_size:int ->
  ?backlog:int ->
  ?no_close:bool ->
  Unix.sockaddr ->
  ((input_channel * output_channel) -> unit Lwt.t) ->
  server Lwt.t
```
Since Lwt 3\.0.0, this is just an alias for [`Lwt_io.establish_server`](./Lwt_io.md#val-establish_server).

deprecated Use Lwt\_io.establish\_server\_with\_client\_address.
since 2\.7.0
```ocaml
val shutdown_server_1 : server -> unit
```
Old version of [`Lwt_io.shutdown_server`](./Lwt_io.md#val-shutdown_server). The current [`Lwt_io.shutdown_server`](./Lwt_io.md#val-shutdown_server) returns a promise, which resolves when the server's listening socket is closed.

deprecated Use Lwt\_io.shutdown\_server.
since 2\.7.0
```ocaml
val shutdown_server_2 : server -> unit Lwt.t
```
Since Lwt 3\.0.0, this is just an alias for [`Lwt_io.shutdown_server`](./Lwt_io.md#val-shutdown_server).

deprecated Use Lwt\_io.shutdown\_server.
since 2\.7.0