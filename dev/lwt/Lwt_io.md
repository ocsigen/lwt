
# Module `Lwt_io`

Buffered byte channels

A **channel** is a high-level object for performing input/output (IO). It allows to read/write from/to the outside world in an efficient way, by minimising the number of system calls.

An **output channel** is used to send data and an **input channel** is used to receive data.

If you are familiar with buffered channels you may be familiar too with the **flush** operation. Note that byte channels of this module are automatically flushed when there is nothing else to do (i.e. before the program becomes idle), so this means that you no longer have to write:

```ocaml
  eprintf "log message\n";
  flush stderr;
```
to have your messages displayed.

Note about errors: input functions of this module raise `End_of_file` when the end-of-file is reached (i.e. when the read function returns `0`). Other exceptions are ones caused by the backend read/write functions, such as `Unix.Unix_error`.

```ocaml
exception Channel_closed of string
```
Exception raised when a channel is closed. The parameter is a description of the channel.


### Types

```ocaml
type 'mode channel
```
Type of buffered byte channels

```ocaml
type input
```
Input mode

```ocaml
type output
```
Output mode

```ocaml
type 'a mode = 
  | Input : input mode
  | Output : output mode
```
Channel mode

```ocaml
val input : input mode
```
`input` input mode representation

```ocaml
val output : output mode
```
`output` output mode representation

```ocaml
type input_channel = input channel
```
Type of input channels

```ocaml
type output_channel = output channel
```
Type of output channels

```ocaml
val mode : 'a channel -> 'a mode
```
`mode ch` returns the mode of a channel


### Well-known instances

```ocaml
val stdin : input_channel
```
The standard input, it reads data from [`Lwt_unix.stdin`](./Lwt_unix.md#val-stdin)

```ocaml
val stdout : output_channel
```
The standard output, it writes data to [`Lwt_unix.stdout`](./Lwt_unix.md#val-stdout)

```ocaml
val stderr : output_channel
```
The standard output for error messages, it writes data to [`Lwt_unix.stderr`](./Lwt_unix.md#val-stderr)

```ocaml
val zero : input_channel
```
Inputs which returns always `'\x00'`

```ocaml
val null : output_channel
```
Output which drops everything


### Channels creation/manipulation

```ocaml
val pipe : 
  ?cloexec:bool ->
  ?in_buffer:Lwt_bytes.t ->
  ?out_buffer:Lwt_bytes.t ->
  unit ->
  input_channel * output_channel
```
`pipe ?cloexec ?in_buffer ?out_buffer ()` creates a pipe using [`Lwt_unix.pipe`](./Lwt_unix.md#val-pipe) and makes two channels from the two returned file descriptors

```ocaml
val make : 
  ?buffer:Lwt_bytes.t ->
  ?close:(unit -> unit Lwt.t) ->
  ?seek:(int64 -> Unix.seek_command -> int64 Lwt.t) ->
  mode:'mode mode ->
  (Lwt_bytes.t -> int -> int -> int Lwt.t) ->
  'mode channel
```
`make ?buffer ?close ~mode perform_io` is the main function for creating new channels.

parameter buffer user-supplied buffer. When this argument is present, its value will be used as the buffer for the created channel. The size of buffer must conform to the limitations described in set\_default\_buffer\_size. When this argument is not present, a new internal buffer of default size will be allocated for this channel.
Warning: do not use the same buffer for simultaneous work with more than one channel.

There are other functions in this module that take a `buffer` argument, sharing the same semantics.

parameter close close function of the channel. It defaults to Lwt.return
parameter seek same meaning as Unix.lseek
parameter mode either input or output
parameter perform\_io is the read or write function. It is called when more input is needed or when the buffer need to be flushed.
```ocaml
val of_bytes : mode:'mode mode -> Lwt_bytes.t -> 'mode channel
```
Create a channel from a byte array. Reading/writing is done directly on the provided array.

```ocaml
val of_fd : 
  ?buffer:Lwt_bytes.t ->
  ?close:(unit -> unit Lwt.t) ->
  mode:'mode mode ->
  Lwt_unix.file_descr ->
  'mode channel
```
`of_fd ?buffer ?close ~mode fd` creates a channel from a file descriptor.

parameter close defaults to closing the file descriptor.
```ocaml
val of_unix_fd : 
  ?buffer:Lwt_bytes.t ->
  ?close:(unit -> unit Lwt.t) ->
  mode:'mode mode ->
  Unix.file_descr ->
  'mode channel
```
`of_unix_fd ?buffer ?close ~mode fd` is a short-hand for:

`of_fd ?buffer ?close (Lwt_unix.of_unix_file_descr fd)`

```ocaml
val close : 'a channel -> unit Lwt.t
```
`close ch` closes the given channel. If `ch` is an output channel, it performs all pending actions, flushes it and closes it. If `ch` is an input channel, it just closes it immediately.

`close` returns the result of the close function of the channel. Multiple calls to `close` will return exactly the same value.

Note: you cannot use `close` on channels obtained with [`atomic`](./#val-atomic).

```ocaml
val abort : 'a channel -> unit Lwt.t
```
`abort ch` abort current operations and close the channel immediately.

```ocaml
val atomic : ('a channel -> 'b Lwt.t) -> 'a channel -> 'b Lwt.t
```
`atomic f` transforms a sequence of io operations into one single atomic io operation.

Note:

- the channel passed to `f` is invalid after `f` terminates
- `atomic` can be called inside another `atomic`
```ocaml
val file_length : string -> int64 Lwt.t
```
Retrieves the length of the file at the given path. If the path refers to a directory, the returned promise is rejected with `Unix.(Unix_error (EISDIR, _, _))`.

```ocaml
val buffered : 'a channel -> int
```
`buffered oc` returns the number of bytes in the buffer

```ocaml
val flush : output_channel -> unit Lwt.t
```
`flush oc` performs all pending writes on `oc`

```ocaml
val flush_all : unit -> unit Lwt.t
```
`flush_all ()` flushes all open output channels

```ocaml
val buffer_size : 'a channel -> int
```
Returns the size of the internal buffer.

```ocaml
val resize_buffer : 'a channel -> int -> unit Lwt.t
```
Resize the internal buffer to the given size

```ocaml
val is_busy : 'a channel -> bool
```
`is_busy channel` returns whether the given channel is currently busy. A channel is busy when there is at least one job using it that has not yet terminated.

```ocaml
val is_closed : 'a channel -> bool
```
`is_closed channel` returns whether the given channel is currently closed.

since 4\.2.0

### Random access

```ocaml
val position : 'a channel -> int64
```
`position ch` Returns the current position in the channel.

```ocaml
val set_position : 'a channel -> int64 -> unit Lwt.t
```
`set_position ch pos` Sets the position in the output channel. This does not work if the channel does not support random access.

```ocaml
val length : 'a channel -> int64 Lwt.t
```
Returns the length of the channel in bytes


### Reading

Note: except for functions dealing with streams ([`read_chars`](./#val-read_chars) and [`read_lines`](./#val-read_lines)) all functions are **atomic**.

```ocaml
val read_char : input_channel -> char Lwt.t
```
`read_char ic` reads the next character of `ic`.

raises `End_of_file` if the end of the file is reached
```ocaml
val read_char_opt : input_channel -> char option Lwt.t
```
Same as [`Lwt_io.read_char`](./#val-read_char), but does not raise `End_of_file` on end of input

```ocaml
val read_chars : input_channel -> char Lwt_stream.t
```
`read_chars ic` returns a stream holding all characters of `ic`

```ocaml
val read_line : input_channel -> string Lwt.t
```
`read_line ic` reads one complete line from `ic` and returns it without the end of line. End of line is either `"\n"` or `"\r\n"`.

If the end of input is reached before reading any character, `End_of_file` is raised. If it is reached before reading an end of line but characters have already been read, they are returned.

```ocaml
val read_line_opt : input_channel -> string option Lwt.t
```
Same as [`read_line`](./#val-read_line) but do not raise `End_of_file` on end of input.

```ocaml
val read_lines : input_channel -> string Lwt_stream.t
```
`read_lines ic` returns a stream holding all lines of `ic`

```ocaml
val read : ?count:int -> input_channel -> string Lwt.t
```
If `~count` is specified, `read ~count ic` reads at most `~count` bytes from `ic` in one read operation. Note that fewer than `~count` bytes can be read. This can happen for multiple reasons, including end of input, or no more data currently available. Check the size of the resulting string. `read` resolves with `""` if the input channel is already at the end of input.

If `~count` is not specified, `read ic` reads all bytes until the end of input.

```ocaml
val read_into : input_channel -> bytes -> int -> int -> int Lwt.t
```
`read_into ic buffer offset length` reads up to `length` bytes, stores them in `buffer` at offset `offset`, and returns the number of bytes read.

Note: `read_into` does not raise `End_of_file`, it returns a length of `0` instead.

```ocaml
val read_into_exactly : input_channel -> bytes -> int -> int -> unit Lwt.t
```
`read_into_exactly ic buffer offset length` reads exactly `length` bytes and stores them in `buffer` at offset `offset`.

raises `End_of_file` on end of input
```ocaml
val read_into_bigstring : 
  input_channel ->
  Lwt_bytes.t ->
  int ->
  int ->
  int Lwt.t
```
```ocaml
val read_into_exactly_bigstring : 
  input_channel ->
  Lwt_bytes.t ->
  int ->
  int ->
  unit Lwt.t
```
```ocaml
val read_value : input_channel -> 'a Lwt.t
```
`read_value channel` reads a marshaled value from `channel`; it corresponds to the standard library's `Stdlib.Marshal.from_channel`. The corresponding writing function is [`write_value`](./#val-write_value).

Note that reading marshaled values is *not*, in general, type-safe. See the warning in the description of module `Stdlib.Marshal` for details. The short version is: if you read a value of one type, such as `string`, when a value of another type, such as `int` has actually been marshaled to `channel`, you may get arbitrary behavior, including segmentation faults, access violations, security bugs, etc.


### Writing

Note: as for reading functions, all functions except [`write_chars`](./#val-write_chars) and [`write_lines`](./#val-write_lines) are **atomic**.

For example if you use [`write_line`](./#val-write_line) in two different threads, the two operations will be serialized, and lines cannot be mixed.

```ocaml
val write_char : output_channel -> char -> unit Lwt.t
```
`write_char oc char` writes `char` on `oc`

```ocaml
val write_chars : output_channel -> char Lwt_stream.t -> unit Lwt.t
```
`write_chars oc chars` writes all characters of `chars` on `oc`

```ocaml
val write : output_channel -> string -> unit Lwt.t
```
`write oc str` writes all characters of `str` on `oc`

```ocaml
val write_line : output_channel -> string -> unit Lwt.t
```
`write_line oc str` writes `str` on `oc` followed by a new-line.

```ocaml
val write_lines : output_channel -> string Lwt_stream.t -> unit Lwt.t
```
`write_lines oc lines` writes all lines of `lines` to `oc`

```ocaml
val write_from : output_channel -> bytes -> int -> int -> int Lwt.t
```
`write_from oc buffer offset length` writes up to `length` bytes to `oc`, from `buffer` at offset `offset` and returns the number of bytes actually written

```ocaml
val write_from_bigstring : 
  output_channel ->
  Lwt_bytes.t ->
  int ->
  int ->
  int Lwt.t
```
```ocaml
val write_from_string : output_channel -> string -> int -> int -> int Lwt.t
```
See [`write`](./#val-write).

```ocaml
val write_from_exactly : output_channel -> bytes -> int -> int -> unit Lwt.t
```
`write_from_exactly oc buffer offset length` writes all `length` bytes from `buffer` at offset `offset` to `oc`

```ocaml
val write_from_exactly_bigstring : 
  output_channel ->
  Lwt_bytes.t ->
  int ->
  int ->
  unit Lwt.t
```
```ocaml
val write_from_string_exactly : 
  output_channel ->
  string ->
  int ->
  int ->
  unit Lwt.t
```
See [`write_from_exactly`](./#val-write_from_exactly).

```ocaml
val write_value : 
  output_channel ->
  ?flags:Stdlib.Marshal.extern_flags list ->
  'a ->
  unit Lwt.t
```
`write_value channel ?flags v` writes `v` to `channel` using the `Marshal` module of the standard library. See `Stdlib.Marshal.to_channel` for an explanation of `?flags`.

The corresponding reading function is [`read_value`](./#val-read_value). See warnings about type safety in the description of [`read_value`](./#val-read_value).


### Printing

These functions are basically helpers. Also you may prefer using the name [`printl`](./#val-printl) rather than [`write_line`](./#val-write_line) because it is shorter.

The general name of a printing function is `<prefix>print<suffixes>`,

where `<prefix>` is one of:

- `'f'`, which means that the function takes as argument a channel
- nothing, which means that the function prints on [`stdout`](./#val-stdout)
- `'e'`, which means that the function prints on [`stderr`](./#val-stderr)
and `<suffixes>` is a combination of:

- `'l'` which means that a new-line character is printed after the message
- `'f'` which means that the function takes as argument a **format** instead of a string
```ocaml
val fprint : output_channel -> string -> unit Lwt.t
```
```ocaml
val fprintl : output_channel -> string -> unit Lwt.t
```
```ocaml
val fprintf : 
  output_channel ->
  ('a, unit, string, unit Lwt.t) Stdlib.format4 ->
  'a
```
`%!` does nothing here. To flush the channel, use `Lwt_io.flush channel`.

```ocaml
val fprintlf : 
  output_channel ->
  ('a, unit, string, unit Lwt.t) Stdlib.format4 ->
  'a
```
`%!` does nothing here. To flush the channel, use `Lwt_io.flush channel`.

```ocaml
val print : string -> unit Lwt.t
```
```ocaml
val printl : string -> unit Lwt.t
```
```ocaml
val printf : ('a, unit, string, unit Lwt.t) Stdlib.format4 -> 'a
```
`%!` does nothing here. To flush the channel, use `Lwt_io.(flush stdout)`.

```ocaml
val printlf : ('a, unit, string, unit Lwt.t) Stdlib.format4 -> 'a
```
`%!` does nothing here. To flush the channel, use `Lwt_io.(flush stdout)`.

```ocaml
val eprint : string -> unit Lwt.t
```
```ocaml
val eprintl : string -> unit Lwt.t
```
```ocaml
val eprintf : ('a, unit, string, unit Lwt.t) Stdlib.format4 -> 'a
```
`%!` does nothing here. To flush the channel, use `Lwt_io.(flush stderr)`.

```ocaml
val eprintlf : ('a, unit, string, unit Lwt.t) Stdlib.format4 -> 'a
```
`%!` does nothing here. To flush the channel, use `Lwt_io.(flush stderr)`.


### Utilities

```ocaml
val hexdump_stream : output_channel -> char Lwt_stream.t -> unit Lwt.t
```
`hexdump_stream oc byte_stream` produces the same output as the command `hexdump -C`.

```ocaml
val hexdump : output_channel -> string -> unit Lwt.t
```
`hexdump oc str = hexdump_stream oc (Lwt_stream.of_string str)`


### File utilities

```ocaml
type file_name = string
```
Type of file names

```ocaml
val open_file : 
  ?buffer:Lwt_bytes.t ->
  ?flags:Unix.open_flag list ->
  ?perm:Unix.file_perm ->
  mode:'a mode ->
  file_name ->
  'a channel Lwt.t
```
`Lwt_io.open_file ~mode file` opens the given file, either for reading (with `~mode:Input`) or for writing (with `~mode:Output`). The returned channel provides buffered I/O on the file.

If `~buffer` is supplied, it is used as the I/O buffer.

If `~flags` is supplied, the file is opened with the given flags (see `Unix.open_flag`). Note that `~flags` is used *exactly* as given. For example, opening a file with `~flags` and `~mode:Input` does *not* implicitly add `O_RDONLY`. So, you should include `O_RDONLY` when opening for reading (`~mode:Input`), and `O_WRONLY` when opening for writing (`~mode:Input`). It is also recommended to include `O_NONBLOCK`, unless you are sure that the file cannot be a socket or a named pipe.

The default permissions used for creating new files are `0o666`, i.e. reading and writing are allowed for the file owner, group, and everyone. These default permissions can be overridden by supplying `~perm`.

Note: if opening for writing (`~mode:Output`), and the file already exists, `open_file` truncates (clears) the file by default. If you would like to keep the pre-existing contents of the file, use the `~flags` parameter to pass a custom flags list that does not include `Unix.O_TRUNC`.

raises `Unix.Unix_error` on error.
```ocaml
val with_file : 
  ?buffer:Lwt_bytes.t ->
  ?flags:Unix.open_flag list ->
  ?perm:Unix.file_perm ->
  mode:'a mode ->
  file_name ->
  ('a channel -> 'b Lwt.t) ->
  'b Lwt.t
```
`Lwt_io.with_file ~mode filename f` opens the given using [`Lwt_io.open_file`](./#val-open_file), and passes the resulting channel to `f`. `Lwt_io.with_file` ensures that the channel is closed when the promise returned by `f` resolves, or if `f` raises an exception.

See [`Lwt_io.open_file`](./#val-open_file) for a description of the arguments, warnings, and other notes.

```ocaml
val open_temp_file : 
  ?buffer:Lwt_bytes.t ->
  ?flags:Unix.open_flag list ->
  ?perm:Unix.file_perm ->
  ?temp_dir:string ->
  ?prefix:string ->
  ?suffix:string ->
  unit ->
  (string * output_channel) Lwt.t
```
`open_temp_file ()` starts creating a new temporary file, and evaluates to a promise for the pair of the file's name, and an output channel for writing to the file.

The caller should take care to delete the file later. Alternatively, see [`Lwt_io.with_temp_file`](./#val-with_temp_file).

The `?buffer` and `?perm` arguments are passed directly to an internal call to [`Lwt_io.open_file`](./#val-open_file).

If not specified, `?flags` defaults to `[O_CREATE; O_EXCL; O_WRONLY; O_CLOEXEC]`. If specified, the specified flags are used exactly. Note that these should typically contain at least `O_CREAT` and `O_EXCL`, otherwise `open_temp_file` may open an existing file.

`?temp_dir` can be used to choose the directory in which the file is created. For the current directory, use `Stdlib.Filename.current_dir_name`. If not specified, the directory is taken from `Stdlib.Filename.get_temp_dir_name`, which is typically set to your system temporary file directory.

`?prefix` helps determine the name of the file. It will be the prefix concatenated with a random sequence of characters. If not specified, `open_temp_file` uses some default prefix.

`?suffix` is like `prefix`, but it is appended at the end of the filename. In particular, it can be used to set the extension. This argument is supported since Lwt 4\.4.0.

since 3\.2.0
```ocaml
val with_temp_file : 
  ?buffer:Lwt_bytes.t ->
  ?flags:Unix.open_flag list ->
  ?perm:Unix.file_perm ->
  ?temp_dir:string ->
  ?prefix:string ->
  ?suffix:string ->
  ((string * output_channel) -> 'b Lwt.t) ->
  'b Lwt.t
```
`with_temp_file f` calls [`open_temp_file`](./#val-open_temp_file)` ()`, passing all optional arguments directly to it. It then attaches `f` to run after the file is created, passing the filename and output channel to `f`. When the promise returned by `f` is resolved, `with_temp_file` closes the channel and deletes the temporary file by calling [`Lwt_unix.unlink`](./Lwt_unix.md#val-unlink).

since 3\.2.0
```ocaml
val create_temp_dir : 
  ?perm:Unix.file_perm ->
  ?parent:string ->
  ?prefix:string ->
  ?suffix:string ->
  unit ->
  string Lwt.t
```
Creates a temporary directory, and returns a promise that resolves to its path. The caller must take care to remove the directory. Alternatively, see [`Lwt_io.with_temp_dir`](./#val-with_temp_dir).

If `~perm` is specified, the directory is created with the given permissions. The default permissions are `0755`.

`~parent` is the directory in which the temporary directory is created. If not specified, the default value is the result of `Filename.get_temp_dir_name ()`.

`~prefix` is prepended to the directory name, and `~suffix` is appended to it.

since 4\.4.0
```ocaml
val with_temp_dir : 
  ?perm:Unix.file_perm ->
  ?parent:string ->
  ?prefix:string ->
  ?suffix:string ->
  (string -> 'a Lwt.t) ->
  'a Lwt.t
```
`with_temp_dir f` first calls [`create_temp_dir`](./#val-create_temp_dir), forwarding all optional arguments to it. Once the temporary directory is created at `path`, `with_temp_dir f` calls `f path`. When the promise returned by `f path` is resolved, `with_temp_dir f` recursively deletes the temporary directory and all its contents by calling [`Lwt_io.delete_recursively`](./#val-delete_recursively).

since 4\.4.0
```ocaml
val delete_recursively : string -> unit Lwt.t
```
`delete_recursively path` attempts to delete the directory `path` and all its content recursively.

This is likely VERY slow for directories with many files. That is probably best addressed by switching to blocking calls run inside a worker thread, i.e. with [`Lwt_preemptive`](./Lwt_preemptive.md).

since 5\.7.0
```ocaml
val open_connection : 
  ?fd:Lwt_unix.file_descr ->
  ?set_tcp_nodelay:bool ->
  ?prepare_fd:(Lwt_unix.file_descr -> unit) ->
  ?in_buffer:Lwt_bytes.t ->
  ?out_buffer:Lwt_bytes.t ->
  Unix.sockaddr ->
  (input_channel * output_channel) Lwt.t
```
`open_connection ?fd ?in_buffer ?out_buffer addr` opens a connection to the given address and returns two channels for using it. If `fd` is not specified, a fresh one will be used.

The connection is completely closed when you close both channels.

raises `Unix.Unix_error` on error.
parameter set\_tcp\_nodelay if true, TCP\_NODELAY is set on the socket FD. This avoids a surprising 40ms delay in some situations. It disables Nagle's algorithm (https://en.wikipedia.org/wiki/Nagle%27s\_algorithm). See for example https://brooker.co.za/blog/2024/05/09/nagle.html for why.
The default value is to attempt to set `TCP_NODELAY` but ignore `EOPNOTSUPP`.

parameter prepare\_fd is a custom callback that can be used to modify the socket FD before it is turned into high level channels.
For example passing `~prepare_fd:(fun fd -> Lwt_unix.setsockopt_int fd SO_SNDBUF 65_536)` will set the socket's send buffer's size to 64kiB.

```ocaml
val with_connection : 
  ?fd:Lwt_unix.file_descr ->
  ?set_tcp_nodelay:bool ->
  ?prepare_fd:(Lwt_unix.file_descr -> unit) ->
  ?in_buffer:Lwt_bytes.t ->
  ?out_buffer:Lwt_bytes.t ->
  Unix.sockaddr ->
  ((input_channel * output_channel) -> 'a Lwt.t) ->
  'a Lwt.t
```
`with_connection ?fd ?in_buffer ?out_buffer addr f` opens a connection to the given address and passes the channels to `f`

See [`open_connection`](./#val-open_connection) for more details about `set_tcp_nodelay` and `prepare_fd`.

```ocaml
type server
```
Type of a server

```ocaml
val establish_server_with_client_socket : 
  ?server_fd:Lwt_unix.file_descr ->
  ?backlog:int ->
  ?no_close:bool ->
  ?set_tcp_nodelay:bool ->
  ?prepare_listening_fd:(Lwt_unix.file_descr -> unit) ->
  ?prepare_client_fd:(Lwt_unix.file_descr -> unit) ->
  Unix.sockaddr ->
  (Lwt_unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t) ->
  server Lwt.t
```
`establish_server_with_client_socket listen_address f` creates a server which listens for incoming connections on `listen_address`. When a client makes a new connection, it is passed to `f`: more precisely, the server calls

```ocaml
f client_address client_socket
```
where `client_address` is the address (peer name) of the new client, and `client_socket` is the socket connected to the client.

The server does not block waiting for `f` to complete: it concurrently tries to accept more client connections while `f` is handling the client.

When the promise returned by `f` completes (i.e., `f` is done handling the client), `establish_server_with_client_socket` automatically closes `client_socket`. This is a default behavior that is useful for simple cases, but for a robust application you should explicitly close these channels yourself, and handle any exceptions as appropriate. If the channels are still open when `f` completes, and their automatic closing raises an exception, `establish_server_with_client_socket` treats it as an unhandled exception reaching the top level of the application: it passes that exception to [`Lwt.async_exception_hook`](./Lwt.md#val-async_exception_hook), the default behavior of which is to print the exception and *terminate your process*.

Automatic closing can be completely disabled by passing `~no_close:true`.

Similarly, if `f` raises an exception (or the promise it returns fails with an exception), `establish_server_with_client_socket` can do nothing with that exception, except pass it to [`Lwt.async_exception_hook`](./Lwt.md#val-async_exception_hook).

`~server_fd` can be specified to use an existing file descriptor for listening. Otherwise, a fresh socket is created internally. In either case, `establish_server_with_client_socket` will internally assign `listen_address` to the server socket.

`~backlog` is the argument passed to [`Lwt_unix.listen`](./Lwt_unix.md#val-listen). Its default value is `SOMAXCONN`, which varies by platform and socket kind.

The returned promise (a `server Lwt.t`) resolves when the server has just started listening on `listen_address`: right after the internal call to `listen`, and right before the first internal call to `accept`.

See [`open_connection`](./#val-open_connection) for more details about `set_tcp_nodelay` and `prepare_fd`.

since 4\.1.0
```ocaml
val establish_server_with_client_address : 
  ?fd:Lwt_unix.file_descr ->
  ?buffer_size:int ->
  ?backlog:int ->
  ?no_close:bool ->
  ?set_tcp_nodelay:bool ->
  ?prepare_listening_fd:(Lwt_unix.file_descr -> unit) ->
  ?prepare_client_fd:(Lwt_unix.file_descr -> unit) ->
  Unix.sockaddr ->
  (Lwt_unix.sockaddr -> (input_channel * output_channel) -> unit Lwt.t) ->
  server Lwt.t
```
Like [`Lwt_io.establish_server_with_client_socket`](./#val-establish_server_with_client_socket), but passes two buffered channels to the connection handler `f`. These channels wrap the client socket.

The channels are closed automatically when the promise returned by `f` resolves. To avoid this behavior, pass `~no_close:true`.

See [`open_connection`](./#val-open_connection) for more details about `set_tcp_nodelay` and `prepare_*_fd`.

since 3\.1.0
```ocaml
val shutdown_server : server -> unit Lwt.t
```
Closes the given server's listening socket. The returned promise resolves when the `close(2)` system call completes. This function does not affect the sockets of connections that have already been accepted, i.e. passed to `f` by [`establish_server`](./#val-establish_server).

since 3\.0.0
```ocaml
val lines_of_file : file_name -> string Lwt_stream.t
```
`lines_of_file name` returns a stream of all lines of the file with name `name`. The file is automatically closed when all lines have been read.

```ocaml
val lines_to_file : file_name -> string Lwt_stream.t -> unit Lwt.t
```
`lines_to_file name lines` writes all lines of `lines` to file with name `name`.

```ocaml
val chars_of_file : file_name -> char Lwt_stream.t
```
`chars_of_file name` returns a stream of all characters of the file with name `name`. As for [`lines_of_file`](./#val-lines_of_file) the file is closed when all characters have been read.

```ocaml
val chars_to_file : file_name -> char Lwt_stream.t -> unit Lwt.t
```
`chars_to_file name chars` writes all characters of `chars` to `name`


### Input/output of integers

```ocaml
module type NumberIO = sig ... end
```
Common interface for reading/writing integers in binary

```ocaml
module LE : NumberIO
```
Reading/writing of numbers in little-endian

```ocaml
module BE : NumberIO
```
Reading/writing of numbers in big-endian

Reading/writing of numbers in the system endianness.


#### Reading

```ocaml
val read_int : input_channel -> int Lwt.t
```
Reads a 32-bits integer as an ocaml int

```ocaml
val read_int16 : input_channel -> int Lwt.t
```
```ocaml
val read_int32 : input_channel -> int32 Lwt.t
```
```ocaml
val read_int64 : input_channel -> int64 Lwt.t
```
```ocaml
val read_float32 : input_channel -> float Lwt.t
```
Reads an IEEE single precision floating point value

```ocaml
val read_float64 : input_channel -> float Lwt.t
```
Reads an IEEE double precision floating point value


#### Writing

```ocaml
val write_int : output_channel -> int -> unit Lwt.t
```
Writes an ocaml int as a 32-bits integer

```ocaml
val write_int16 : output_channel -> int -> unit Lwt.t
```
```ocaml
val write_int32 : output_channel -> int32 -> unit Lwt.t
```
```ocaml
val write_int64 : output_channel -> int64 -> unit Lwt.t
```
```ocaml
val write_float32 : output_channel -> float -> unit Lwt.t
```
Writes an IEEE single precision floating point value

```ocaml
val write_float64 : output_channel -> float -> unit Lwt.t
```
Writes an IEEE double precision floating point value

```ocaml
type byte_order = Lwt_sys.byte_order = 
  | Little_endian
  | Big_endian (* Type of byte order *)
```
```ocaml
val system_byte_order : byte_order
```
Same as [`Lwt_sys.byte_order`](./Lwt_sys.md#val-byte_order).


### Low-level access to the internal buffer

```ocaml
val block : 'a channel -> int -> (Lwt_bytes.t -> int -> 'b Lwt.t) -> 'b Lwt.t
```
`block ch size f` pass to `f` the internal buffer and an offset. The buffer contains `size` chars at `offset`. `f` may read or write these chars. `size` must satisfy `0 <= size <= 16`

```ocaml
type direct_access = {
  da_buffer : Lwt_bytes.t; (* The internal buffer *)
  mutable da_ptr : int; (* The pointer to:the beginning of free space for output channelsthe beginning of data for input channels *)
  mutable da_max : int; (* The maximum offset *)
  da_perform : unit -> int Lwt.t; (* for input channels: refills the buffer and returns how many bytes have been readfor output channels: flush partially the buffer and returns how many bytes have been written *)
}
```
Information for directly accessing the internal buffer of a channel

```ocaml
val direct_access : 'a channel -> (direct_access -> 'b Lwt.t) -> 'b Lwt.t
```
`direct_access ch f` passes to `f` a [`direct_access`](./#type-direct_access) structure. `f` must use it and update `da_ptr` to reflect how many bytes have been read/written.


### Misc

```ocaml
val default_buffer_size : unit -> int
```
Return the default size for buffers. Channels that are created without a specific buffer use new buffer of this size.

```ocaml
val set_default_buffer_size : int -> unit
```
Change the default buffer size.

raises `Invalid_argument` if the given size is smaller than 16 or greater than Stdlib.Sys.max\_string\_length

### Deprecated

```ocaml
val establish_server : 
  ?fd:Lwt_unix.file_descr ->
  ?buffer_size:int ->
  ?backlog:int ->
  ?no_close:bool ->
  Unix.sockaddr ->
  ((input_channel * output_channel) -> unit Lwt.t) ->
  server Lwt.t
```
Like `establish_server_with_client_address`, but does not pass the client address or fd to the callback `f`.

deprecated Use establish\_server\_with\_client\_address.
since 3\.0.0
```ocaml
module Versioned : sig ... end
```
Versioned variants of APIs undergoing breaking changes.
