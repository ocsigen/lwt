
# Module `Lwt_process`

Process management

This module allows you to spawn processes and communicate with them.

This module makes heavy use of [`Lwt_unix.fork`](./Lwt_unix.md#val-fork). Important caveats are documented there. Read them. TL;DR: no domains, no threads, no preemptive, yes `Async_none`.

```ocaml
type command = string * string array
```
A command. The first field is the name of the executable and the second is the list of arguments. For example:

```ocaml
  ("ls", [|"ls"; "-l"|])
```
Notes:

- if the name is the empty string, then the first argument will be used. You should specify a name only if you do not want the executable to be searched in the PATH. On Windows the only way to enable automatic search in PATH is to pass an empty name.
- it is possible to \``inline'' an argument, i.e. split it into multiple arguments. To do that prefix it with `"\000"`. For example:
```ocaml
  ("", [|"echo"; "\000foo bar"|])
```
is the same as:

```ocaml
  ("", [|"echo"; "foo"; "bar"|])
```
```ocaml
val shell : string -> command
```
A command executed with the shell. (with `"/bin/sh -c <cmd>"` on Unix and `"cmd.exe /c <cmd>"` on Windows).

All the following functions take an optional argument `timeout`, in seconds. If specified, after expiration, the process will be sent a `Unix.sigkill` signal and channels will be closed. When the channels are closed, any pending I/O operations on them (such as [`Lwt_io.read_chars`](./Lwt_io.md#val-read_chars)) fail with exception [`Lwt_io.Channel_closed`](./Lwt_io.md#exception-Channel_closed).


### High-level functions


#### Redirections

```ocaml
type redirection = [ 
  | `Keep (* Point to the same file as in the parent. *)
  | `Dev_null (* Redirect to /dev/null (POSIX) or nul (Win32). *)
  | `Close (* Close the file descriptor. *)
  | `FD_copy of Unix.file_descr (* Redirect to the file pointed to by fd. fd remains open in the parent. *)
  | `FD_move of Unix.file_descr (* Redirect to the file pointed to by fd. fd is then closed in the parent. *)
 ]
```
File descriptor redirections. These are used with the `~stdin`, `~stdout`, and `~stderr` arguments below to specify how the standard file descriptors should be redirected in the child process. All optional redirection arguments default to ``Keep`.


#### Executing

```ocaml
val exec : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdin:redirection ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  command ->
  Unix.process_status Lwt.t
```
Executes the given command and returns its exit status.


#### Receiving

```ocaml
val pread : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdin:redirection ->
  ?stderr:redirection ->
  command ->
  string Lwt.t
```
```ocaml
val pread_chars : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdin:redirection ->
  ?stderr:redirection ->
  command ->
  char Lwt_stream.t
```
```ocaml
val pread_line : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdin:redirection ->
  ?stderr:redirection ->
  command ->
  string Lwt.t
```
```ocaml
val pread_lines : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdin:redirection ->
  ?stderr:redirection ->
  command ->
  string Lwt_stream.t
```

#### Sending

```ocaml
val pwrite : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  command ->
  string ->
  unit Lwt.t
```
```ocaml
val pwrite_chars : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  command ->
  char Lwt_stream.t ->
  unit Lwt.t
```
```ocaml
val pwrite_line : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  command ->
  string ->
  unit Lwt.t
```
```ocaml
val pwrite_lines : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  command ->
  string Lwt_stream.t ->
  unit Lwt.t
```

#### Mapping

```ocaml
val pmap : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stderr:redirection ->
  command ->
  string ->
  string Lwt.t
```
```ocaml
val pmap_chars : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stderr:redirection ->
  command ->
  char Lwt_stream.t ->
  char Lwt_stream.t
```
```ocaml
val pmap_line : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stderr:redirection ->
  command ->
  string ->
  string Lwt.t
```
```ocaml
val pmap_lines : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stderr:redirection ->
  command ->
  string Lwt_stream.t ->
  string Lwt_stream.t
```

### Spawning processes

```ocaml
type state = 
  | Running (* The process is still running *)
  | Exited of Unix.process_status (* The process has exited *)
```
State of a sub-process

```ocaml
class process_none : ?timeout:float -> ?env:string array -> ?cwd:string -> ?stdin:redirection -> ?stdout:
  redirection -> ?stderr:redirection -> command -> object ... end
```
```ocaml
val open_process_none : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdin:redirection ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  command ->
  process_none
```
```ocaml
val with_process_none : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdin:redirection ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  command ->
  (process_none -> 'a Lwt.t) ->
  'a Lwt.t
```
```ocaml
class process_in : ?timeout:float -> ?env:string array -> ?cwd:string -> ?stdin:redirection -> ?stderr:
  redirection -> command -> object ... end
```
```ocaml
val open_process_in : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdin:redirection ->
  ?stderr:redirection ->
  command ->
  process_in
```
```ocaml
val with_process_in : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdin:redirection ->
  ?stderr:redirection ->
  command ->
  (process_in -> 'a Lwt.t) ->
  'a Lwt.t
```
```ocaml
class process_out : ?timeout:float -> ?env:string array -> ?cwd:string -> ?stdout:redirection -> ?stderr:
  redirection -> command -> object ... end
```
```ocaml
val open_process_out : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  command ->
  process_out
```
```ocaml
val with_process_out : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stdout:redirection ->
  ?stderr:redirection ->
  command ->
  (process_out -> 'a Lwt.t) ->
  'a Lwt.t
```
```ocaml
class process : ?timeout:float -> ?env:string array -> ?cwd:string -> ?stderr:redirection -> 
  command -> object ... end
```
```ocaml
val open_process : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stderr:redirection ->
  command ->
  process
```
```ocaml
val with_process : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  ?stderr:redirection ->
  command ->
  (process -> 'a Lwt.t) ->
  'a Lwt.t
```
```ocaml
class process_full : ?timeout:float -> ?env:string array -> ?cwd:string -> command -> object ... end
```
```ocaml
val open_process_full : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  command ->
  process_full
```
```ocaml
val with_process_full : 
  ?timeout:float ->
  ?env:string array ->
  ?cwd:string ->
  command ->
  (process_full -> 'a Lwt.t) ->
  'a Lwt.t
```