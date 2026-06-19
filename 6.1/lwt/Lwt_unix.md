
# Module `Lwt_unix`

Cooperative system calls

This modules maps system calls, like those of the standard library's `Unix` module, to cooperative ones, which will not block the program.

The semantics of all operations is the following: if the action (for example reading from a **file descriptor**) can be performed immediately, it is performed and returns an already resolved promise, otherwise it returns a pending promise which is resolved when the operation completes.

Most operations on sockets and pipes (on Windows it is only sockets) are **cancelable**, meaning you can cancel them with [`Lwt.cancel`](./Lwt.md#val-cancel). For example if you want to read something from a **file descriptor** with a timeout, you can cancel the action after the timeout and the reading will not be performed if not already done.

For example, consider that you have two sockets `sock1` and `sock2`. You want to read something from `sock1` or exclusively from `sock2` and fail with an exception if a timeout of 1 second expires, without reading anything from `sock1` and `sock2`, even if they become readable in the future.

Then you can do:

```ocaml
Lwt.pick
  [Lwt_unix.timeout 1.0;
   read sock1 buf1 ofs1 len1;
   read sock2 buf2 ofs2 len2]
```
In this case, it is guaranteed that exactly one of the three operations will complete, and the others will be cancelled.

```ocaml
val handle_unix_error : ('a -> 'b Lwt.t) -> 'a -> 'b Lwt.t
```
Same as `Unix.handle_unix_error` but catches lwt-level exceptions


### Sleeping

```ocaml
val sleep : float -> unit Lwt.t
```
`sleep d` is a promise that remains in a pending state for `d` seconds after which it is resolved with value `()`.

```ocaml
val yield : unit -> unit Lwt.t
```
`yield ()` is a promise in a pending state. It resumes itself as soon as possible and resolves with value `()`.

deprecated Since 5.5.0 yield is deprecated. Use the more general Lwt.pause instead. See Lwt\_main.yield for additional details.
```ocaml
val auto_yield : float -> unit -> unit Lwt.t
```
deprecated Since 5.5.0. Use auto\_pause instead.
```ocaml
val auto_pause : float -> unit -> unit Lwt.t
```
`auto_pause timeout` returns a function `f`, and `f ()` has the following behavior:

- If it has been more than `timeout` seconds since the last time `f ()` behaved like [`Lwt.pause`](./Lwt.md#val-pause), `f ()` calls [`Lwt.pause`](./Lwt.md#val-pause).
- Otherwise, if it has been less than `timeout` seconds, `f ()` behaves like [`Lwt.return_unit`](./Lwt.md#val-return_unit), i.e. it does not yield.
```ocaml
exception Timeout
```
Exception raised by timeout operations

```ocaml
val timeout : float -> 'a Lwt.t
```
`timeout d` is a promise that remains pending for `d` seconds and then is rejected with [`Timeout`](./#exception-Timeout).

raises [`Timeout`](./#exception-Timeout) The promise timeout d is rejected with Timeout unless it is cancelled.
```ocaml
val with_timeout : float -> (unit -> 'a Lwt.t) -> 'a Lwt.t
```
`with_timeout d f` is a short-hand for:

```ocaml
  Lwt.pick [Lwt_unix.timeout d; f ()]
```
raises [`Timeout`](./#exception-Timeout) The promise with\_timeout d f raises Timeout if the promise returned by f () takes more than d seconds to resolve.

### Operations on file-descriptors

```ocaml
type file_descr
```
The abstract type for **file descriptor**s. A Lwt **file descriptor** is a pair of a unix **file descriptor** (of type `Unix.file_descr`) and a **state**.

A **file descriptor** may be:

- **opened**, in which case it is fully usable
- **closed** or **aborted**, in which case it is no longer usable
```ocaml
type state = 
  | Opened (* The file descriptor is opened *)
  | Closed (* The file descriptor has been closed by close. It must not be used for any operation. *)
  | Aborted of exn (* The file descriptor has been aborted, the only operation possible is close, all others will fail. *)
```
State of a **file descriptor**

```ocaml
val state : file_descr -> state
```
`state fd` returns the [`state`](./#type-state) of `fd`.

```ocaml
val unix_file_descr : file_descr -> Unix.file_descr
```
Returns the underlying unix **file descriptor**. It always succeeds, even if the **file descriptor**'s state is not `Opened`.

```ocaml
val of_unix_file_descr : 
  ?blocking:bool ->
  ?set_flags:bool ->
  Unix.file_descr ->
  file_descr
```
Wraps a `Unix` file descriptor `fd` in an Lwt [`file_descr`](./#type-file_descr) `fd'`.

`~blocking` controls the *internal* strategy Lwt uses to perform I/O on the underlying `fd`. Regardless of `~blocking`, at the API level, `Lwt_unix.read`, `Lwt_unix.write`, etc. on `fd'` *always* block the Lwt promise, but *never* block the whole process. However, for performance reasons, it is important that `~blocking` match the actual blocking mode of `fd`.

If `~blocking` is not specified, `of_unix_file_descr` chooses non-blocking mode for Unix sockets, Unix pipes, and Windows sockets, and blocking mode for everything else. **Note:** not specifying `~blocking` causes `fstat` to be lazily called on `fd`, the first time your code performs I/O on `fd'`. This `fstat` call can be expensive, so if you use `of_unix_file_descr` a lot, be sure to specify `~blocking` explicitly.

`of_unix_file_descr` runs a system call to set the specified or chosen blocking mode on the underlying `fd`.

To prevent `of_unix_file_descr` from running this system call, you can pass `~set_flags:false`. Note that, in this case, if `~blocking`, whether passed explicitly or chosen by Lwt, does not match the true blocking mode of the underlying `fd`, I/O on `fd'` will suffer performance degradation.

Note that `~set_flags` is effectively always `false` if running on Windows and `fd` is not a socket.

Generally, non-blocking I/O is faster: for blocking I/O, Lwt typically has to run system calls in worker threads to avoid blocking the process. See your system documentation for whether particular kinds of file descriptors support non-blocking I/O.

```ocaml
val blocking : file_descr -> bool Lwt.t
```
`blocking fd` indicates whether Lwt is internally using blocking or non-blocking I/O with `fd`.

Note that this may differ from the blocking mode of the underlying Unix file descriptor (i.e. `unix_file_descr fd`).

See [`of_unix_file_descr`](./#val-of_unix_file_descr) for details.

```ocaml
val set_blocking : ?set_flags:bool -> file_descr -> bool -> unit
```
`set_blocking fd b` causes Lwt to internally use blocking or non-blocking I/O with `fd`, according to the value of `b`.

If `~set_flags` is `true` (the default), Lwt also makes a system call to set the underlying file descriptor's blocking mode to match. Otherwise, `set_blocking` is only informational for Lwt.

It is important that the underlying file descriptor actually have the same blocking mode as that indicated by `b`.

See [`of_unix_file_descr`](./#val-of_unix_file_descr) for details.

```ocaml
val abort : file_descr -> exn -> unit
```
`abort fd exn` makes all current and further uses of the file descriptor fail with the given exception. This put the **file descriptor** into the `Aborted` state.

If the **file descriptor** is closed, this does nothing, if it is aborted, this replace the abort exception by `exn`.

Note that this only works for reading and writing operations on file descriptors supporting non-blocking mode.


### Process handling

```ocaml
val fork : unit -> int
```
`fork ()` does the same as `Unix.fork`. You must use this function instead of `Unix.fork` when you want to use Lwt in the child process, even if you have not started using Lwt before the fork.

Notes:

- In the child process all pending `Lwt_unix` I/O jobs are abandoned. This may cause the child's copy of their associated promises to remain forever pending.
- If you are going to use Lwt in the parent and the child, it is a good idea to call [`Lwt_io.flush_all`](./Lwt_io.md#val-flush_all) before callling [`fork`](./#val-fork) to avoid double-flush.
- Otherwise, if you will not use Lwt in the child, call [`Lwt_main.Exit_hooks.remove_all`](./Lwt_main-Exit_hooks.md#val-remove_all) to avoid Lwt calling [`Lwt_main.run`](./Lwt_main.md#val-run) during process exit.
- None of the above is necessary if you intend to call `exec`. Indeed, in that case, it is not even necessary to use `Lwt_unix.fork`. You can use `Unix.fork`.
- To abandon some more promises, see [`Lwt.abandon_paused`](./Lwt.md#val-abandon_paused).
Furthermore:

- Calling `Lwt_unix.fork` raises an execption if `Domain.spawn` has been called at any point in the program's past.
- Calling `Lwt_unix.fork` can result in the child process being in a corrupted state if any thread has been started. Lwt starts threads when `Lwt_preemptive.detach` is called. Lwt implicitly starts threads to perform blocking I/O unless the [`async_method`](./#val-async_method) is set to `Async_none`.
```ocaml
type process_status = Unix.process_status = 
  | WEXITED of int
  | WSIGNALED of int
  | WSTOPPED of int
```
```ocaml
type wait_flag = Unix.wait_flag = 
  | WNOHANG
  | WUNTRACED
```
```ocaml
val wait : unit -> (int * process_status) Lwt.t
```
Wrapper for `Unix.wait`

```ocaml
val waitpid : wait_flag list -> int -> (int * process_status) Lwt.t
```
A promise-returning analog to `Unix.waitpid`. This call is non-blocking on Unix-like systems, but is always blocking on Windows.

```ocaml
type resource_usage = {
  ru_utime : float; (* User time used *)
  ru_stime : float; (* System time used *)
}
```
Resource usages

```ocaml
val wait4 : 
  wait_flag list ->
  int ->
  (int * process_status * resource_usage) Lwt.t
```
`wait4 flags pid` returns `(pid, status, rusage)` where `(pid, status)` is the same result as `Unix.waitpid flags pid`, and `rusage` contains accounting information about the child.

On windows it will always returns `{ utime = 0.0; stime = 0.0 }`.

```ocaml
val wait_count : unit -> int
```
Returns the number of promises waiting for a child process to terminate.

```ocaml
val system : string -> process_status Lwt.t
```
Executes the given command, waits until it terminates, and return its termination status. The string is interpreted by the shell `/bin/sh` on Unix and `cmd.exe` on Windows. The result `WEXITED 127` indicates that the shell couldn't be executed.

The function uses [`fork`](./#val-fork) internally. As a result, this function is brittle. See all the warnings relating to `fork` for more details.


### Basic file input/output

```ocaml
val stdin : file_descr
```
The **file descriptor** for standard input.

```ocaml
val stdout : file_descr
```
The **file descriptor** for standard output.

```ocaml
val stderr : file_descr
```
The **file descriptor** for standard error.

```ocaml
type file_perm = Unix.file_perm
```
```ocaml
type open_flag = Unix.open_flag = 
  | O_RDONLY
  | O_WRONLY
  | O_RDWR
  | O_NONBLOCK
  | O_APPEND
  | O_CREAT
  | O_TRUNC
  | O_EXCL
  | O_NOCTTY
  | O_DSYNC
  | O_SYNC
  | O_RSYNC
  | O_SHARE_DELETE
  | O_CLOEXEC
  | O_KEEPEXEC
```
```ocaml
val openfile : string -> open_flag list -> file_perm -> file_descr Lwt.t
```
Wrapper for `Unix.openfile`.

```ocaml
val close : file_descr -> unit Lwt.t
```
Close a **file descriptor**. This close the underlying unix **file descriptor** and set its state to `Closed`.

```ocaml
val read : file_descr -> bytes -> int -> int -> int Lwt.t
```
`read fd buf ofs len` reads up to `len` bytes from `fd`, and writes them to `buf`, starting at offset `ofs`. The function immediately evaluates to an Lwt promise which waits for the operation to complete. If it completes successfully, the promise resolves to the number of bytes actually read, or zero if the end of file has been reached.

Note that the Lwt promise waits for data (or end of file) even if the underlying file descriptor is in non-blocking mode. See [`of_unix_file_descr`](./#val-of_unix_file_descr) for a discussion of non-blocking I/O and Lwt.

If Lwt is using blocking I/O on `fd`, `read` writes data into a temporary buffer, then copies it into `buf`.

The promise can be rejected with any exception that can be raised by `Unix.read`, except `Unix.Unix_error Unix.EAGAIN`, `Unix.Unix_error Unix.EWOULDBLOCK` or `Unix.Unix_error Unix.EINTR`.

```ocaml
val pread : file_descr -> bytes -> file_offset:int -> int -> int -> int Lwt.t
```
`pread fd buf ~file_offset ofs len` on file descriptors allowing seek, reads up to `len` bytes from `fd` at offset `file_offset` from the beginning of the file, and writes them to `buf`, starting at offset `ofs`.

On Unix systems, the file descriptor position is unaffected. On Windows it is changed to be just after the last read position.

The promise can be rejected with any exception that can be raised by `read` or `lseek`.

```ocaml
val write : file_descr -> bytes -> int -> int -> int Lwt.t
```
`write fd buf ofs len` writes up to `len` bytes to `fd` from `buf`, starting at buffer offset `ofs`. The function immediately evaluates to an Lwt promise which waits for the operation to complete. If the operation completes successfully, the promise resolves to the number of bytes actually written, which may be less than `len`.

Note that the Lwt promise waits to write even if the underlying file descriptor is in non-blocking mode. See [`of_unix_file_descr`](./#val-of_unix_file_descr) for a discussion of non-blocking I/O and Lwt.

If Lwt is using blocking I/O on `fd`, `buf` is copied before writing.

The promise can be rejected with any exception that can be raised by `Unix.single_write`, except `Unix.Unix_error Unix.EAGAIN`, `Unix.Unix_error Unix.EWOULDBLOCK` or `Unix.Unix_error Unix.EINTR`.

```ocaml
val pwrite : file_descr -> bytes -> file_offset:int -> int -> int -> int Lwt.t
```
`pwrite fd buf ~file_offset ofs len` on file descriptors allowing seek, writes up to `len` bytes to `fd` from `buf`, starting at buffer offset `ofs`. The data is written at offset `file_offset` from the beginning of `fd`.

On Unix systems, the file descriptor position is unaffected. On Windows it is changed to be just after the last written position.

The promise can be rejected with any exception that can be raised by `write` or `lseek`.

```ocaml
val write_string : file_descr -> string -> int -> int -> int Lwt.t
```
See [`write`](./#val-write).

```ocaml
val pwrite_string : 
  file_descr ->
  string ->
  file_offset:int ->
  int ->
  int ->
  int Lwt.t
```
See [`pwrite`](./#val-pwrite).

```ocaml
module IO_vectors : sig ... end
```
Sequences of buffer slices for [`writev`](./#val-writev).

```ocaml
val readv : file_descr -> IO_vectors.t -> int Lwt.t
```
`readv fd vs` reads bytes from `fd` into the buffer slices `vs`. If the operation completes successfully, the resulting promise resolves to the number of bytes read.

Data is always read directly into `Bigarray` slices. If the Unix file descriptor underlying `fd` is in non-blocking mode, data is also read directly into `bytes` slices. Otherwise, data for `bytes` slices is first read into temporary buffers, then copied.

Note that the returned Lwt promise is pending until failure or a successful read, even if the underlying file descriptor is in non-blocking mode. See [`of_unix_file_descr`](./#val-of_unix_file_descr) for a discussion of non-blocking I/O and Lwt.

If [`IO_vectors.system_limit`](./Lwt_unix-IO_vectors.md#val-system_limit) is `Some n` and the count of slices in `vs` exceeds `n`, then `Lwt_unix.readv` reads only into the first `n` slices of `vs`.

Not implemented on Windows. It should be possible to implement, upon request, for Windows sockets only.

See [`readv(3p)`](https://man7.org/linux/man-pages/man3/readv.3p.html).

since 2\.7.0
```ocaml
val writev : file_descr -> IO_vectors.t -> int Lwt.t
```
`writev fd vs` writes the bytes in the buffer slices `vs` to the file descriptor `fd`. If the operation completes successfully, the resulting promise resolves to the number of bytes written.

If the Unix file descriptor underlying `fd` is in non-blocking mode, `writev` does not make a copy the bytes before writing. Otherwise, it copies `bytes` slices, but not `Bigarray` slices.

Note that the returned Lwt promise is pending until failure or a successful write, even if the underlying descriptor is in non-blocking mode. See [`of_unix_file_descr`](./#val-of_unix_file_descr) for a discussion of non-blocking I/O and Lwt.

If [`IO_vectors.system_limit`](./Lwt_unix-IO_vectors.md#val-system_limit) is `Some n` and the count of slices in `vs` exceeds `n`, then `Lwt_unix.writev` passes only the first `n` slices in `vs` to the underlying `writev` system call.

Not implemented on Windows. It should be possible to implement, upon request, for Windows sockets only.

The behavior of `writev` when `vs` has zero slices depends on the system, and may change in future versions of Lwt. On Linux, `writev` will succeed and write zero bytes. On BSD (including macOS), `writev` will fail with `Unix.Unix_error (Unix.EINVAL, "writev", ...)`.

See [`writev(3p)`](https://man7.org/linux/man-pages/man3/writev.3p.html).

since 2\.7.0
```ocaml
val readable : file_descr -> bool
```
Returns whether the given file descriptor is currently readable.

```ocaml
val writable : file_descr -> bool
```
Returns whether the given file descriptor is currently writable.

```ocaml
val wait_read : file_descr -> unit Lwt.t
```
Waits (without blocking other promises) until there is something to read from the file descriptor.

Note that you don't need to use this function if you are using Lwt I/O functions for reading, since they provide non-blocking waiting automatically.

The intended use case for this function is interfacing with existing libraries that are known to be blocking.

```ocaml
val wait_write : file_descr -> unit Lwt.t
```
Waits (without blocking other promises) until it is possible to write on the file descriptor.

Note that you don't need to use this function if you are using Lwt I/O functions for writing, since they provide non-blocking waiting automatically.

The intended use case for this function is interfacing with existing libraries that are known to be blocking.


### Seeking and truncating

```ocaml
type seek_command = Unix.seek_command = 
  | SEEK_SET
  | SEEK_CUR
  | SEEK_END
```
```ocaml
val lseek : file_descr -> int -> seek_command -> int Lwt.t
```
Wrapper for `Unix.lseek`

```ocaml
val truncate : string -> int -> unit Lwt.t
```
Wrapper for `Unix.truncate`

```ocaml
val ftruncate : file_descr -> int -> unit Lwt.t
```
Wrapper for `Unix.ftruncate`


### Syncing

```ocaml
val fsync : file_descr -> unit Lwt.t
```
Synchronise all data and metadata of the file descriptor with the disk. On Windows it uses `FlushFileBuffers`.

```ocaml
val fdatasync : file_descr -> unit Lwt.t
```
Synchronise all data (but not metadata) of the file descriptor with the disk.

Note that `fdatasync` is not available on Windows and OS X.


### File status

```ocaml
type file_kind = Unix.file_kind = 
  | S_REG
  | S_DIR
  | S_CHR
  | S_BLK
  | S_LNK
  | S_FIFO
  | S_SOCK
```
```ocaml
type stats = Unix.stats = {
  st_dev : int;
  st_ino : int;
  st_kind : file_kind;
  st_perm : file_perm;
  st_nlink : int;
  st_uid : int;
  st_gid : int;
  st_rdev : int;
  st_size : int;
  st_atime : float;
  st_mtime : float;
  st_ctime : float;
}
```
```ocaml
val stat : string -> stats Lwt.t
```
Wrapper for `Unix.stat`

```ocaml
val lstat : string -> stats Lwt.t
```
Wrapper for `Unix.lstat`

```ocaml
val fstat : file_descr -> stats Lwt.t
```
Wrapper for `Unix.fstat`

```ocaml
val file_exists : string -> bool Lwt.t
```
`file_exists name` tests if a file named `name` exists.

Note that `file_exists` behaves similarly to `Sys.file_exists`:

- “file” is interpreted as “directory entry” in this context
- `file_exists name` will return `false` in circumstances that would make [`stat`](./#val-stat) raise a `Unix.Unix_error` exception.
```ocaml
val utimes : string -> float -> float -> unit Lwt.t
```
`utimes path atime mtime` updates the access and modification times of the file at `path`. The access time is set to `atime` and the modification time to `mtime`. To set both to the current time, call `utimes path 0. 0.`.

This function corresponds to `Unix.utimes`. See also [`utimes(3p)`](https://man7.org/linux/man-pages/man3/utimes.3p.html).

since 2\.6.0
```ocaml
val isatty : file_descr -> bool Lwt.t
```
Wrapper for `Unix.isatty`


### File operations on large files

```ocaml
module LargeFile : sig ... end
```

### Operations on file names

```ocaml
val unlink : string -> unit Lwt.t
```
Wrapper for `Unix.unlink`

```ocaml
val rename : string -> string -> unit Lwt.t
```
Wrapper for `Unix.rename`

```ocaml
val link : string -> string -> unit Lwt.t
```
Wrapper for `Unix.link`


### File permissions and ownership

```ocaml
val chmod : string -> file_perm -> unit Lwt.t
```
Wrapper for `Unix.chmod`

```ocaml
val fchmod : file_descr -> file_perm -> unit Lwt.t
```
Wrapper for `Unix.fchmod`

```ocaml
val chown : string -> int -> int -> unit Lwt.t
```
Wrapper for `Unix.chown`

```ocaml
val fchown : file_descr -> int -> int -> unit Lwt.t
```
Wrapper for `Unix.fchown`

```ocaml
type access_permission = Unix.access_permission = 
  | R_OK
  | W_OK
  | X_OK
  | F_OK
```
```ocaml
val access : string -> access_permission list -> unit Lwt.t
```
Wrapper for `Unix.access`


### Operations on file descriptors

```ocaml
val dup : ?cloexec:bool -> file_descr -> file_descr
```
Wrapper for `Unix.dup`

```ocaml
val dup2 : ?cloexec:bool -> file_descr -> file_descr -> unit
```
Wrapper for `Unix.dup2`

```ocaml
val set_close_on_exec : file_descr -> unit
```
Wrapper for `Unix.set_close_on_exec`

```ocaml
val clear_close_on_exec : file_descr -> unit
```
Wrapper for `Unix.clear_close_on_exec`


### Directories

```ocaml
val mkdir : string -> file_perm -> unit Lwt.t
```
Wrapper for `Unix.mkdir`

```ocaml
val rmdir : string -> unit Lwt.t
```
Wrapper for `Unix.rmdir`

```ocaml
val chdir : string -> unit Lwt.t
```
Wrapper for `Unix.chdir`

```ocaml
val getcwd : unit -> string Lwt.t
```
Wrapper for `Unix.getcwd`

since 3\.1.0
```ocaml
val chroot : string -> unit Lwt.t
```
Wrapper for `Unix.chroot`

```ocaml
type dir_handle = Unix.dir_handle
```
```ocaml
val opendir : string -> dir_handle Lwt.t
```
Opens a directory for listing. Directories opened with this function must be explicitly closed with [`closedir`](./#val-closedir). This is a cooperative analog of `Unix.opendir`.

```ocaml
val readdir : dir_handle -> string Lwt.t
```
Reads the next directory entry from the given directory. Special entries such as `.` and `..` are included. If all entries have been read, raises `End_of_file`. This is a cooperative analog of `Unix.readdir`.

```ocaml
val readdir_n : dir_handle -> int -> string array Lwt.t
```
`readdir_n handle count` reads at most `count` entries from the given directory. It is more efficient than calling `readdir` `count` times. If the length of the returned array is smaller than `count`, this means that the end of the directory has been reached.

```ocaml
val rewinddir : dir_handle -> unit Lwt.t
```
Resets the given directory handle, so that directory listing can be restarted. Cooperative analog of `Unix.rewinddir`.

```ocaml
val closedir : dir_handle -> unit Lwt.t
```
Closes a directory handle. Cooperative analog of `Unix.closedir`.

```ocaml
val files_of_directory : string -> string Lwt_stream.t
```
`files_of_directory dir` returns the stream of all files of `dir`.


### Pipes and redirections

```ocaml
val pipe : ?cloexec:bool -> unit -> file_descr * file_descr
```
`pipe ()` creates pipe using `Unix.pipe` and returns two lwt **file descriptor**s created from unix **file\_descriptor**

```ocaml
val pipe_in : ?cloexec:bool -> unit -> file_descr * Unix.file_descr
```
`pipe_in ()` is the same as [`pipe`](./#val-pipe) but maps only the unix **file descriptor** for reading into a lwt one. The second is not put into non-blocking mode. You usually want to use this before forking to receive data from the child process.

```ocaml
val pipe_out : ?cloexec:bool -> unit -> Unix.file_descr * file_descr
```
`pipe_out ()` is the inverse of [`pipe_in`](./#val-pipe_in). You usually want to use this before forking to send data to the child process

```ocaml
val mkfifo : string -> file_perm -> unit Lwt.t
```
Wrapper for `Unix.mkfifo`


### Symbolic links

```ocaml
val symlink : ?to_dir:bool -> string -> string -> unit Lwt.t
```
Wrapper for `Unix.symlink`

```ocaml
val readlink : string -> string Lwt.t
```
Wrapper for `Unix.readlink`


### Locking

```ocaml
type lock_command = Unix.lock_command = 
  | F_ULOCK
  | F_LOCK
  | F_TLOCK
  | F_TEST
  | F_RLOCK
  | F_TRLOCK
```
```ocaml
val lockf : file_descr -> lock_command -> int -> unit Lwt.t
```
Wrapper for `Unix.lockf`


### User id, group id

```ocaml
type passwd_entry = Unix.passwd_entry = {
  pw_name : string;
  pw_passwd : string;
  pw_uid : int;
  pw_gid : int;
  pw_gecos : string;
  pw_dir : string;
  pw_shell : string;
}
```
```ocaml
type group_entry = Unix.group_entry = {
  gr_name : string;
  gr_passwd : string;
  gr_gid : int;
  gr_mem : string array;
}
```
```ocaml
val getlogin : unit -> string Lwt.t
```
Wrapper for `Unix.getlogin`

```ocaml
val getpwnam : string -> passwd_entry Lwt.t
```
Wrapper for `Unix.getpwnam`

```ocaml
val getgrnam : string -> group_entry Lwt.t
```
Wrapper for `Unix.getgrnam`

```ocaml
val getpwuid : int -> passwd_entry Lwt.t
```
Wrapper for `Unix.getpwuid`

```ocaml
val getgrgid : int -> group_entry Lwt.t
```
Wrapper for `Unix.getgrgid`


### Signals

```ocaml
type signal_handler_id
```
Id of a signal handler, used to cancel it

```ocaml
val on_signal : int -> (int -> unit) -> signal_handler_id
```
`on_signal signum f` calls `f` each time the signal with numnber `signum` is received by the process. It returns a signal handler identifier that can be used to stop monitoring `signum`.

```ocaml
val on_signal_full : 
  int ->
  (signal_handler_id -> int -> unit) ->
  signal_handler_id
```
`on_signal_full f` is the same as `on_signal f` except that `f` also receive the signal handler identifier as argument so it can disable it.

```ocaml
val disable_signal_handler : signal_handler_id -> unit
```
Stops receiving this signal

```ocaml
val signal_count : unit -> int
```
Returns the number of registered signal handler.

```ocaml
val reinstall_signal_handler : int -> unit
```
`reinstall_signal_handler signum` if any signal handler is registered for this signal with [`on_signal`](./#val-on_signal), it reinstall the signal handler (with `Sys.set_signal`). This is useful in case another part of the program install another signal handler.

```ocaml
val handle_signal : int -> unit
```
`handle_signal signum` acts as if Lwt had received the `signum` signal. This allows another IO library to install the handler, perform its own handling, but still notify Lwt. It is particularly useful for SIGCHLD, where several IO libraries may be spawning sub-processes.

This function is thread-safe.


### Sockets

```ocaml
type inet_addr = Unix.inet_addr
```
```ocaml
type socket_domain = Unix.socket_domain = 
  | PF_UNIX
  | PF_INET
  | PF_INET6
```
```ocaml
type socket_type = Unix.socket_type = 
  | SOCK_STREAM
  | SOCK_DGRAM
  | SOCK_RAW
  | SOCK_SEQPACKET
```
```ocaml
type sockaddr = Unix.sockaddr = 
  | ADDR_UNIX of string
  | ADDR_INET of inet_addr * int
```
```ocaml
val socket : ?cloexec:bool -> socket_domain -> socket_type -> int -> file_descr
```
`socket domain type proto` is the same as `Unix.socket` but maps the result into a lwt **file descriptor**

```ocaml
val socketpair : 
  ?cloexec:bool ->
  socket_domain ->
  socket_type ->
  int ->
  file_descr * file_descr
```
Wrapper for `Unix.socketpair`

```ocaml
val bind : file_descr -> sockaddr -> unit Lwt.t
```
Binds an address to the given socket. This is the cooperative analog of `Unix.bind`. See also [`bind(3p)`](https://man7.org/linux/man-pages/man3/bind.3p.html).

since 3\.0.0
```ocaml
val listen : file_descr -> int -> unit
```
Wrapper for `Unix.listen`

```ocaml
val accept : ?cloexec:bool -> file_descr -> (file_descr * sockaddr) Lwt.t
```
Wrapper for `Unix.accept`

```ocaml
val accept_n : 
  ?cloexec:bool ->
  file_descr ->
  int ->
  ((file_descr * sockaddr) list * exn option) Lwt.t
```
`accept_n fd count` accepts up to `count` connections at one time.

- if no connection is available right now, it returns a pending promise
- if more than 1 and less than `count` are available, it returns all of them
- if more than `count` are available, it returns the next `count` of them
- if an error happens, it returns the connections that have been successfully accepted so far and the error
`accept_n` has the advantage of improving performance. If you want a more detailed description, you can have a look at:

[Acceptable strategies for improving web server performance](https://dl.acm.org/doi/10.5555/1247415.1247435)

```ocaml
val connect : file_descr -> sockaddr -> unit Lwt.t
```
Wrapper for `Unix.connect`

```ocaml
type shutdown_command = Unix.shutdown_command = 
  | SHUTDOWN_RECEIVE
  | SHUTDOWN_SEND
  | SHUTDOWN_ALL
```
```ocaml
val shutdown : file_descr -> shutdown_command -> unit
```
Wrapper for `Unix.shutdown`

```ocaml
val getsockname : file_descr -> sockaddr
```
Wrapper for `Unix.getsockname`

```ocaml
val getpeername : file_descr -> sockaddr
```
Wrapper for `Unix.getpeername`

```ocaml
type msg_flag = Unix.msg_flag = 
  | MSG_OOB
  | MSG_DONTROUTE
  | MSG_PEEK
```
```ocaml
val recv : file_descr -> bytes -> int -> int -> msg_flag list -> int Lwt.t
```
Wrapper for `Unix.recv`.

On Windows, `recv` writes data into a temporary buffer, then copies it into the given one.

```ocaml
val recvfrom : 
  file_descr ->
  bytes ->
  int ->
  int ->
  msg_flag list ->
  (int * sockaddr) Lwt.t
```
Wrapper for `Unix.recvfrom`.

On Windows, `recvfrom` writes data into a temporary buffer, then copies it into the given one.

```ocaml
val send : file_descr -> bytes -> int -> int -> msg_flag list -> int Lwt.t
```
Wrapper for `Unix.send`.

On Windows, `send` copies the given buffer before writing.

```ocaml
val sendto : 
  file_descr ->
  bytes ->
  int ->
  int ->
  msg_flag list ->
  sockaddr ->
  int Lwt.t
```
Wrapper for `Unix.sendto`.

On Windows, `sendto` copies the given buffer before writing.

```ocaml
val recv_msg : 
  socket:file_descr ->
  io_vectors:IO_vectors.t ->
  (int * Unix.file_descr list) Lwt.t
```
`recv_msg ~socket ~io_vectors` receives data into a list of io-vectors, plus any file-descriptors that may accompany the messages. It returns a tuple whose first field is the number of bytes received and second is a list of received file descriptors. The messages themselves will be recorded in the provided `io_vectors` list. Data is written directly into the `iov_buffer` buffers.

Not implemented on Windows.

since 5\.0.0
```ocaml
val send_msg : 
  socket:file_descr ->
  io_vectors:IO_vectors.t ->
  fds:Unix.file_descr list ->
  int Lwt.t
```
`send_msg ~socket ~io_vectors ~fds` sends data from a list of io-vectors, accompanied with a list of file-descriptors. It returns the number of bytes sent. If fd-passing is not possible on the current system and `fds` is not empty, it raises `Lwt_sys.Not_available "fd_passing"`. Data is written directly from the `io_vectors` buffers.

Not implemented on Windows.

since 5\.0.0
```ocaml
val send_msgto : 
  socket:file_descr ->
  io_vectors:IO_vectors.t ->
  fds:Unix.file_descr list ->
  dest:Unix.sockaddr ->
  int Lwt.t
```
`send_msgto ~socket ~io_vectors ~fds ~dest` is similar to `send_msg` but takes an additional `dest` argument to set the address when using a connection-less socket.

Not implemented on Windows.

since 5\.4.0
```ocaml
type credentials = {
  cred_pid : int;
  cred_uid : int;
  cred_gid : int;
}
```
```ocaml
val get_credentials : file_descr -> credentials
```
`get_credentials fd` returns credentials information from the given socket. On some platforms, obtaining the peer pid is not possible and it will be set to `-1`. If obtaining credentials is not possible on the current system, it raises `Lwt_sys.Not_available "get_credentials"`.

This call is not available on windows.


#### Socket options

```ocaml
type socket_bool_option = Unix.socket_bool_option = 
  | SO_DEBUG
  | SO_BROADCAST
  | SO_REUSEADDR
  | SO_KEEPALIVE
  | SO_DONTROUTE
  | SO_OOBINLINE
  | SO_ACCEPTCONN
  | TCP_NODELAY
  | IPV6_ONLY
  | SO_REUSEPORT
```
```ocaml
type socket_int_option = Unix.socket_int_option = 
  | SO_SNDBUF
  | SO_RCVBUF
  | SO_ERROR
  | SO_TYPE
  | SO_RCVLOWAT
  | SO_SNDLOWAT
```
```ocaml
type socket_optint_option = Unix.socket_optint_option = 
  | SO_LINGER
```
```ocaml
type socket_float_option = Unix.socket_float_option = 
  | SO_RCVTIMEO
  | SO_SNDTIMEO (* Note: these options are provided for the sake of completeness only. Lwt places all sockets in non-blocking mode, for which these options are meaningless. Use Lwt.pick with Lwt_unix.sleep or Lwt_unix.timeout for timeouts. *)
```
```ocaml
val getsockopt : file_descr -> socket_bool_option -> bool
```
Wrapper for `Unix.getsockopt`

```ocaml
val setsockopt : file_descr -> socket_bool_option -> bool -> unit
```
Wrapper for `Unix.setsockopt`

```ocaml
val getsockopt_int : file_descr -> socket_int_option -> int
```
Wrapper for `Unix.getsockopt_int`

```ocaml
val setsockopt_int : file_descr -> socket_int_option -> int -> unit
```
Wrapper for `Unix.setsockopt_int`

```ocaml
val getsockopt_optint : file_descr -> socket_optint_option -> int option
```
Wrapper for `Unix.getsockopt_optint`

```ocaml
val setsockopt_optint : 
  file_descr ->
  socket_optint_option ->
  int option ->
  unit
```
Wrapper for `Unix.setsockopt_optint`

```ocaml
val getsockopt_float : file_descr -> socket_float_option -> float
```
Wrapper for `Unix.getsockopt_float`

```ocaml
val setsockopt_float : file_descr -> socket_float_option -> float -> unit
```
Wrapper for `Unix.setsockopt_float`

```ocaml
val getsockopt_error : file_descr -> Unix.error option
```
Wrapper for `Unix.getsockopt_error`


#### Multicast functions

```ocaml
val mcast_set_loop : file_descr -> bool -> unit
```
Whether sent multicast messages are received by the sending host

```ocaml
val mcast_set_ttl : file_descr -> int -> unit
```
Set TTL/hops value

```ocaml
val mcast_add_membership : 
  file_descr ->
  ?ifname:Unix.inet_addr ->
  Unix.inet_addr ->
  unit
```
`mcast_add_membership fd ~ifname addr` joins the multicast group `addr` on the network interface `ifname`.

```ocaml
val mcast_drop_membership : 
  file_descr ->
  ?ifname:Unix.inet_addr ->
  Unix.inet_addr ->
  unit
```
`mcast_drop_membership fd ~ifname addr` leaves the multicast group `addr` on the network interface `ifname`.


### Host and protocol databases

```ocaml
type host_entry = Unix.host_entry = {
  h_name : string;
  h_aliases : string array;
  h_addrtype : socket_domain;
  h_addr_list : inet_addr array;
}
```
```ocaml
type protocol_entry = Unix.protocol_entry = {
  p_name : string;
  p_aliases : string array;
  p_proto : int;
}
```
```ocaml
type service_entry = Unix.service_entry = {
  s_name : string;
  s_aliases : string array;
  s_port : int;
  s_proto : string;
}
```
```ocaml
val gethostname : unit -> string Lwt.t
```
Wrapper for `Unix.gethostname`

```ocaml
val gethostbyname : string -> host_entry Lwt.t
```
Wrapper for `Unix.gethostbyname`

```ocaml
val gethostbyaddr : inet_addr -> host_entry Lwt.t
```
Wrapper for `Unix.gethostbyaddr`

```ocaml
val getprotobyname : string -> protocol_entry Lwt.t
```
Wrapper for `Unix.getprotobyname`

```ocaml
val getprotobynumber : int -> protocol_entry Lwt.t
```
Wrapper for `Unix.getprotobynumber`

```ocaml
val getservbyname : string -> string -> service_entry Lwt.t
```
Wrapper for `Unix.getservbyname`

```ocaml
val getservbyport : int -> string -> service_entry Lwt.t
```
Wrapper for `Unix.getservbyport`

```ocaml
type addr_info = Unix.addr_info = {
  ai_family : socket_domain;
  ai_socktype : socket_type;
  ai_protocol : int;
  ai_addr : sockaddr;
  ai_canonname : string;
}
```
```ocaml
type getaddrinfo_option = Unix.getaddrinfo_option = 
  | AI_FAMILY of socket_domain
  | AI_SOCKTYPE of socket_type
  | AI_PROTOCOL of int
  | AI_NUMERICHOST
  | AI_CANONNAME
  | AI_PASSIVE
```
```ocaml
val getaddrinfo : 
  string ->
  string ->
  getaddrinfo_option list ->
  addr_info list Lwt.t
```
Wrapper for `Unix.getaddrinfo`

```ocaml
type name_info = Unix.name_info = {
  ni_hostname : string;
  ni_service : string;
}
```
```ocaml
type getnameinfo_option = Unix.getnameinfo_option = 
  | NI_NOFQDN
  | NI_NUMERICHOST
  | NI_NAMEREQD
  | NI_NUMERICSERV
  | NI_DGRAM
```
```ocaml
val getnameinfo : sockaddr -> getnameinfo_option list -> name_info Lwt.t
```
Wrapper for `Unix.getnameinfo`


### Terminal interface

```ocaml
type terminal_io = Unix.terminal_io = {
  mutable c_ignbrk : bool;
  mutable c_brkint : bool;
  mutable c_ignpar : bool;
  mutable c_parmrk : bool;
  mutable c_inpck : bool;
  mutable c_istrip : bool;
  mutable c_inlcr : bool;
  mutable c_igncr : bool;
  mutable c_icrnl : bool;
  mutable c_ixon : bool;
  mutable c_ixoff : bool;
  mutable c_opost : bool;
  mutable c_obaud : int;
  mutable c_ibaud : int;
  mutable c_csize : int;
  mutable c_cstopb : int;
  mutable c_cread : bool;
  mutable c_parenb : bool;
  mutable c_parodd : bool;
  mutable c_hupcl : bool;
  mutable c_clocal : bool;
  mutable c_isig : bool;
  mutable c_icanon : bool;
  mutable c_noflsh : bool;
  mutable c_echo : bool;
  mutable c_echoe : bool;
  mutable c_echok : bool;
  mutable c_echonl : bool;
  mutable c_vintr : char;
  mutable c_vquit : char;
  mutable c_verase : char;
  mutable c_vkill : char;
  mutable c_veof : char;
  mutable c_veol : char;
  mutable c_vmin : int;
  mutable c_vtime : int;
  mutable c_vstart : char;
  mutable c_vstop : char;
}
```
```ocaml
val tcgetattr : file_descr -> terminal_io Lwt.t
```
Wrapper for `Unix.tcgetattr`

```ocaml
type setattr_when = Unix.setattr_when = 
  | TCSANOW
  | TCSADRAIN
  | TCSAFLUSH
```
```ocaml
val tcsetattr : file_descr -> setattr_when -> terminal_io -> unit Lwt.t
```
Wrapper for `Unix.tcsetattr`

```ocaml
val tcsendbreak : file_descr -> int -> unit Lwt.t
```
Wrapper for `Unix.tcsendbreak`

```ocaml
val tcdrain : file_descr -> unit Lwt.t
```
Wrapper for `Unix.tcdrain`

```ocaml
type flush_queue = Unix.flush_queue = 
  | TCIFLUSH
  | TCOFLUSH
  | TCIOFLUSH
```
```ocaml
val tcflush : file_descr -> flush_queue -> unit Lwt.t
```
Wrapper for `Unix.tcflush`

```ocaml
type flow_action = Unix.flow_action = 
  | TCOOFF
  | TCOON
  | TCIOFF
  | TCION
```
```ocaml
val tcflow : file_descr -> flow_action -> unit Lwt.t
```
Wrapper for `Unix.tcflow`


### Configuration

```ocaml
type async_method = 
  | Async_none (* System calls are made synchronously, and may block the entire program.The main use cases for this are:debugging (execution is simpler)working with fork and exec (which are not thread-safe)when calling specific blocking I/O which is known to be fast *)
  | Async_detach (* System calls are made in another system thread, thus without blocking other Lwt promises. The drawback is that it may degrade performance in some cases.This is the default. *)
```
For system calls that cannot be made asynchronously, Lwt uses one of the following method:

```ocaml
val default_async_method : unit -> async_method
```
Returns the default async method.

This can be initialized using the environment variable `"LWT_ASYNC_METHOD"` with possible values `"none"` and `"detach"`.

```ocaml
val set_default_async_method : async_method -> unit
```
Sets the default async method.

```ocaml
val async_method : unit -> async_method
```
`async_method ()` returns the async method used in the current thread.

```ocaml
val async_method_key : async_method Lwt.key
```
The key for storing the local async method.

```ocaml
val with_async_none : (unit -> 'a) -> 'a
```
`with_async_none f` is a shorthand for:

```ocaml
  Lwt.with_value async_method_key (Some Async_none) f
```
```ocaml
val with_async_detach : (unit -> 'a) -> 'a
```
`with_async_detach f` is a shorthand for:

```ocaml
  Lwt.with_value async_method_key (Some Async_detach) f
```

### Low-level interaction

```ocaml
exception Retry
```
If an action raises [`Retry`](./#exception-Retry), it will be requeued until the **file descriptor** becomes readable/writable again.

```ocaml
exception Retry_read
```
If an action raises [`Retry_read`](./#exception-Retry_read), it will be requeued until the **file descriptor** becomes readable.

```ocaml
exception Retry_write
```
If an action raises [`Retry_read`](./#exception-Retry_read), it will be requeued until the **file descriptor** becomes writables.

```ocaml
type io_event = 
  | Read
  | Write
```
```ocaml
val wrap_syscall : io_event -> file_descr -> (unit -> 'a) -> 'a Lwt.t
```
`wrap_syscall set fd action` wrap an action on a **file descriptor**. It tries to execute action, and if it can not be performed immediately without blocking, it is registered for later.

In the latter case, if the promise is canceled, `action` is removed from `set`.

```ocaml
val check_descriptor : file_descr -> unit
```
`check_descriptor fd` raise an exception if `fd` is not in the state `Open`.

```ocaml
val register_action : io_event -> file_descr -> (unit -> 'a) -> 'a Lwt.t
```
`register_action set fd action` registers `action` on `fd`. When `fd` becomes `readable`/`writable` `action` is called.

Note:

- you must call `check_descriptor fd` before calling `register_action`
- you should prefer using [`wrap_syscall`](./#val-wrap_syscall)
```ocaml
type 'a job
```
Type of job descriptions. A job description describe how to call a C function and how to get its result. The C function may be executed in another system thread.

```ocaml
val run_job : ?async_method:async_method -> 'a job -> 'a Lwt.t
```
`run_job ?async_method job` starts `job` and wait for its termination.

The `~async_method` argument will be ignored in Lwt 5\.0.0, and this function will always act as if `~async_method:Async_detach` is passed.

The async method is chosen follow:

- if the optional parameter `async_method` is specified, it is used,
- otherwise if the local key [`async_method_key`](./#val-async_method_key) is set in the current thread, it is used,
- otherwise the default method (returned by [`default_async_method`](./#val-default_async_method)) is used.
If the method is `Async_none` then the job is run synchronously and may block the current system thread, thus blocking all Lwt threads.

If the method is `Async_detach` then the job is run in another system thread, unless the the maximum number of worker threads has been reached (as given by [`pool_size`](./#val-pool_size)).

If the method is `Async_switch` then the job is run synchronously and if it blocks, execution will continue in another system thread (unless the limit is reached).

```ocaml
val abort_jobs : exn -> unit
```
`abort_jobs exn` make all pending jobs to fail with exn. Note that this does not abort the real job (i.e. the C function executing it), just the lwt thread for it.

```ocaml
val cancel_jobs : unit -> unit
```
`cancel_jobs ()` is the same as `abort_jobs Lwt.Canceled`.

```ocaml
val wait_for_jobs : unit -> unit Lwt.t
```
Wait for all pending jobs to terminate.


### Notifications

Lwt internally use a pipe to send notification to the main thread. The following functions allow to use this pipe.

```ocaml
type notification
```
```ocaml
val make_notification : ?once:bool -> (unit -> unit) -> notification
```
`make_notification ?once f` registers a new notifier. It returns the id of the notifier. Each time a notification with this id is received, `f` is called.

if `once` is specified, then the notification is stopped after the first time it is received. It defaults to `false`.

```ocaml
val send_notification : notification -> unit
```
`send_notification id` sends a notification.

This function is thread-safe.

```ocaml
val stop_notification : notification -> unit
```
Stop the given notification. Note that you should not reuse the id after the notification has been stopped, the result is unspecified if you do so.

```ocaml
val call_notification : notification -> unit
```
Call the handler associated to the given notification. Note that if the notification was defined with `once = true` it is removed.

```ocaml
val set_notification : notification -> (unit -> unit) -> unit
```
`set_notification id f` replace the function associated to the notification by `f`. It raises `Not_found` if the given notification is not found.


### System threads pool

If the program is using the async method `Async_detach` or `Async_switch`, Lwt will launch system threads to execute blocking system calls asynchronously.

```ocaml
val pool_size : unit -> int
```
Maximum number of system threads that can be started. If this limit is reached, jobs will be executed synchronously.

```ocaml
val set_pool_size : int -> unit
```
Change the size of the pool.

```ocaml
val thread_count : unit -> int
```
The number of system threads running (excluding this one).

```ocaml
val thread_waiting_count : unit -> int
```
The number threads waiting for a job.


### CPUs

```ocaml
val get_cpu : unit -> int
```
`get_cpu ()` returns the number of the CPU the current thread is running on.

```ocaml
val get_affinity : ?pid:int -> unit -> int list
```
`get_affinity ?pid ()` returns the list of CPUs the process with pid `pid` is allowed to run on. If `pid` is not specified then the affinity of the current process is returned.

```ocaml
val set_affinity : ?pid:int -> int list -> unit
```
`set_affinity ?pid cpus` sets the list of CPUs the given process is allowed to run on.


### Versioned interfaces

```ocaml
module Versioned : sig ... end
```
Versioned variants of APIs undergoing breaking changes.
