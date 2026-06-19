
# Class `Lwt_engine.poll_based`

Abstract class for engines based on a poll-like function.

```ocaml
inherit t
```
```ocaml
method private virtual poll : (Unix.file_descr * bool * bool) list ->
  float ->
  (Unix.file_descr * bool * bool) list
```
`poll fds tiomeout`, where `fds` is a list of tuples of the form `(fd, check_readable, check_writable)`, waits for either:

- one of the file descriptor with `check_readable` set to `true` to become readable
- one of the file descriptor with `check_writable` set to `true` to become writable
- timeout to expire
and returns the list of file descriptors with their readable and writable status.
