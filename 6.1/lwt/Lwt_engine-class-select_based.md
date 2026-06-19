
# Class `Lwt_engine.select_based`

Abstract class for engines based on a select-like function.

```ocaml
inherit t
```
```ocaml
method private virtual select : Unix.file_descr list ->
  Unix.file_descr list ->
  float ->
  Unix.file_descr list * Unix.file_descr list
```
`select fds_r fds_w timeout` waits for either:

- one of the file descriptor of `fds_r` to become readable
- one of the file descriptor of `fds_w` to become writable
- timeout to expire
and returns the list of readable file descriptor and the list of writable file descriptors.
