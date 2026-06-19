
# Module `Lwt_unix.LargeFile`

```ocaml
val lseek : file_descr -> int64 -> seek_command -> int64 Lwt.t
```
Wrapper for `Unix.LargeFile.lseek`

```ocaml
val truncate : string -> int64 -> unit Lwt.t
```
Wrapper for `Unix.LargeFile.truncate`

```ocaml
val ftruncate : file_descr -> int64 -> unit Lwt.t
```
Wrapper for `Unix.LargeFile.ftruncate`

```ocaml
type stats = Unix.LargeFile.stats = {
  st_dev : int;
  st_ino : int;
  st_kind : file_kind;
  st_perm : file_perm;
  st_nlink : int;
  st_uid : int;
  st_gid : int;
  st_rdev : int;
  st_size : int64;
  st_atime : float;
  st_mtime : float;
  st_ctime : float;
}
```
```ocaml
val stat : string -> stats Lwt.t
```
Wrapper for `Unix.LargeFile.stat`

```ocaml
val lstat : string -> stats Lwt.t
```
Wrapper for `Unix.LargeFile.lstat`

```ocaml
val fstat : file_descr -> stats Lwt.t
```
Wrapper for `Unix.LargeFile.fstat`

```ocaml
val file_exists : string -> bool Lwt.t
```
`file_exists name` tests if a file named `name` exists.

Note that `file_exists` behaves similarly to `Sys.file_exists`:

- “file” is interpreted as “directory entry” in this context
- `file_exists name` will return `false` in circumstances that would make [`stat`](./#val-stat) raise a `Unix.Unix_error` exception.