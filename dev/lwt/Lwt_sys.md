
# Module `Lwt_sys`

System informations.

```ocaml
exception Not_available of string
```
`Not_available(feature)` is an exception that may be raised when a feature is not available on the current system.

```ocaml
type feature = [ 
  | `wait4
  | `get_cpu
  | `get_affinity
  | `set_affinity
  | `recv_msg
  | `send_msg
  | `fd_passing
  | `get_credentials
  | `mincore
  | `madvise
  | `fdatasync
  | `libev
 ]
```
Features that can be tested.

```ocaml
val have : feature -> bool
```
Test whether the given feature is available on the current system.

```ocaml
type byte_order = 
  | Little_endian
  | Big_endian (* Type of byte order *)
```
```ocaml
val byte_order : byte_order
```
The byte order used by the computer running the program.

```ocaml
val windows : bool
```
deprecated Use Sys.win32.