
# Module `Lwt_direct.Storage`

Local storage.

This storage is the same as the one described with [`Lwt.key`](./../lwt/Lwt.md#type-key), except that it is usable from the inside of [`spawn`](./Lwt_direct.md#val-spawn) or [`spawn_in_the_background`](./Lwt_direct.md#val-spawn_in_the_background).

Each task has its own storage, independent from other tasks or promises.

NOTE: it is recommended to use `Lwt_direct.Storage` functions rather than `Lwt.key` functions from [`Lwt`](./../lwt/Lwt.md). The latter is deprecated.

```ocaml
type 'a key = 'a Lwt.key
```
```ocaml
val new_key : unit -> 'a key
```
Alias to [`Lwt.new_key`](./../lwt/Lwt.md#val-new_key)

```ocaml
val get : 'a key -> 'a option
```
get the value associated with this key in local storage, or `None`

```ocaml
val set : 'a key -> 'a -> unit
```
`set k v` sets the key to the value for the rest of the task.

```ocaml
val remove : 'a key -> unit
```
Remove the value associated with this key, if any
