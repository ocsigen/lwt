
# Module `Lwt_main.Exit_hooks`

Promise-returning hooks, of type `unit -> unit Lwt.t`, that are called at process exit. Exceptions raised by these hooks are ignored.

since 4\.2.0
```ocaml
type 'return_value kind = 'return_value Lwt.t
```
Hooks are functions of either type `unit -> unit` or `unit -> unit Lwt.t`; this type constructor is used only to express both possibilities in one signature.

```ocaml
type hook
```
Values of type `hook` represent hooks that have been added, so that they can be removed later (if needed).

```ocaml
val add_first : (unit -> unit kind) -> hook
```
Adds a hook to the hook sequence underlying this module, to be run *first*, before any other hooks already added.

```ocaml
val add_last : (unit -> unit kind) -> hook
```
Adds a hook to the hook sequence underlying this module, to be run *last*, after any other hooks already added.

```ocaml
val remove : hook -> unit
```
Removes a hook added by [`add_first`](./#val-add_first) or [`add_last`](./#val-add_last).

```ocaml
val remove_all : unit -> unit
```
Removes all hooks from the hook sequence underlying this module.
