# Module `Lwt_main.Exit_hooks`

Promise-returning hooks, of type `unit -> unit Lwt.t`, that are called at process exit. Exceptions raised by these hooks are ignored.

The hooks are per domain. Those registered on a domain other than the main one run when that domain exits. At process exit, the hooks that run are those of the domain that called `Stdlib.exit`, or of the main domain when the program returns. An `exit` called from another domain does not run the main domain's hooks, so call `Stdlib.exit` from the main domain. The default [`Lwt.async_exception_hook`](./Lwt.md#val-async_exception_hook) calls `exit` on whichever domain the exception occurs on, so replace it when running Lwt on several domains.

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
