
# Module `Lwt.Exception_filter`

```ocaml
type t
```
An `Exception_filter.t` is a value which indicates to Lwt what exceptions to catch and what exceptions to let bubble up all the way out of the main loop immediately.

```ocaml
val handle_all : t
```
`handle_all` is the default filter. With it the all the exceptions (including `Out_of_memory` and `Stack_overflow`) can be handled: caught and transformed into rejected promises.

```ocaml
val handle_all_except_runtime : t
```
`handle_all_except_runtime` is a filter which lets the OCaml runtime exceptions (`Out_of_memory` and `Stack_overflow`) go through all the Lwt abstractions and bubble all the way out of the call to `Lwt_main.run`.

Note that if you set this handler, then the runtime exceptions leave the Lwt internal state inconsistent. For this reason, you will not be able to call `Lwt_main.run` again after such an exception has escaped `Lwt_main.run`.

```ocaml
val set : t -> unit
```
`set` sets the given exception filter globally. You should call this function at most once during the start of your program, before the first call to `Lwt_main.run`.
