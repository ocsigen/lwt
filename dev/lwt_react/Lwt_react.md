
# Module `Lwt_react`

React utilities

This module is an overlay for the `React` module. You can open it instead of the `React` module in order to get all of `React`'s functions plus Lwt ones.

This module is provided by OPAM package `lwt_react`. Link with ocamlfind package `lwt_react`.

```ocaml
type 'a event = 'a React.event
```
Type of events.

```ocaml
type 'a signal = 'a React.signal
```
Type of signals.

```ocaml
module E : sig ... end
```
```ocaml
module S : sig ... end
```