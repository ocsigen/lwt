
# Module `Lwt_fmt`

Format API for Lwt-powered IOs

since 4\.1.0
This module bridges the gap between `Stdlib.Format` and [`Lwt`](./Lwt.md). Although it is not required, it is recommended to use this module with the [`Fmt`](https://erratique.ch/software/fmt) library.

Compared to regular formatting function, the main difference is that printing statements will now return promises instead of blocking.

```ocaml
val printf : 
  ('a, Stdlib.Format.formatter, unit, unit Lwt.t) Stdlib.format4 ->
  'a
```
Returns a promise that prints on the standard output. Similar to `Stdlib.Format.printf`.

```ocaml
val eprintf : 
  ('a, Stdlib.Format.formatter, unit, unit Lwt.t) Stdlib.format4 ->
  'a
```
Returns a promise that prints on the standard error. Similar to `Stdlib.Format.eprintf`.


## Formatters

```ocaml
type formatter
```
Lwt enabled formatters

```ocaml
type order = 
  | String of string * int * int (* String (s, off, len) indicate the output of s at offset off and length len. *)
  | Flush (* Flush operation *)
```
```ocaml
val make_stream : unit -> order Lwt_stream.t * formatter
```
`make_stream ()` returns a formatter and a stream of all the writing order given on that stream.

```ocaml
val of_channel : Lwt_io.output_channel -> formatter
```
`of_channel oc` creates a formatter that writes to the channel `oc`.

```ocaml
val stdout : formatter
```
Formatter printing on [`Lwt_io.stdout`](./Lwt_io.md#val-stdout).

```ocaml
val stderr : formatter
```
Formatter printing on [`Lwt_io.stdout`](./Lwt_io.md#val-stdout).

```ocaml
val make_formatter : 
  commit:(unit -> unit Lwt.t) ->
  fmt:Stdlib.Format.formatter ->
  unit ->
  formatter
```
`make_formatter ~commit ~fmt` creates a new lwt formatter based on the `Stdlib.Format.formatter` `fmt`. The `commit` function will be called by the printing functions to update the underlying channel.

```ocaml
val get_formatter : formatter -> Stdlib.Format.formatter
```
`get_formatter fmt` returns the underlying `Stdlib.Format.formatter`. To access the underlying formatter during printing, it is recommended to use `%t` and `%a`.


### Printing

```ocaml
val fprintf : 
  formatter ->
  ('a, Stdlib.Format.formatter, unit, unit Lwt.t) Stdlib.format4 ->
  'a
```
```ocaml
val kfprintf : 
  (formatter -> unit Lwt.t -> 'a) ->
  formatter ->
  ('b, Stdlib.Format.formatter, unit, 'a) Stdlib.format4 ->
  'b
```
```ocaml
val ifprintf : 
  formatter ->
  ('a, Stdlib.Format.formatter, unit, unit Lwt.t) Stdlib.format4 ->
  'a
```
```ocaml
val ikfprintf : 
  (formatter -> unit Lwt.t -> 'a) ->
  formatter ->
  ('b, Stdlib.Format.formatter, unit, 'a) Stdlib.format4 ->
  'b
```
```ocaml
val flush : formatter -> unit Lwt.t
```
`flush fmt` flushes the formatter (as with `Stdlib.Format.pp_print_flush`) and executes all the printing action on the underlying channel.

Low level functions

```ocaml
val write_order : Lwt_io.output_channel -> order -> unit Lwt.t
```
`write_order oc o` applies the order `o` on the channel `oc`.

```ocaml
val write_pending : formatter -> unit Lwt.t
```
Write all the pending orders of a formatter. Warning: This function flush neither the internal format queues nor the underlying channel and is intended for low level use only. You should probably use [`flush`](./#val-flush) instead.
