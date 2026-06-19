
# Module type `Lwt_io.NumberIO`

Common interface for reading/writing integers in binary


#### Reading

```ocaml
val read_int : input_channel -> int Lwt.t
```
Reads a 32-bits integer as an ocaml int

```ocaml
val read_int16 : input_channel -> int Lwt.t
```
```ocaml
val read_int32 : input_channel -> int32 Lwt.t
```
```ocaml
val read_int64 : input_channel -> int64 Lwt.t
```
```ocaml
val read_float32 : input_channel -> float Lwt.t
```
Reads an IEEE single precision floating point value

```ocaml
val read_float64 : input_channel -> float Lwt.t
```
Reads an IEEE double precision floating point value


#### Writing

```ocaml
val write_int : output_channel -> int -> unit Lwt.t
```
Writes an ocaml int as a 32-bits integer

```ocaml
val write_int16 : output_channel -> int -> unit Lwt.t
```
```ocaml
val write_int32 : output_channel -> int32 -> unit Lwt.t
```
```ocaml
val write_int64 : output_channel -> int64 -> unit Lwt.t
```
```ocaml
val write_float32 : output_channel -> float -> unit Lwt.t
```
Writes an IEEE single precision floating point value

```ocaml
val write_float64 : output_channel -> float -> unit Lwt.t
```
Writes an IEEE double precision floating point value
