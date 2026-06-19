
# Lwt


## Introduction

Lwt is a concurrent programming library for OCaml. It provides a single data type: the *promise*, which is a value that will become resolved in the future. Creating a promise spawns a computation. When that computation is I/O, Lwt runs it in parallel with your OCaml code.

OCaml code, including creating and waiting on promises, is run in a single thread by default, so you don't have to worry about locking or preemption. You can detach code to be run in separate threads on an opt-in basis.

Here is a simplistic Lwt program which requests the Google front page, and fails if the request is not completed in five seconds:

```ocaml
let () =
  let request =
    let%lwt addresses = Lwt_unix.getaddrinfo "google.com" "80" [] in
    let google = Lwt_unix.((List.hd addresses).ai_addr) in

    Lwt_io.(with_connection google (fun (incoming, outgoing) ->
      write outgoing "GET / HTTP/1.1\r\n";%lwt
      write outgoing "Connection: close\r\n\r\n";%lwt
      let%lwt response = read incoming in
      Lwt.return (Some response)))
  in

  let timeout =
    Lwt_unix.sleep 5.;%lwt
    Lwt.return_none
  in

  match Lwt_main.run (Lwt.pick [request; timeout]) with
  | Some response -> print_string response
  | None -> prerr_endline "Request timed out"; exit 1

(* ocamlfind opt -package lwt.unix,lwt_ppx -linkpkg example.ml && ./a.out *)
```
If you are not using the `lwt_ppx` syntax extension, you can use the `let*` binding opoerators from the `Lwt.Syntax` module instead.

```ocaml
let () =
  let open Lwt.Syntax in
  let request =
    let* addresses = Lwt_unix.getaddrinfo "google.com" "80" [] in
    let google = Lwt_unix.((List.hd addresses).ai_addr) in

    Lwt_io.(with_connection google (fun (incoming, outgoing) ->
      let* () = write outgoing "GET / HTTP/1.1\r\n" in
      let* () = write outgoing "Connection: close\r\n\r\n" in
      let* response = read incoming in
      Lwt.return (Some response)))
  in

  let timeout =
    let* () = Lwt_unix.sleep 5. in
    Lwt.return_none
  in

  match Lwt_main.run (Lwt.pick [request; timeout]) with
  | Some response -> print_string response
  | None -> prerr_endline "Request timed out"; exit 1

(* ocamlfind opt -package lwt.unix,lwt_ppx -linkpkg example.ml && ./a.out *)
```
In the program above, functions such as `Lwt_io.write` create promises. The `let%lwt ... in` construct (provided by `lwt_ppx`) or the `let* ... in` construct (provided by `Lwt.Syntax`) are used to wait for a promise to resolve. The code after `in` is scheduled to run after the code inside the `let...in` has resolved.

`Lwt.pick` races promises against each other, and behaves as the first one to complete.

`Lwt_main.run` forces the whole promise-computation network to be executed. All the visible OCaml code is run in a single thread, but Lwt internally uses a combination of worker threads and non-blocking file descriptors to resolve in parallel the promises that do I/O.


## Tour

Lwt compiles to native code on Linux, macOS, Windows, and other systems. It's also routinely compiled to JavaScript for the front end and Node by js\_of\_ocaml.

In Lwt,

- The core library [`Lwt`](./Lwt.md) provides promises...
- ...and a few pure-OCaml helpers, such as promise-friendly [mutexes](./Lwt_mutex.md), [condition variables](./Lwt_condition.md), and [mvars](./Lwt_mvar.md).
- There is a big Unix binding, [`Lwt_unix`](./Lwt_unix.md), that binds almost every Unix system call. A higher-level module [`Lwt_io`](./Lwt_io.md) provides nice I/O channels.
- [`Lwt_process`](./Lwt_process.md) is for subprocess handling.
- [`Lwt_preemptive`](./Lwt_preemptive.md) spawns system threads.

## Installing

1. Use your system package manager to install a development libev package. It is often called `libev-dev` or `libev-devel`.
2. `opam install conf-libev lwt`

## Additional Docs

- [Manual](./manual.md) ([Online manual](https://ocsigen.org/lwt/)).
- [Concurrent Programming with Lwt](https://github.com/dkim/rwo-lwt#readme) is a nice source of Lwt examples. They are translations of code from Real World OCaml, but are just as useful if you are not reading the book.
- [Mirage Lwt tutorial](https://mirage.io/docs/tutorial-lwt).
- [Example server](https://baturin.org/code/lwt-counter-server/) written with Lwt.

## API: Library `lwt`

This is the system-independent, pure-OCaml core of Lwt. To link with it, use `(libraries lwt)` in your `dune` file.

[`Lwt`](./Lwt.md) Asynchronous programming with promises.
[`Lwt_list`](./Lwt_list.md) List helpers
[`Lwt_stream`](./Lwt_stream.md) Data streams
[`Lwt_result`](./Lwt_result.md) Explicit error handling
[`Lwt_mutex`](./Lwt_mutex.md) Cooperative locks for mutual exclusion
[`Lwt_condition`](./Lwt_condition.md) Conditions
[`Lwt_mvar`](./Lwt_mvar.md) Mailbox variables
[`Lwt_switch`](./Lwt_switch.md) Lwt switches
[`Lwt_pool`](./Lwt_pool.md) External resource pools.

## API: Library `lwt.unix`

This is the system call and I/O library. Despite its name, it is implemented on both Unix-like systems and Windows, although not all functions are available on Windows. To link with this library, use `(libraries lwt.unix)` in your `dune` file.

[`Lwt_unix`](./Lwt_unix.md) Cooperative system calls
[`Lwt_main`](./Lwt_main.md) Main loop and event queue
[`Lwt_io`](./Lwt_io.md) Buffered byte channels
[`Lwt_process`](./Lwt_process.md) Process management
[`Lwt_bytes`](./Lwt_bytes.md) Byte arrays
[`Lwt_preemptive`](./Lwt_preemptive.md) This module allows to mix preemptive threads with Lwt cooperative threads. It maintains an extensible pool of preemptive threads to which you can detach computations.
[`Lwt_fmt`](./Lwt_fmt.md) Format API for Lwt-powered IOs
[`Lwt_throttle`](./Lwt_throttle.md) Rate limiters.
[`Lwt_timeout`](./Lwt_timeout.md) Cancelable timeouts.
[`Lwt_engine`](./Lwt_engine.md) Lwt unix main loop engine
[`Lwt_gc`](./Lwt_gc.md) Interaction with the garbage collector
[`Lwt_sys`](./Lwt_sys.md) System informations.