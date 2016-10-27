# Lwt &nbsp;&nbsp; [![version 2.6.0][version]][releases] [![BSD license][license-img]][copying] [![Manual][docs-img]][manual] [![Gitter chat][gitter-img]][gitter] [![Travis status][travis-img]][travis] [![AppVeyor status][appveyor-img]][appveyor]

[version]:      https://img.shields.io/badge/version-2.6.0-blue.svg
[releases]:     https://github.com/ocsigen/lwt/releases
[license-img]:  https://img.shields.io/badge/license-LGPL-blue.svg
[gitter-img]:   https://img.shields.io/badge/chat-on_gitter-lightgrey.svg
[docs-img]:     https://img.shields.io/badge/docs-online-lightgrey.svg
[travis]:       https://travis-ci.org/ocsigen/lwt/branches
[travis-img]:   https://img.shields.io/travis/ocsigen/lwt/master.svg?label=travis
[appveyor]:     https://ci.appveyor.com/project/aantron/lwt/branch/master
[appveyor-img]: https://img.shields.io/appveyor/ci/aantron/lwt/master.svg?label=appveyor

Lwt provides *lightweight* (a.k.a. *cooperative* or *green*) threads for OCaml.
Normally-blocking operations can be run concurrently in a single OCaml process,
without managing system threads or locking. Lwt threads are type-disciplined and
composable – Lwt is a *monad*.

Here is a simplistic program which requests the Google search page, and fails
if the request is not completed in five seconds:

```ocaml
let () =
  let request =
    let%lwt addresses = Lwt_unix.getaddrinfo "google.com" "80" [] in
    let server = (List.hd addresses).Lwt_unix.ai_addr in

    Lwt_io.(with_connection server (fun (incoming, outgoing) ->
      let%lwt () = write outgoing "GET / HTTP/1.1\r\n" in
      let%lwt () = write outgoing "Connection: close\r\n\r\n" in
      let%lwt response = read incoming in
      Lwt.return (Some response)))
  in

  let timeout =
    let%lwt () = Lwt_unix.sleep 5. in
    Lwt.return None
  in

  match Lwt_main.run (Lwt.pick [request; timeout]) with
  | Some response -> print_string response
  | None -> prerr_endline "Request timed out"; exit 1
```

The above program can be compiled and run with

```
ocamlfind opt -package lwt.unix -package lwt.ppx -linkpkg -o request example.ml
./request
```

## Installing

```
opam install lwt
```

## Documentation

Documentation can be found [here][manual]. There are also some examples
available in [`doc/examples`][examples].

[manual]:   http://ocsigen.org/lwt/manual/
[examples]: https://github.com/ocsigen/lwt/tree/master/doc/examples

## Contact

Open an [issue][issues], visit [Gitter][gitter] chat, [email][email] the
maintainer, or ask in [#ocaml][irc]. If you think enough people will be
interested in the answer, it is also possible to ask on [Stack Overflow][so].

[issues]: https://github.com/ocsigen/lwt/issues/new
[gitter]: https://gitter.im/ocaml-lwt/Lobby
[email]:  mailto:antonbachin@yahoo.com
[irc]:    http://webchat.freenode.net/?channels=#ocaml
[so]:     http://stackoverflow.com/questions/ask?tags=ocaml,lwt,ocaml-lwt

## Contributing

Lwt is a very mature library, but there is considerable room for improvement.
Contributions are welcome. To clone the source and install a development
version,

```
opam source --dev-repo --pin lwt
```

This will also install the development dependency OASIS.

A list of [project suggestions][projects] and a [roadmap][roadmap] can be found
on the wiki.

[projects]: https://github.com/ocsigen/lwt/wiki/Plan#projects
[roadmap]:  https://github.com/ocsigen/lwt/wiki/Plan#roadmap

## License

Lwt is released under the LGPL, with the OpenSSL linking exception. See
[`COPYING`][copying].

[copying]: https://github.com/ocsigen/lwt/blob/master/doc/COPYING
