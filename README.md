# Lwt &nbsp;&nbsp; [![version 3.1.0][version]][releases] [![LGPL][license-img]][copying] [![Gitter chat][gitter-img]][gitter] [![Travis status][travis-img]][travis] [![AppVeyor status][appveyor-img]][appveyor]

[version]:      https://img.shields.io/badge/version-3.1.0-blue.svg
[releases]:     https://github.com/ocsigen/lwt/releases
[license-img]:  https://img.shields.io/badge/license-LGPL-blue.svg
[gitter-img]:   https://img.shields.io/badge/chat-on_gitter-lightgrey.svg
[travis]:       https://travis-ci.org/ocsigen/lwt/branches
[travis-img]:   https://img.shields.io/travis/ocsigen/lwt/master.svg?label=travis
[appveyor]:     https://ci.appveyor.com/project/aantron/lwt/branch/master
[appveyor-img]: https://img.shields.io/appveyor/ci/aantron/lwt/master.svg?label=appveyor

Lwt is OCaml's concurrent programming library. It provides a single data type:
the *promise*, which is a value that will become determined in the future.
Creating a promise spawns a computation. When that computation is I/O, Lwt runs
it in parallel with your OCaml code.

OCaml code, including creating and waiting on promises, is run in a single
thread by default, so you don't have to worry about locking or preemption. You
can detach code to be run in separate threads on an opt-in basis.

Here is a simplistic Lwt program which requests the Google front page, and fails
if the request is not completed in five seconds:

```ocaml
let () =
  let request =
    let%lwt addresses = Lwt_unix.getaddrinfo "google.com" "80" [] in
    let google = (List.hd addresses).Lwt_unix.ai_addr in

    Lwt_io.(with_connection google (fun (incoming, outgoing) ->
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

(* ocamlfind opt -package lwt.unix -package lwt.ppx -linkpkg -o request example.ml
   ./request *)
```

In the program, functions such as `Lwt_io.write` create promises. The
`let%lwt ... in` construct is used to wait for a promise to become determined;
the code after `in` is scheduled to run in a "callback." `Lwt.pick` races
promises against each other, and behaves as the first one to complete.
`Lwt_main.run` forces the whole promise-computation network to be executed. All
the visible OCaml code is run in a single thread, but Lwt internally uses a
combination of worker threads and non-blocking file descriptors to resolve in
parallel the promises that do I/O.


<br/>

## Installing

1. Use your system package manager to install a development libev package,
   often called `libev-dev` or `libev-devel`.
2. `opam install conf-libev lwt`


<br/>

## Documentation

We are currently working on improving the Lwt documentation (drastically; we are
rewriting the manual). In the meantime:

- The current manual can be found [here][manual].
- Mirage has a nicely-written [Lwt tutorial][mirage-tutorial].
- An example of a [simple server][counter-server] written in Lwt.
- [Concurrent Programming with Lwt][rwo-lwt] is a great source of Lwt examples.
  They are translations of code from the excellent Real World OCaml, but are
  just as useful if you are not reading the book.
- Some examples are also available in Lwt's [`doc/examples`][examples].

*Note: much of the current manual refers to `'a Lwt.t` as "lightweight threads"
or just "threads." This will be fixed in the new manual. `'a Lwt.t` is a
promise, and has nothing to do with system or preemptive threads.*

[manual]:   http://ocsigen.org/lwt/manual/
[examples]: https://github.com/ocsigen/lwt/tree/master/doc/examples/unix
[rwo-lwt]:  https://github.com/dkim/rwo-lwt#readme
[mirage-tutorial]: https://mirage.io/wiki/tutorial-lwt
[counter-server]: http://www.baturin.org/code/lwt-counter-server/


<br/>

## Contact

Open an [issue][issues], visit [Gitter][gitter] chat, ask in [#ocaml][irc],
on [discuss.ocaml.org][discourse], or on [Stack Overflow][so]. Please do ask!
Even apparently simple questions often end up educating other users, not to
mention enlightening the maintainers!

Subscribe to the [announcements issue][announcements] to get news about Lwt
releases. It is less noisy than watching the whole repository. Announcements are
also made in [/r/ocaml][reddit], on the [OCaml mailing list][caml-list], and on
[discuss.ocaml.org][discourse].

[gitter]: https://gitter.im/ocaml-lwt/Lobby
[irc]:    http://webchat.freenode.net/?channels=#ocaml
[so]:     http://stackoverflow.com/questions/ask?tags=ocaml,lwt,ocaml-lwt
[announcements]: https://github.com/ocsigen/lwt/issues/309
[reddit]: https://www.reddit.com/r/ocaml/
[caml-list]: https://sympa.inria.fr/sympa/arc/caml-list
[discourse]: https://discuss.ocaml.org/c/lwt
[issues]: https://github.com/ocsigen/lwt/issues/new


<br/>

## Contributing

What counts as a contribution to Lwt? All kinds of things make the project
better, and are very much appreciated:

- [Asking](#contact) anything. This helps everyone understand Lwt, including
  long-time maintainers!
- Making or requesting edits to the [docs](#documentation), or just reading
  them.
- Reading any [issue or PR][issues-and-prs], and, optionally, adding your
  opinion or requesting clarification.
- Explaining how to make Lwt easier to contribute to, finding problems with the
  [contributing docs][contributing-md], etc.
- Helping other people with Lwt, whether in this repo, or elsewhere in the
  world.
- Writing or clarifying [test cases][tests].
- And, of course, the traditional kind of contribution, picking up
  [issues][all-issues] and writing code :)

Contributing to Lwt is not only for OCaml "experts!" If you are near the
beginning of your OCaml journey, we'd love to give you a little help by
recommending appropriate issues, or even just chatting about Lwt or OCaml.
Newcomers make valuable contributions, that maintainers often learn from – not
the least because newcomers bring a fresh, valuable perspective :) Don't be
afraid to ask anything.

We hope you'll join us to work in a friendly community around Lwt :) On behalf
of all users of, and contributors to, Lwt: Thank you! :tada:

#### Resources

There are several resources to help you get started:

- If you'd like to ask a question, or otherwise talk, there is the
  [contact](#contact) information.
- Lwt maintains a list of [easy issues][easy-issues], which you can use to try
  out the code contribution workflow. This list works two ways! Please
  contribute to it: if you find something that needs a fix, open an issue. It
  might be an easy issue that another contributor would love to solve :)
- [`CONTRIBUTING.md`][contributing-md] contains optional tips for working on the
  code of Lwt, instructions on how to check the code out, and a high-level
  outline of the code base.
- The project [roadmap][roadmap] contains a list of long-term, large-scale
  projects, so you can get an idea of where Lwt is headed, as a whole. Planned
  upcoming releases are also listed there.
- Watch this repository :)

[issues-and-prs]: https://github.com/ocsigen/lwt/issues?utf8=%E2%9C%93&q=is%3Aopen
[all-issues]: https://github.com/ocsigen/lwt/issues
[roadmap]:  https://github.com/ocsigen/lwt/wiki/Roadmap
[easy-issues]: https://github.com/ocsigen/lwt/labels/easy
[contributing-md]: https://github.com/ocsigen/lwt/blob/master/doc/CONTRIBUTING.md#readme
[tests]: https://github.com/ocsigen/lwt/tree/master/test


<br/>

## License

Lwt is released under the LGPL, with an OpenSSL linking exception. See
[`COPYING`][copying].

[copying]: https://github.com/ocsigen/lwt/blob/master/doc/COPYING
