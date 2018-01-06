# Contributing to the Lwt code

Contributing to Lwt doesn't only mean writing code! Asking questions, fixing
docs, etc., are all valuable contributions. For notes on contributing in
general, see [Contributing][contributing] in the Lwt `README`. This file
contains extra information for working on code specifically.

This file is meant to be an aid, not a hindrance. If you think you already
have a good idea of what to do, go ahead and work without reading this :)


<br/>

#### Table of contents

- [General](#General)
- [OPAM+git workflow](#Workflow)
  - [Getting the code](#Checkout)
  - [Testing](#Testing)
  - [Testing with coverage analysis](#Test_with_coverage_analysis)
  - [Getting your change merged](#Getting_your_change_merged)
  - [Making additional changes](#Making_additional_changes)
  - [Cleaning up](#Cleaning_up)
- [Internal documentation](#Documentation)
- [Code overview](#Code_overview)


<br/>

<a id="General"></a>
## General

1. If you get stuck, or have any question, please [ask][contact]!

2. If you start working, but then life interferes and you don't want to
   continue, there is no problem in stopping. This can be for any reason
   whatsoever, and you don't have to tell anyone what that reason is. Lwt
   respects your time and your needs.

3. If a maintainer is trying your patience (hopefully by accident) by making you
   fix too many nits, do excessive history rewriting, or something else like
   that, please let them know! Lwt doesn't want to tire you out!

4. To find something to work on, you can look at the [easy issues][easy]. If
   those don't look interesting, some [medium issues][medium] are
   self-contained. If you [contact][contact] the maintainers, they may be able
   to suggest a few. Otherwise, you are welcome to work on anything at all.

5. If you begin working on an issue, it's good to leave a comment on it to claim
   it. This prevents multiple people from doing the same work.

[contact]: https://github.com/ocsigen/lwt#contact
[contributing]: https://github.com/ocsigen/lwt#contributing
[easy]: https://github.com/ocsigen/lwt/labels/easy
[medium]: https://github.com/ocsigen/lwt/labels/medium


<br/>

<a id="Workflow"></a>
## OPAM+git workflow

<a id="Checkout"></a>
#### Getting the code

To get started, fork the Lwt repo by clicking on the "Fork" button at the very
top of this page. You will now have a repository at
`https://github.com/your-user-name/lwt`. Let's clone it to your machine:

```
git clone https://github.com/your-user-name/lwt.git
cd lwt/
```

We'll use Lwt's own [`opam`][opam-depends] file to install Lwt's dependencies
automatically. Before doing that, you may want to switch to a special OPAM
switch just for Lwt:

```
opam switch 4.04.02-lwt --alias-of 4.04.2   # optional
eval `opam config env`                      # optional
opam pin add --no-action lwt .
opam install --deps-only lwt
```

[opam-depends]: https://github.com/ocsigen/lwt/blob/8bff603ae6d976e69698fa08e8ce08fe9615489d/opam/opam#L35-L44

On most systems, you should also [install libev][installing]:

```
your-package-manager install libev-devel
opam install conf-libev
```

[installing]: https://github.com/ocsigen/lwt#installing

Now, check out a new branch, and make your changes:

```
git checkout -b my-awesome-change
```

<a id="Testing"></a>
#### Testing

Each time you are ready to test, run

```
make test
```

If you want to test your development branch using another OPAM package that
depends on Lwt, install your development copy of Lwt with:

```
opam install lwt
```

If you make further changes, you can install your updated code with:

```
opam upgrade lwt
```

Since Lwt is pinned, these commands will install Lwt from your modified code.
All installed OPAM packages that depend on Lwt will be rebuilt against your
modified code when you run these commands.

<a id="Testing_with_coverage_analysis"></a>
#### Testing with coverage analysis

Coverage analysis is only enabled for `src/core/*` for now. To generate coverage
reports, first pin Bisect_ppx to its development version:

```
opam pin add --dev-repo bisect_ppx
```

Then run

```
make coverage
```

in the Lwt repo. To view the coverage report, open `_coverage/index.html` in
your browser.

<a id="Getting_your_change_merged"></a>
#### Getting your change merged

When you are ready, commit your change:

```
git commit
```

You can see examples of commit messages in the Git log; run `git log`. Now,
upload your commit(s) to your fork:

```
git push -u origin my-awesome-change
```

Go to the GitHub web interface for your Lwt fork
(`https://github.com/your-user-name/lwt`), and click on the New Pull Request
button. Follow the instructions, and open the pull request.

This will trigger automatic building and testing of your change on many versions
of OCaml, and several operating systems, in [Travis][travis-ci] and
[AppVeyor][appveyor-ci]. You can even a submit a preliminary PR just to trigger
these tests – just say in the description that it's not ready for review!

At about the same time, a (hopefully!) friendly maintainer will review your
change and start a conversation with you. Ultimately, this will result in a
merged PR and a "thank you!" :smiley: You'll be immortalized in the history,
mentioned in the changelog, and you will have helped a bunch of users have an
easier time with Lwt.

Finally, take a nice break :) This process can be a lot!

<a id="Making_additional_changes"></a>
#### Making additional changes

If additional changes are needed after you open the PR, make them in your branch
locally, commit them, and run:

```
git push
```

This will push the changes to your fork, and GitHub will automatically update
the PR.

#### Tidy history

In some cases, you may be asked to rebase or squash your PR for a cleaner
history (it's normal). If that happens, you will need to run some combination of
`git rebase master`, `git rebase -i master`, and/or `git cherry-pick`. There
isn't really enough space to explain these commands here, but:

- We encourage you to find examples and documentation for them online.
- You can always ask a maintainer for help using them.
- You can always ask a maintainer to do it for you (and we will usually offer).
  We can tell you what commands we ran and why.

Afterwards, `git push -f` will force the new history into the PR.

If we do this rewriting, it is usually at the very end, right before merging the
PR. This is to avoid interfering with reviewers while they are still reviewing
it.

<a id="Cleaning_up"></a>
#### Cleaning up

If you don't want a development Lwt installed in your OPAM switch anymore, you
can do

```
opam pin remove lwt
```

However, if you need your change for your purposes, you may want to keep your
development copy pinned until the next Lwt release.

[travis-ci]: https://travis-ci.org/ocsigen/lwt
[appveyor-ci]: https://ci.appveyor.com/project/aantron/lwt


<br/>

<a id="Documentation"></a>
## Internal documentation

Lwt internal documentation is currently pretty sparse, but we are working on
fixing that.

- The bulk of documentation is still the [manual][manual].
- The [internals of the Lwt core][lwt.ml] are well-documented.
- Working on the Unix binding (`Lwt_unix`, `Lwt_bytes`, etc.) sometimes requires
  writing C code. To make this easier, we have thoroughly
  [documented `Lwt_unix.getcwd`][unix-model] as a model function.
- Everything else is sparsely documented in comments.

[manual]: https://ocsigen.org/lwt/manual/
[lwt.ml]: https://github.com/ocsigen/lwt/blob/master/src/core/lwt.ml
[unix-model]: https://github.com/ocsigen/lwt/blob/99d1ec8b5c159456855eb2f55ddab77207bc92b3/src/unix/unix_c/unix_getcwd_job.c#L36


<br/>

<a id="Code_overview"></a>
## Code overview

Lwt is separated into several layers and sub-libraries, grouped by directory.
This list surveys them, roughly in order of importance.

- `src/core/` is the "core" library. It is written in pure OCaml, so it is
  portable across all systems and to JavaScript.

  The major file here is `src/core/lwt.ml`, which implements the main type,
  `'a Lwt.t`. Also here are some pure-OCaml data structures and synchronization
  primitives. Most of the modules besides `Lwt` are relatively trivial – the
  only exception to this is `Lwt_stream`.

  The code in `src/core/` doesn't know how to do I/O – that is system specific.
  On Unix (including Windows), I/O is provided by the Unix binding (see below).
  On js_of_ocaml, it is provided by `Lwt_js`, a module distributed with
  js_of_ocaml.

- `src/ppx/` is the Lwt PPX. It is also portable, but separated into its own
  little code base, as it is an optional separate library.

- `src/unix/` is the Unix binding, i.e. `Lwt_unix`, `Lwt_io`, `Lwt_main`, some
  other related modules, and a bunch of C code. This is what actually does I/O,
  maintains a worker thread pool, etc. Obviously, this is not portable to
  JavaScript. It supports Unix and Windows. We want to write a future pair of
  Node.js and Unix/Windows bindings, so that code using them is portable, even
  if two separate sets of bindings are required.

  Some of the C code in the Unix binding is generated by
  `src/unix/gen_stubs.ml`.

- `src/preemptive/` provides `Lwt_preemptive` if the threaded runtime is
  available. This is almost always the case when compiling to native code (i.e.,
  when the Unix binding is available).

- `src/logger/` provides `Lwt_log`. This library is separated into two portions,
  `Lwt_log_core` is portable, as the Lwt core is, and `Lwt_log` proper assumes
  a Unix system. `Lwt_log` is in the Unix binding.

- `src/react/` provides the separate library `Lwt_react`. This is basically an
   independent project that lives in the Lwt repo.

- `src/util/` contains various scripts, such as the configure script
  `src/util/discover.ml`, Travis and AppVeyor scripts, etc.

- *deprecated* `src/camlp4/` contains the Camlp4 syntax extension, which is
  deprecated in favor of the PPX (in `src/ppx`).

- *deprecated* `src/simple_top/` contains the Lwt top-level, which is deprecated
  in favor of [utop][utop].

[utop]: https://github.com/diml/utop
