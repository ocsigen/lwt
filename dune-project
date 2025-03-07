(lang dune 2.7)

(name lwt)

(generate_opam_files true)

(maintainers
 "Raphaël Proust <code@bnwr.net>"
 "Anton Bachin <antonbachin@yahoo.com>")
(authors "Jérôme Vouillon" "Jérémie Dimino")
(license MIT)
(source (github ocsigen/lwt))
(documentation "https://ocsigen.org/lwt")

(package
 (name lwt_ppx_let)
 (synopsis "Dummy package context for ppx_let tests")
 (description "Internal package used to partition ppx_let tests.")
 (depends
  (ocaml (>= 4.08))
  (ppx_let :with-test)
  lwt))

(package
 (name lwt_ppx)
 (version 5.8.1)
 (synopsis "PPX syntax for Lwt, providing something similar to async/await from JavaScript")
 (depends
  (ocaml (>= 4.08))
  (ppxlib (and (>= 0.16.0) (< 0.36)))
  lwt))

(package
 (name lwt_react)
 (synopsis "Helpers for using React with Lwt")
 (depends
  (ocaml (>= 4.08))
  (cppo (and :build (>= 1.1.0)))
  (lwt (>= 3.0.0))
  (react (>= 1.0.0))))

(package
 (name lwt)
 (version 5.8.1)
 (synopsis "Promises and event-driven I/O")
 (description "A promise is a value that may become determined in the future.

Lwt provides typed, composable promises. Promises that are resolved by I/O are
resolved by Lwt in parallel.

Meanwhile, OCaml code, including code creating and waiting on promises, runs in
a single thread by default. This reduces the need for locks or other
synchronization primitives. Code can be run in parallel on an opt-in basis.
")
 (depends
  (ocaml (>= 4.08))
  (cppo (and :build (>= 1.1.0)))
  (ocamlfind (and :dev (>= 1.7.3-1)))
  (odoc (and :with-doc (>= 2.3.0)))
  dune-configurator
  ocplib-endian)
 (depopts base-threads base-unix conf-libev))
