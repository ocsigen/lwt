(lang dune 3.15)

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
 (name lwt_retry)
 (synopsis "Utilities for retrying Lwt computations")
 (authors "Shon Feder")
 (maintainers
   "Raphaël Proust <code@bnwr.net>"
   "Shon Feder <shon.feder@gmail.com>")
 (depends
  (ocaml (>= 4.08))
  (lwt (>= 5.3))))

(package
 (name lwt_ppx)
 (version 5.9.2)
 (authors "Gabriel Radanne")
 (synopsis "PPX syntax for Lwt, providing something similar to async/await from JavaScript")
 (depends
  (ocaml (>= 4.08))
  (ppxlib (>= 0.36))
  (lwt (>= 5.7))))

(package
 (name lwt_ppx__ppx_let_tests)
 (synopsis "DO NOT RELEASE! only for testing let_ppx")
 (license NOTFORRELEASE) ;; trap for opam-repo ci
 (allow_empty)
 (depends
  (ocaml (>= 5.1))
  (ppx_let (and :with-test (>= v0.17.1)))))

(package
 (name lwt_react)
 (synopsis "Helpers for using React with Lwt")
 (depends
  (ocaml (>= 4.08))
  (cppo (and :build (>= 1.1)))
  (lwt (>= 3.0))
  (react (>= 1.0))))

(package
 (name lwt)
 (version 5.9.2)
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
  (cppo (and :build (>= 1.1)))
  (ocamlfind (and :dev (>= 1.7.3-1)))
  (odoc (and :with-doc (>= 2.3)))
  dune-configurator
  ocplib-endian)
 (depopts base-threads base-unix conf-libev))
