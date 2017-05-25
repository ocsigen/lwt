A few errors found by randomly installing some lwt packages.

All seem to point to libraries using lwt.unix but only linking to lwt.

# irmin_watcher

```
# File "src/irmin_watcher_polling.ml", line 94, characters 28-42:
# Error: Unbound module Lwt_unix
# Command exited with code 2.
```

# biocaml

```
# + ocamlfind ocamlc -annot -bin-annot -c -g -I lib -I lib/lwt -o lib/lwt/future_lwt.cmi -short-paths -thread -w A-4-33-41-42-44-45-48 -package camlzip,cfstream,core_kernel,lwt,lwt.ppx,ppx_compare,ppx_sexp_conv,re.perl,uri,xmlm lib/lwt/future_lwt.mli
# findlib: [WARNING] Interface topdirs.cmi occurs in several directories: /home/andyman/.opam/4.04.0/lib/ocaml/compiler-libs, /home/andyman/.opam/4.04.0/lib/ocaml
# File "lib/lwt/future_lwt.mli", line 7, characters 22-42:
# Error: Unbound module Lwt_io
```

# mirage-profile

```
# File "lwt/dns_resolver_unix.ml", line 35, characters 2-10:
# Error: Unbound module Lwt_unix
# Command exited with code 2.
```

# dns

```
# File "lwt/dns_resolver_unix.ml", line 35, characters 2-10:
# Error: Unbound module Lwt_unix
```

# markup

```
# + ocamlfind ocamlc -c -g -w +A-4-9-44-45-48 -warn-error +A-3 -bin-annot -safe-string -package lwt -package uutf -I src -I doc -I test -I test/performance -I test/dependency -I test/js_of_ocaml -I test/pages -o src/markup_lwt_unix.cmi src/markup_lwt_unix.mli
# File "src/markup_lwt_unix.mli", line 20, characters 14-41:
# Error: Unbound module Lwt_io
```
