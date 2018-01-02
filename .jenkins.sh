opam pin add --no-action lwt .
opam install camlp4
opam install react conf-libev
opam install ocamlbuild uchar
opam install --deps-only lwt
opam install --verbose lwt

do_build_doc () {
  rm -rf doc/api
  # generate wikidoc documentation
  make doc-api-wiki
  # copy manual pages and api documentation
  cp -Rf doc/*.wiki ${MANUAL_SRC_DIR}
  cp -Rf doc/api/wiki/*.wiki ${API_DIR}
}

do_remove () {
  opam remove lwt
}
