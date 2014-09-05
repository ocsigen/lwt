
opam pin add --no-action lwt .
opam install ssl lablgtk text react conf-libev
opam install camlp4
opam install --deps-only lwt
opam install --verbose lwt

do_build_doc () {
  rm -rf _build/lwt-api.wikidocdir
  ./setup-dev.exe -build lwt-api.wikidocdir/index.wiki
  cp -Rf manual/*.wiki ${MANUAL_SRC_DIR}
  cp -Rf _build/lwt-api.wikidocdir/*.wiki ${API_DIR}
}

do_remove () {
  opam remove lwt
}
