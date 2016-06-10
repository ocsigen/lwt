set -e
set -x

ocamlc -version

opam pin add -y --no-action .
opam install -y --build-test --keep-build-dir lwt
