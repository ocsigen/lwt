set -e
set -x

opam install -y --build-test --keep-build-dir --verbose lwt
