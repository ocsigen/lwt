set -e
set -x

opam init default https://github.com/fdopen/opam-repository-mingw.git#opam2 -c ocaml-variants.4.07.1+mingw64c --disable-sandboxing --yes --auto-setup

make dev-deps
opam clean

eval `opam config env`

opam --version
ocaml -version
