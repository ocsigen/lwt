set -e
set -x

eval `opam config env`

make build
make test
