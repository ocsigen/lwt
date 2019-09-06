set -e
set -x

eval `opam config env`

if [ -d _cache/_build ]
then
    cp -r _cache/_build .
fi

make build
dune runtest -j 1 --no-buffer --force

if [ ! -d _cache/_build ]
then
    mkdir -p _cache
    cp -r _build _cache
fi
