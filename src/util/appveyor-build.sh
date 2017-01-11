set -e
set -x

if [ "$SYSTEM" = cygwin ]
then
    PACKAGES="lwt lwt_react lwt_ssl"
else
    PACKAGES="lwt lwt_react"
fi

opam install -y --keep-build-dir --verbose $PACKAGES
cd `opam config var lib`/../build/lwt.*
ocaml setup.ml -configure --enable-tests
make test

! opam list -i batteries
