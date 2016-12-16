set -e
set -x

if [ "$SYSTEM" = cygwin ]
then
    PACKAGES="lwt lwt_react lwt_ssl"
else
    PACKAGES="lwt lwt_react"
fi

opam install -y --build-test --keep-build-dir --verbose $PACKAGES
