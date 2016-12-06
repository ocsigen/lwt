set -e
set -x

if [ "$SYSTEM" = cygwin ]
then
    PACKAGES="lwt lwt_ssl"
else
    PACKAGES=lwt
fi

opam install -y --build-test --keep-build-dir --verbose $PACKAGES
