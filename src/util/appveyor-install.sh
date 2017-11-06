set -e
set -x

ocamlc -version

DIRECTORY=$(pwd)

# AppVeyor does not cache empty subdirectories of .opam, such as $SWITCH/build.
# To get around that, create a tar archive of .opam.
CACHE=$DIRECTORY/../opam-cache-$SYSTEM-$COMPILER-$LIBEV.tar

pin_extra_package () {
    PACKAGE=$1
    opam pin add -y --no-action lwt_$PACKAGE .
}

if [ ! -f $CACHE ]
then
    opam init -y --auto-setup
    eval `opam config env`

    # Pin Jbuilder to 1.0+beta14, to avoid beta15. See
    #   https://github.com/janestreet/jbuilder/issues/311
    opam pin add -y --no-action jbuilder 1.0+beta14

    # Pin Lwt and install its dependencies.
    opam pin add -y --no-action lwt .
    opam install -y --deps-only lwt
    if [ "$LIBEV" = yes ]
    then
        opam install -y conf-libev
    fi

    # Generate build systems of extra packages and pin them. This includes
    # lwt_glib, which isn't actually installed on any system tested in AppVeyor.
    pin_extra_package react
    pin_extra_package ssl
    pin_extra_package glib
    pin_extra_package camlp4

    # For the tests, obviously...
    opam install -y ounit

    ( cd ~ ; tar cf $CACHE .opam )
else
    ( cd ~ ; tar xf $CACHE )
    eval `opam config env`
fi
