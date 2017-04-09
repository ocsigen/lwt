set -e
set -x

ocamlc -version

DIRECTORY=$(pwd)

# AppVeyor does not cache empty subdirectories of .opam, such as $SWITCH/build.
# To get around that, create a tar archive of .opam.
CACHE=$DIRECTORY/../opam-cache-$SYSTEM-$COMPILER-$LIBEV.tar

pin_extra_package () {
    PACKAGE=$1
    ( cd src/$PACKAGE/ && oasis setup -setup-update none )
    opam pin add -y --no-action src/$PACKAGE/
}

if [ ! -f $CACHE ]
then
    opam init -y --auto-setup
    eval `opam config env`

    # Pin Lwt and install its dependencies. This also installs OASIS, which is
    # needed later to generate the build systems of extra packages.s
    opam pin add -y --no-action .
    opam install -y --deps-only lwt
    opam install -y camlp4
    if [ "$LIBEV" = yes ]
    then
        opam install -y conf-libev
    fi

    # Generate build systems of extra packages and pin them. This includes
    # lwt_glib, which isn't actually installed on any system tested in AppVeyor.
    pin_extra_package react
    pin_extra_package ssl
    pin_extra_package glib

    # For the tests, obviously...
    opam install -y ounit

    ( cd ~ ; tar cf $CACHE .opam )
else
    ( cd ~ ; tar xf $CACHE )
    eval `opam config env`
fi
