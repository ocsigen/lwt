set -e
set -x

ocamlc -version

DIRECTORY=$(pwd)

# AppVeyor does not cache empty subdirectories of .opam, such as $SWITCH/build.
# To get around that, create a tar archive of .opam.
CACHE=$DIRECTORY/../opam-cache-$SYSTEM-$COMPILER-$LIBEV.tar

if [ ! -f $CACHE ]
then
    opam init -y --auto-setup
    eval `opam config env`

    opam pin add -y --no-action lwt .
    opam pin add -y --no-action lwt_react .
    opam pin add -y --no-action lwt_ssl .
    # lwt_glib is not tested at the moment; this is for future-proofing.
    opam pin add -y --no-action lwt_glib .

    opam install -y --deps-only lwt
    # Install OUnit here; otherwie --build-test on installation of Lwt seems to
    # trigger recompilation of ocamlmod.
    opam install -y ounit
    opam install -y camlp4 react

    if [ "$SYSTEM" = cygwin ]
    then
        if [ "$LIBEV" = yes ]
        then
            opam install -y conf-libev
        fi
    fi

    ( cd ~ ; tar cf $CACHE .opam )
else
    ( cd ~ ; tar xf $CACHE )
    eval `opam config env`
fi
