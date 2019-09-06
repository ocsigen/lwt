set -e
set -x

DIRECTORY=`pwd`

# AppVeyor does not cache empty subdirectories of .opam, such as $SWITCH/build.
# To get around that, create a tar archive of .opam.
CACHE=$DIRECTORY/opam-cache.tar

if [ -f $CACHE ]
then
    ( cd ~ ; tar xf $CACHE )
else
    opam init default https://github.com/fdopen/opam-repository-mingw.git#opam2 -c ocaml-variants.4.07.1+mingw64c --disable-sandboxing --yes --auto-setup

    make dev-deps
    opam clean

    ( cd ~ ; tar cf $CACHE .opam )
fi

eval `opam config env`

opam --version
ocaml -version
