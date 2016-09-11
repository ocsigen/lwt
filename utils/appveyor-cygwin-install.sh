set -e
set -x

ocamlc -version

DIRECTORY=$(pwd)

# AppVeyor does not cache empty subdirectories of .opam, such as $SWITCH/build.
# To get around that, create a tar archive of .opam.
CACHE=$DIRECTORY/../opam-cache-$SYSTEM-$COMPILER.tar

if [ ! -f $CACHE ]
then
    opam init -y --auto-setup
    eval `opam config env`

    opam pin add -y --no-action .
    opam install -y --deps-only lwt
    opam install -y ounit

    ( cd ~ ; tar cf $CACHE .opam )
else
    ( cd ~ ; tar xf $CACHE )
    eval `opam config env`
fi
