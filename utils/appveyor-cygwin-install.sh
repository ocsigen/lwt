set -e
set -x

ocamlc -version

DIRECTORY=$(pwd)

# AppVeyor does not cache empty subdirectories of .opam, such as $SWITCH/build.
# To get around that, create a tar archive of .opam.
CACHE=$DIRECTORY/../opam-cache-$COMPILER.tar

if [ ! -f $CACHE ]
then
    opam init -y --auto-setup
    eval `opam config env`

    git config --global user.name "AppVeyor"
    git config --global user.email "appveyor@none.org"
    cd ..
    git clone https://github.com/ocaml/oasis.git
    cd oasis
    git checkout 0.4.6
    wget -O - https://github.com/Chris00/oasis/commit/4316cf28797ab0686196e0d90651ebf0cdfb6319.patch | git am
    wget -O - https://github.com/pwbs/oasis/commit/00640c1ef3ea4a3790699c0e35a15cad70a7a4b4.patch | git am
    opam pin add -y --no-action oasis .
    cd $DIRECTORY

    opam pin add -y --no-action .
    opam install -y --deps-only lwt
    opam install -y ounit

    ( cd ~ ; tar cf $CACHE .opam )
else
    ( cd ~ ; tar xf $CACHE )
    eval `opam config env`
fi
