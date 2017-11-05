set -x



# Install system packages.
packages_apt () {
    case $COMPILER in
        4.01) PPA=avsm/ocaml41+opam12;;
        4.02) PPA=avsm/ocaml42+opam12;;
        4.03) PPA=avsm/ocaml42+opam12; DO_SWITCH=yes;;
        4.04) PPA=avsm/ocaml42+opam12; DO_SWITCH=yes;;
        4.05) PPA=avsm/ocaml42+opam12; DO_SWITCH=yes;;
        4.06) PPA=avsm/ocaml42+opam12; DO_SWITCH=yes;;
           *) echo Unsupported compiler $COMPILER; exit 1;;
    esac

    sudo add-apt-repository -y ppa:$PPA
    sudo apt-get update -qq

    if [ -z "$DO_SWITCH" ]
    then
        sudo apt-get install -qq ocaml-nox
    fi

    sudo apt-get install -qq opam

    if [ "$LIBEV" != no ]
    then
        sudo apt-get install -qq libev-dev
    fi
}

packages_homebrew () {
    brew update > /dev/null

    if [ "$COMPILER" = system ]
    then
        brew install ocaml
    else
        DO_SWITCH=yes
    fi

    brew install gtk+ opam

    if [ "$LIBEV" != no ]
    then
        brew install libev
    fi
}

packages_macports () {
    eval `wget -q -O - https://aantron.github.io/binaries/macports/x86_64/macports/current/install.sh | bash`
    sudo port install pkgconfig gtk2 | cat

    if [ "$LIBEV" != no ]
    then
        sudo port install libev | cat
    fi

    wget -q -O - https://aantron.github.io/binaries/macports/x86_64/opam/1.2/install.sh | bash
    wget -q -O - https://aantron.github.io/binaries/macports/x86_64/ocaml/$COMPILER/install.sh | bash
    wget -q -O - https://aantron.github.io/binaries/macports/x86_64/camlp4/$COMPILER/install.sh | bash
}

packages_osx () {
    case $PACKAGE_MANAGER in
        macports) packages_macports;;
               *) packages_homebrew;;
    esac
}

packages () {
    case $TRAVIS_OS_NAME in
        linux) packages_apt;;
          osx) packages_osx;;
            *) echo Unsupported system $TRAVIS_OS_NAME; exit 1;;
    esac
}

packages



# Initialize OPAM and switch to the right compiler, if necessary.
case $COMPILER in
    4.01) OCAML_VERSION=4.01.0;;
    4.02) OCAML_VERSION=4.02.3;;
    4.03) OCAML_VERSION=4.03.0;;
    4.04) OCAML_VERSION=4.04.2;;
    4.05) OCAML_VERSION=4.05.0;;
    4.06) OCAML_VERSION=4.06.0;;
    system) OCAML_VERSION=`ocamlc -version`;;
       *) echo Unsupported compiler $COMPILER; exit 1;;
esac

if [ "$FLAMBDA" = yes ]
then
    SWITCH="$OCAML_VERSION+flambda"
else
    SWITCH="$OCAML_VERSION"
fi

if [ -n "$DO_SWITCH" ]
then
    opam init --compiler=$SWITCH -ya
else
    opam init -ya
fi

eval `opam config env`

ACTUAL_COMPILER=`ocamlc -version`
if [ "$ACTUAL_COMPILER" != "$OCAML_VERSION" ]
then
    echo Expected OCaml $OCAML_VERSION, but $ACTUAL_COMPILER is installed
fi



# Pin Lwt, install dependencies, and then install Lwt. Lwt is installed
# separately because we want to keep the build directory for running the tests.
opam pin add -y --no-action lwt .

opam install -y --deps-only lwt

if [ "$HAVE_CAMLP4" != no ]
then
    opam install -y camlp4
fi

if [ "$LIBEV" != no ]
then
    opam install -y conf-libev
fi

opam install --keep-build-dir --verbose lwt

# Pin additional packages and install them. There
# aren't any specific tests for these packages. Installation itself is the only
# test.
install_extra_package () {
    PACKAGE=$1
    opam pin add -y --no-action lwt_$PACKAGE .
    opam install -y --verbose lwt_$PACKAGE
}

install_extra_package ppx
install_extra_package react
install_extra_package ssl
install_extra_package glib

# Build and run the tests.
opam install -y ounit
cd `opam config var lib`/../build/lwt.*
make clean

if [ "$LIBEV" != no ]
then
    LIBEV_FLAG=true
else
    LIBEV_FLAG=false
fi

if [ "$HAVE_CAMLP4" = no ]
then
    CAMLP4_FLAG=false
else
    CAMLP4_FLAG=true
fi

ocaml src/util/configure.ml -use-libev $LIBEV_FLAG -use-camlp4 $CAMLP4_FLAG
make build-all test-all



# Run the packaging tests.
make packaging-test



# Build examples.
jbuilder build doc/examples/unix/logging.exe
_build/default/doc/examples/unix/logging.exe
jbuilder build doc/examples/unix/parallelize.exe
jbuilder build doc/examples/unix/relay.exe
# Skipping the GTK example, as installing GTK is dubious in a headless buildbot.
# opam install -y lablgtk
# jbuilder build doc/examples/gtk/connect.exe



# Some sanity checks.
if [ "$LIBEV" != yes ]
then
    ! opam list -i conf-libev
fi

opam list -i ppx_tools_versioned
! opam list -i batteries
