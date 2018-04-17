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
        4.07) PPA=avsm/ocaml42+opam12; DO_SWITCH=yes;;
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
    # See https://github.com/Homebrew/homebrew-core/issues/26358.
    brew upgrade python > /dev/null

    if [ "$COMPILER" = system ]
    then
        brew install ocaml
    else
        DO_SWITCH=yes
    fi

    brew install opam

    if [ "$LIBEV" != no ]
    then
        brew install libev
    fi
}

# This code is dead for now – there is some upstream problem in MacPorts, so we
# have disabled testing on it. If that is not fixed soon, this code should be
# removed from this script.
packages_macports () {
    eval `wget -q -O - https://aantron.github.io/binaries/macports/x86_64/macports/current/install.sh | bash`
    sudo port install pkgconfig | cat

    if [ "$LIBEV" != no ]
    then
        sudo port install libev | cat
    fi

    wget -q -O - https://aantron.github.io/binaries/macports/x86_64/opam/1.2/install.sh | bash
    wget -q -O - https://aantron.github.io/binaries/macports/x86_64/ocaml/$COMPILER/install.sh | bash
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
    4.06) OCAML_VERSION=4.06.1;;
    4.07) OCAML_VERSION=4.07.0+beta2;;
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



# Install Lwt's development dependencies.
make dev-deps

if [ "$LIBEV" != no ]
then
    opam install -y conf-libev
fi



# Build and run the tests.
if [ "$LIBEV" != no ]
then
    LIBEV_FLAG=true
else
    LIBEV_FLAG=false
fi

ocaml src/util/configure.ml -use-libev $LIBEV_FLAG
make build
make test
make coverage



# Run the packaging tests.
make clean
make install-for-packaging-test
make packaging-test



# Some sanity checks.
if [ "$LIBEV" == no ]
then
    ! opam list -i conf-libev
fi

opam list -i ppx_tools_versioned
! opam list -i batteries
