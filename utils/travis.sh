set -x



# Install system packages.
packages_apt () {
    case $COMPILER in
        4.01) PPA=avsm/ocaml41+opam12;;
        4.02) PPA=avsm/ocaml42+opam12;;
        4.03) PPA=avsm/ocaml42+opam12; DO_SWITCH=yes;;
           *) echo Unsupported compiler $COMPILER; exit 1;;
    esac

    sudo add-apt-repository -y ppa:$PPA
    sudo apt-get update -qq
    sudo apt-get install -qq ocaml-nox opam

    if [ "$LIBEV" = yes ]
    then
        sudo apt-get install -qq libev-dev
    fi
}

packages_homebrew () {
    brew update > /dev/null
    brew install gtk+ opam

    if [ "$LIBEV" = yes ]
    then
        brew install libev
    fi

    DO_SWITCH=yes
}

packages_macports () {
    eval `wget -q -O - https://aantron.github.io/binaries/macports/x86_64/macports/current/install.sh | bash`
    sudo port install pkgconfig gtk2 | cat

    if [ "$LIBEV" = yes ]
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
    4.01) SWITCH=4.01.0;;
    4.02) SWITCH=4.02.3;;
    4.03) SWITCH=4.03.0;;
       *) echo Unsupported compiler $COMPILER; exit 1;;
esac

if [ -n "$DO_SWITCH" ]
then
    opam init --compiler=$SWITCH -ya
else
    opam init -ya
fi

eval `opam config env`

ACTUAL_COMPILER=`ocamlc -version`
if [ "$ACTUAL_COMPILER" != "$SWITCH" ]
then
    echo Expected OCaml $SWITCH, but $ACTUAL_COMPILER is installed
fi



# Install optional dependencies.
if [ "$LIBEV" = yes ]
then
    opam install -y conf-libev
fi



# Pin Lwt, install its optional dependencies, then install Lwt and run the
# tests.
opam pin add -y --no-action .
opam install -y `opam list --short --depopts --required-by lwt | grep -v '^conf-'`
opam install -y ounit
opam install -y --build-test --keep-build-dir --verbose lwt



# Some sanity checks.
if [ "$LIBEV" != yes ]
then
    ! opam list -i conf-libev
fi

if [ "$COMPILER" != 4.01 ]
then
    opam list -i ppx_tools
fi
