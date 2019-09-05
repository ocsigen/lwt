set -x



date

# Install libev, if requested.
if [ "$LIBEV" != no ]
then
    case $TRAVIS_OS_NAME in
        "linux")
            sudo apt-get update -qq
            sudo apt-get install -qq libev-dev;;
        "osx")
            brew update > /dev/null
            brew install libev
    esac
fi



date

# Install opam.
case $TRAVIS_OS_NAME in
    "linux") OPAM_OS=linux;;
    "osx") OPAM_OS=macos;;
    *) echo Unsupported system $TRAVIS_OS_NAME; exit 1;;
esac

OPAM_VERSION=2.0.5
OPAM_PKG=opam-${OPAM_VERSION}-x86_64-${OPAM_OS}

wget https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/${OPAM_PKG}
sudo mv ${OPAM_PKG} /usr/local/bin/opam
sudo chmod a+x /usr/local/bin/opam



date

# Initialize opam.
opam init -y --bare --disable-sandboxing --disable-shell-hook
if [ ! -d _opam/bin ]
then
    rm -rf _opam
    opam switch create . $COMPILER $REPOSITORIES --no-install
fi
eval `opam env`
opam --version
ocaml -version



date

# Install Lwt's development dependencies.
if [ ! -d _cache/_build ]
then
    make dev-deps

    if [ "$LIBEV" != no ]
    then
        opam install -y conf-libev
    fi
else
    cp -r _cache/_build .
fi



date

# Build and run the tests.
if [ "$LIBEV" != no ]
then
    LWT_DISCOVER_ARGUMENTS="--use-libev true"
else
    LWT_DISCOVER_ARGUMENTS="--use-libev false"
fi
export LWT_DISCOVER_ARGUMENTS

date

if [ "$COVERAGE" != yes ]
then
    make build
    dune runtest -j 1 --no-buffer --force
else
    make coverage-only
    bisect-ppx-report send-to Coveralls
fi

if [ ! -d _cache/_build ]
then
    mkdir -p _cache
    cp -r _build _cache
fi



date

# Run the packaging tests.
if [ "$PACKAGING" == yes ]
then
    make install-for-packaging-test
    make packaging-test
    make uninstall-after-packaging-test
fi



date

# Run the ppx_let integratio test.
if [ "$PPX_LET" == yes ]
then
    make ppx_let-test-deps
    make ppx_let-test
fi



date

# Clean up the opam switch for better caching.
opam clean

date
