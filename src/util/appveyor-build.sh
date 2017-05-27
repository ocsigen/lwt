set -e
set -x

# install packages and run tests
if [ "$SYSTEM" = cygwin ]
then
    opam install -y -t --verbose lwt lwt_react lwt_ssl
else
    opam install -y -t --verbose lwt lwt_react
fi

! opam list -i batteries
