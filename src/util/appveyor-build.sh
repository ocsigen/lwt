set -e
set -x

# install packages and run tests
if [ "$SYSTEM" = cygwin ]
then
    opam install -y -t --verbose lwt lwt_react lwt_ssl lwt_camlp4
else
    opam install -y -t --verbose lwt lwt_react lwt_camlp4
fi

! opam list -i batteries
