set -e
set -x

# install packages and run tests
opam install -y -t --verbose lwt lwt_react lwt_camlp4

! opam list -i batteries
