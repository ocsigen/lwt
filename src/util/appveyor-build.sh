set -e
set -x

# install packages and run tests
opam install -y -t --verbose lwt lwt_react

! opam list -i batteries
