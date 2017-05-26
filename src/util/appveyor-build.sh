set -e
set -x

if [ "$SYSTEM" = cygwin ]
then
    opam install -y --keep-build-dir --verbose lwt lwt_react lwt_ssl
    cd `opam config var lib`/../build/lwt.*
    jbuilder runtest --only-packages lwt,lwt_react,lwt_ssl
else
    opam install -y --keep-build-dir --verbose lwt lwt_react
    cd `opam config var lib`/../build/lwt.*
    jbuilder runtest --only-packages lwt,lwt_react
fi

! opam list -i batteries
