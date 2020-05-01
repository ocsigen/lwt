{ lib, fetchzip, pkgconfig, ncurses
, libev, buildDunePackage, ocaml
, cppo, ocplib-endian, result
, mmap, seq, stdenv
}:

buildDunePackage {
  src = ./.;
  pname = "lwt";
  version = "5.0.0";
  minimumOCamlVersion = "4.02";
  buildInputs =
    [ cppo ] ++ lib.optional (!lib.versionAtLeast ocaml.version "4.07") ncurses;
  propagatedBuildInputs =
    [ libev mmap ocplib-endian seq result ];
  doCheck = true;
  meta = {
    homepage = https://ocsigen.org/lwt;
    description = "OCaml promises and concurrent I/O";
    license = stdenv.lib.licenses.mit;
    maintainers = with stdenv.lib.maintainers; [ dmjio ];
  };
}
