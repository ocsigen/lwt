{ stdenv, buildDunePackage, result, cppo }:

buildDunePackage rec {
  pname = "lwt";
  version = "0.5.0";
  minimumOCamlVersion = "4.03";
  src = ./.;
  propagatedBuildInputs = [ result cppo ];
  doCheck = !stdenv.isDarwin;
  meta = {
    homepage = https://ocsigen.org/lwt;
    description = "OCaml promises and concurrent I/O";
    license = stdenv.lib.licenses.mit;
    maintainers = with stdenv.lib.maintainers; [ dmjio ];
  };
}
