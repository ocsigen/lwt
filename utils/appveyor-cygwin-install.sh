set -e
set -x

opam init -y --auto-setup

cd ..
git clone https://github.com/ocaml/oasis.git
cd oasis
git checkout 0.4.6
wget -O - https://github.com/Chris00/oasis/commit/4316cf28797ab0686196e0d90651ebf0cdfb6319.patch | git am
wget -O - https://github.com/pwbs/oasis/commit/00640c1ef3ea4a3790699c0e35a15cad70a7a4b4.patch | git am
opam pin add -y --no-action oasis .
cd ../lwt
