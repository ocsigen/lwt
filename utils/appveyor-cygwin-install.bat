%CYGWIN%\setup-x86.exe -q -P ocaml,ocaml-camlp4,ocaml-compiler-libs,libncurses-devel,unzip,libmpfr-devel,patch,flexdll
set PATH=%PATH%;%CYGWIN%\bin
cd ..
wget https://github.com/ocaml/opam/releases/download/1.2.2/opam-full-1.2.2.tar.gz
tar xvf opam-full-1.2.2.tar.gz
%CYGSH% "cd /cygdrive/c/projects/opam-full-1.2.2 && env DJDIR=workaround ./configure && make lib-ext && make && make install"
%CYGSH% "opam init -ya"
%CYGSH% "opam switch %COMPILER%"
git clone https://github.com/ocaml/oasis.git
cd oasis
git checkout 0.4.6
wget -O - https://github.com/Chris00/oasis/commit/4316cf28797ab0686196e0d90651ebf0cdfb6319.patch | git am
wget -O - https://github.com/pwbs/oasis/commit/00640c1ef3ea4a3790699c0e35a15cad70a7a4b4.patch | git am
%CYGSH% "opam pin add -yn oasis /cygdrive/c/projects/oasis"
cd ../lwt
