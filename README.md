Lwt: lightweight thread library for OCaml

--------------------------------------------------------------------------

This library is part of the Ocsigen project. See:

  http://ocsigen.org/lwt

--------------------------------------------------------------------------

Requirements:

 * OCaml with ocamlbuild (>= 3.12.1)
 * findlib
 * [optional] react (from http://erratique.ch/software/react)
 * [optional] libev (from http://software.schmorp.de/pkg/libev.html)
 * [optional] ocaml-text (needed for the enhanced toplevel)
 * [optional] ocamlssl (>= 0.4.0) (ocamlssl needs openssl) (>= 0.4.1 for MacOS)
 * [optional] glib-2.0 development files and pkg-config

If the dependencies are not installed on your computer, you can use OPAM
to install them automatically. See:

  http://opam.ocaml.org/

They might also be available through your distribution.

--------------------------------------------------------------------------

Instructions:

 * run "ocaml setup.ml -configure" to configure sources
     You can add '--enable-<lib>' to enable compilation of
     the sub-library <lib>. The flag '--enable-all' will
     enable everything.
     In order to compile without libev support you must add
     '--disable-libev'.
 * run "ocaml setup.ml -build" to compile
 * run "ocaml setup.ml -install" as root to install compiled libraries
 * run "ocaml setup.ml -uninstall" as root to uninstall them

HTML documentation is generated in _build/lwt.docdir/, but is not
installed by default.

If you get the development version you need to obtain OASIS
(http://oasis.forge.ocamlcore.org/).

If you want to build the toplevel you have to install compiler
libraries. Under Debian it is the package ocaml-compiler-libs.

Note that the utop project replaces the Lwt toplevel:

  https://github.com/diml/utop

--------------------------------------------------------------------------

Authors:

 * Jérôme Vouillon
 * Vincent Balat
 * Nataliya Guts
 * Pierre Clairambault
 * Stéphane Glondu
 * Jérémie Dimino
 * Warren Harris (Metaweb Technologies, Inc.)
 * Pierre Chambart
 * Mauricio Fernandez

See each source file for copyright information, and COPYING for license.

--------------------------------------------------------------------------
