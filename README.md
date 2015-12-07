# Lwt: lightweight thread library for OCaml

--------------------------------------------------------------------------

This library is part of the Ocsigen project. See:

  http://ocsigen.org/lwt

--------------------------------------------------------------------------

## Requirements:

 * OCaml (>= 4.01)
 * findlib
 * [optional] camlp4 (from http://github.com/ocaml/camlp4)
 * [optional] ppx_tools (from http://github.com/alainfrisch/ppx_tools)
 * [optional] react (from http://erratique.ch/software/react)
 * [optional] libev (from http://software.schmorp.de/pkg/libev.html)
 * [optional] ocamlssl (>= 0.4.0) (ocamlssl needs openssl) (>= 0.4.1 for MacOS)
 * [optional] glib-2.0 development files and pkg-config

If the dependencies are not installed on your computer, you can use OPAM
to install them automatically. See:

  http://opam.ocaml.org/

They might also be available through your distribution.

--------------------------------------------------------------------------

## Instructions:

 * run `ocaml setup.ml -configure` to configure sources
   You can add `--enable-<lib>` to enable compilation of
   the sub-library <lib>. The flag `--enable-all` will
   enable everything, including `--enable-ppx`; pass
   `--disable-ppx` explicitly on OCaml <4.02.
   In order to compile without libev support you must add
   `--disable-libev`.
   On OCaml >= 4.02, you should pass '--enable-ppx' and
   install ppx_tools.
 * run `ocaml setup.ml -build` to compile
 * run `ocaml setup.ml -install` as root to install compiled libraries
 * run `ocaml setup.ml -uninstall` as root to uninstall them

HTML documentation is generated in `_build/lwt.docdir/`, but is not
installed by default.

If you get the development version you need to obtain OASIS
(http://oasis.forge.ocamlcore.org/).

The Lwt toplevel was deprecated and removed. Use utop instead:

  https://github.com/diml/utop

--------------------------------------------------------------------------

## Authors:

 * Jérôme Vouillon
 * Vincent Balat
 * Nataliya Guts
 * Pierre Clairambault
 * Stéphane Glondu
 * Jérémie Dimino
 * Warren Harris
 * Pierre Chambart
 * Mauricio Fernandez
 * Gregoire Henri
 * Gabriel Radanne
 * Peter Zotov
 * Hugo Heuzard
 * Vincent Bernardoff
 * Romain Slootmaekers

See each source file for copyright information, and COPYING for license.

--------------------------------------------------------------------------
