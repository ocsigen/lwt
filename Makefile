# Lightweight thread library for Objective Caml
# http://www.ocsigen.org/lwt
# Makefile
# Copyright (C) 2008 Stéphane Glondu
# Laboratoire PPS - CNRS Université Paris Diderot
#               2009 Jérémie Dimino
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, with linking exception;
# either version 2.1 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

OCAMLFIND := ocamlfind
OCAMLBUILD := ocamlbuild
DESTDIR := $(shell $(OCAMLFIND) printconf destdir)/$(NAME)

# Use classic-display when compiling under a terminal which do not
# support ANSI sequence
ifeq ($(TERM),dumb)
OCAMLBUILD += -classic-display
endif

NAME := lwt
VERSION := $(shell head -n 1 VERSION)

DOC = lwt.docdir/index.html
SYNTAX = syntax/pa_lwt.cmo
ARCHIVES_BYTE := $(patsubst %.mllib,%.cma,$(wildcard src/*.mllib)) src/top_lwt.cmo
ARCHIVES_OPT := $(patsubst %.mllib,%.cmxa,$(wildcard src/*.mllib)) src/top_lwt.cmx
TOINSTALL = $(wildcard $(ARCHIVES_BYTE) $(ARCHIVES_OPT)) \
  $(wildcard src/*.mli _build/src/*.cmi _build/src/*.cma) \
  $(wildcard _build/src/*.cmx* _build/src/*.a) \
  $(wildcard _build/src/*.so) _build/syntax/pa_lwt.cmo _build/src/top_lwt.cmo

all:
	$(OCAMLBUILD) META $(ARCHIVES_BYTE) $(ARCHIVES_OPT) $(SYNTAX) $(DOC)

byte:
	$(OCAMLBUILD) $(ARCHIVES_BYTE)

opt:
	$(OCAMLBUILD) $(ARCHIVES_OPT)

doc:
	$(OCAMLBUILD) $(DOC)

examples:
	$(MAKE) -C examples

dist:
	DARCS_REPO=$(PWD) darcs dist -d $(NAME)-$(VERSION)

install:
	mkdir -p "$(DESTDIR)"
	$(OCAMLFIND) install $(NAME) -destdir "$(DESTDIR)" _build/META $(TOINSTALL)

uninstall:
	$(OCAMLFIND) remove $(NAME) -destdir "$(DESTDIR)"

reinstall: uninstall install

clean:
	$(OCAMLBUILD) -clean
	-rm -Rf *~ src/*~ $(NAME)-*.tar.gz
	$(MAKE) -C examples clean


.PHONY: all byte opt examples install uninstall clean
