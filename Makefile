# Lightweight thread library for Objective Caml
# http://www.ocsigen.org/lwt
# Makefile
# Copyright (C) 2008 Stéphane Glondu
# Laboratoire PPS - CNRS Université Paris Diderot
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

NAME := lwt
VERSION := $(shell head -n 1 VERSION)

ARCHIVE_BYTE := $(NAME).cma
ARCHIVE_OPT := $(NAME).cmxa
TOINSTALL = $(wildcard *.mli _build/*.cmi) \
  $(wildcard _build/*.cma _build/*.cmx* _build/*.a)

all: META byte opt doc

byte:
	$(OCAMLBUILD) $(ARCHIVE_BYTE)

opt:
	$(OCAMLBUILD) $(ARCHIVE_OPT)

doc:
	$(OCAMLBUILD) lwt.docdir/index.html

examples:
	$(MAKE) -C examples

META: VERSION META.in
	sed -e 's/@VERSION@/$(VERSION)/' META.in > META

dist:
	DARCS_REPO=$(PWD) darcs dist -d $(NAME)-$(VERSION)

install:
	mkdir -p "$(DESTDIR)"
	$(OCAMLFIND) install $(NAME) -destdir "$(DESTDIR)" META $(TOINSTALL)

uninstall:
	$(OCAMLFIND) remove $(NAME) -destdir "$(DESTDIR)"

clean:
	$(OCAMLBUILD) -clean
	-rm -Rf *~ *.bak $(NAME)-*.tar.gz META
	$(MAKE) -C examples clean


.PHONY: all byte opt examples install uninstall clean
