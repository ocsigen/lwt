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

include utils/utils.mk

# +------------------------------------------------------------------+
# | Configuration                                                    |
# +------------------------------------------------------------------+

# General project parameters:
NAME := lwt
VERSION := $(shell head -n 1 VERSION)
DESTDIR := $(shell $(OCAMLFIND) printconf destdir)

# Context dependent options:
HAVE_THREADS := $(call have_package,threads)
HAVE_SSL := $(call have_package,ssl)
HAVE_GLIB := $(and $(call have_package,lablgtk2),$(call exec,pkg-config glib-2.0))
HAVE_TEXT:= $(call have_package,text)

DOC := lwt.docdir/index.html

# Libraries to build:
LIBRARIES := lwt \
	$(if $(HAVE_THREADS),lwt_preemptive lwt_extra) \
	$(if $(HAVE_SSL),lwt_ssl) \
	$(if $(HAVE_GLIB),lwt_glib) \
	$(if $(HAVE_TEXT),lwt_text lwt_top)

ARCHIVES_BYTE := $(patsubst %,src/%.cma,$(LIBRARIES)) syntax/pa_lwt.cmo
ARCHIVES_NATIVE := $(patsubst %,src/%.cmxa,$(LIBRARIES))

# +------------------------------------------------------------------+
# | Rules                                                            |
# +------------------------------------------------------------------+

all:
	$(info +--[ compilation options ]----------+)
	$(info | native compilation:           $(call yes_no,$(HAVE_NATIVE) ) |)
	$(info | preemptive threads support:   $(call yes_no,$(HAVE_THREADS)) |)
	$(info | ssl support:                  $(call yes_no,$(HAVE_SSL)    ) |)
	$(info | glib support:                 $(call yes_no,$(HAVE_GLIB)   ) |)
	$(info | text support:                 $(call yes_no,$(HAVE_TEXT)   ) |)
	$(info +-----------------------------------+)
	$(OCAMLBUILD) META $(ARCHIVES_BYTE) $(if $(HAVE_NATIVE),$(ARCHIVES_NATIVE)) $(DOC)

byte:
	$(OCAMLBUILD) META $(ARCHIVES_BYTE)

native:
	$(OCAMLBUILD) META $(ARCHIVES_NATIVE)

opt: native

doc:
	$(OCAMLBUILD) $(DOC)

examples:
	$(MAKE) -C examples

dist:
	DARCS_REPO=$(PWD) darcs dist -d $(NAME)-$(VERSION)

install:
	mkdir -p "$(DESTDIR)"
	$(OCAMLFIND) install $(NAME) -destdir "$(DESTDIR)" _build/META \
	  $(wildcard _build/src/*.mli) \
	  $(wildcard _build/src/*.cmi) \
	  $(wildcard _build/src/*.cmx) \
	  $(wildcard _build/src/*.cma) \
	  $(wildcard _build/src/*.cmxa) \
	  $(wildcard _build/src/*.so) \
	  $(wildcard _build/src/*.a) \
	  _build/syntax/pa_lwt.cmo

uninstall:
	$(OCAMLFIND) remove $(NAME) -destdir "$(DESTDIR)"

reinstall: uninstall install

clean:
	$(OCAMLBUILD) -clean
	rm -Rf *~ src/*~ $(NAME)-*.tar.gz
	$(MAKE) -C examples clean


.PHONY: sanitize all byte native opt examples install uninstall clean
