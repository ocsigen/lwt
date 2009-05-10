# Makefile
# --------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of Lwt.

include ../utils/utils.mk

HAVE_LWT := $(call have_package,lwt)
HAVE_LWT_GLIB := $(call have_package,lwt.glib)

ifeq ($(HAVE_LWT),)
  $(error You need to install Lwt before compiling examples!)
endif

EXAMPLES := relay start_editor show_keys parallelize $(if $(HAVE_LWT_GLIB),ex_gtk)
EXAMPLES_BYTE := $(patsubst %,%.byte,$(EXAMPLES))
EXAMPLES_NATIVE := $(patsubst %,%.native,$(EXAMPLES))

best:
	$(OCAMLBUILD) $(if $(HAVE_NATIVE),$(EXAMPLES_NATIVE),$(EXAMPLES_BYTE))

all:
	$(OCAMLBUILD) $(EXAMPLES_BYTE) $(EXAMPLES_NATIVE)

byte:
	$(OCAMLBUILD) $(EXAMPLES_BYTE)

native:
	$(OCAMLBUILD) $(EXAMPLES_NATIVE)

clean:
	$(OCAMLBUILD) -clean
	rm -f *~

.PHONY: best all byte native clean
