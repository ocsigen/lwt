# Makefile
# --------
# Copyright : (c) 2012, Jérémie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# Generic Makefile for oasis project

# Suppress duplicate topdirs.cmi warnings.
OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

# Set to setup.exe for the release
SETUP := setup-dev.exe

# Default rule
default: build

# Setup for the development version
setup-dev.exe: _oasis setup.ml
	grep -v '^#' setup.ml > setup_dev.ml
	ocamlfind ocamlopt -o $@ -linkpkg -package ocamlbuild,oasis.dynrun setup_dev.ml || \
	  ocamlfind ocamlc -o $@ -linkpkg -package ocamlbuild,oasis.dynrun setup_dev.ml || true
	rm -f setup_dev.*

# Setup for the release
setup.exe: setup.ml
	ocamlopt.opt -o $@ $< || ocamlopt -o $@ $< || ocamlc -o $@ $<
	rm -f setup.cmx setup.cmi setup.o setup.obj setup.cmo

setup: $(SETUP)

build: $(SETUP) setup.data
	./$(SETUP) -build $(BUILDFLAGS)

doc: $(SETUP) setup.data build
	./$(SETUP) -doc $(DOCFLAGS)

doc-api: $(SETUP) setup.data build
	./$(SETUP) -build lwt-api.docdir/index.html

test: $(SETUP) setup.data build clean-coverage
	./$(SETUP) -test $(TESTFLAGS)

all: $(SETUP)
	./$(SETUP) -all $(ALLFLAGS)

install: $(SETUP) setup.data
	./$(SETUP) -install $(INSTALLFLAGS)

uninstall: $(SETUP) setup.data
	./$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: $(SETUP) setup.data
	./$(SETUP) -reinstall $(REINSTALLFLAGS)

clean: $(SETUP) clean-coverage
	./$(SETUP) -clean $(CLEANFLAGS)

distclean: $(SETUP)
	./$(SETUP) -distclean $(DISTCLEANFLAGS)
	rm -rf setup*.exe

clean-coverage:
	rm -rf bisect*.out
	rm -rf _coverage/

configure: $(SETUP)
	./$(SETUP) -configure $(CONFIGUREFLAGS)

setup.data: $(SETUP)
	./$(SETUP) -configure $(CONFIGUREFLAGS)

coverage: test
	bisect-ppx-report -I _build/ -html _coverage/ bisect*.out
	bisect-ppx-report -text - -summary-only bisect*.out
	@echo See _coverage/index.html

.PHONY: default setup build doc test all install uninstall reinstall clean distclean configure coverage
