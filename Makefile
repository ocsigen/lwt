# Makefile
# --------
# Copyright : (c) 2012, Jérémie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# Generic Makefile for oasis project

# Suppress duplicate topdirs.cmi warnings.
OCAMLFIND_IGNORE_DUPS_IN = $(shell ocamlfind query compiler-libs)
export OCAMLFIND_IGNORE_DUPS_IN

# Default rule
default: build

# build the usual development packages
build: 
	jbuilder build \
		lwt-core.install \
		lwt-log.install \
		lwt-unix.install \
		lwt-preemptive.install \
		lwt-simple-top.install \
		lwt-ppx.install 

doc-api: $(SETUP) setup.data build
	./$(SETUP) -build lwt-api.docdir/index.html

# run all tests
test: 
	jbuilder runtest

# build everything
all: 
	jbuilder build @install

install: 
	jbuilder install

uninstall: 
	jbuilder uninstall

reinstall: 
	jbuilder uninstall
	jbuilder install

clean: 
	rm -fr _build
	rm -f *.install

clean-coverage:
	rm -rf bisect*.out
	rm -rf _coverage/

coverage: test
	bisect-ppx-report -I _build/ -html _coverage/ bisect*.out
	bisect-ppx-report -text - -summary-only bisect*.out
	@echo See _coverage/index.html

.PHONY: default build doc test all install uninstall reinstall clean coverage
