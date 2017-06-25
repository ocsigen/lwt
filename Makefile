# Copyright : (c) 2012, Jérémie Dimino <jeremie@dimino.org>
#                 2017, Andrew Ray <evilkidder@gmail.com>

# Default rule
default: build

# build the usual development packages
build: check-config
	jbuilder build \
		--only-packages lwt \
		@install

# build everything
all: check-config
	jbuilder build @install

# run all unit tests
test: check-config
	jbuilder runtest

# configuration
check-config:
	@if [ ! -f src/jbuild-ignore ] ; \
	then \
	    make default-config ; \
	fi

default-config:
	ocaml src/util/configure.ml -use-libev false -use-camlp4 false

# Use jbuilder/odoc to generate static html documentation.
# Currenty requires ocaml 4.03.0 to install odoc.
doc:
	jbuilder build @doc

# Build HTML documentation with ocamldoc
doc-api-html: all
	make -C doc api/html/index.html

# Build wiki documentation with wikidoc
# requires ocaml 4.03.0 and pinning the repo
# https://github.com/ocsigen/wikidoc
doc-api-wiki: all
	make -C doc api/wiki/index.wiki

#install:
#	ocaml src/util/install_filter.ml
#	jbuilder install
#
#uninstall:
#	jbuilder uninstall
#
#reinstall:
#	jbuilder uninstall
#	jbuilder install

# Use opam-installer, rather than jbuilder while we need to
# post-process the lwt.install file
install:
	ocaml src/util/install_filter.ml
	opam-installer --prefix `opam config var prefix` -i lwt.install

uninstall:
	opam-installer --prefix `opam config var prefix` -u lwt.install

reinstall: uninstall install

# Packaging tests. These are run with Lwt installed by OPAM, typically during
# CI. To run locally, run the install-for-packaging-test target first.
packaging-test:
	ocamlfind query lwt
	for TEST in `ls -d tests/packaging/*/*` ; \
	do \
	    make -wC $$TEST ; \
	done

install-for-packaging-test: clean
	opam pin add --yes --no-action lwt .
	opam pin add --yes --no-action lwt_react .
	opam pin add --yes --no-action lwt_ssl .
	opam pin add --yes --no-action lwt_glib .
	opam install --yes camlp4
	opam reinstall --yes lwt lwt_react lwt_ssl lwt_glib

clean:
	rm -fr _build
	rm -f *.install
	rm -fr doc/api
	rm -f src/jbuild-ignore src/unix/lwt_config
	for TEST in `ls -d tests/packaging/*/*` ; \
	do \
	    make -wC $$TEST clean ; \
	done

clean-coverage:
	rm -rf bisect*.out
	rm -rf _coverage/

coverage: test
	bisect-ppx-report -I _build/ -html _coverage/ bisect*.out
	bisect-ppx-report -text - -summary-only bisect*.out
	@echo See _coverage/index.html

.PHONY: \
    default build doc test all install uninstall reinstall clean coverage \
    check-config default-config doc-api-html doc-api-wiki clean-coverage
