# Copyright : (c) 2012, Jérémie Dimino <jeremie@dimino.org>
#                 2017, Andrew Ray <evilkidder@gmail.com>

# Default rule
.PHONY: default
default: build

# build the usual development packages
.PHONY: build
build: check-config
	jbuilder build --dev --only-packages lwt

# build everything, including additional packages
.PHONY: build-all
build-all: check-config
	jbuilder build --dev --only-packages lwt,lwt_react

# run unit tests for package lwt
.PHONY: test
test: build
	jbuilder runtest --dev --only-packages lwt -j 1 --no-buffer

# run all unit tests
.PHONY: test-all
test-all: check-config
	jbuilder runtest --dev --only-packages lwt,lwt_react

# configuration
.PHONY: check-config
check-config:
	@if [ ! -f src/jbuild-ignore ] ; \
	then \
	    make default-config ; \
	fi

.PHONY: default-config
default-config:
	ocaml src/util/configure.ml -use-libev false -use-camlp4 false

# Use jbuilder/odoc to generate static html documentation.
# Currenty requires ocaml 4.03.0 to install odoc.
.PHONY: doc
doc:
	jbuilder build @doc

# Build HTML documentation with ocamldoc
.PHONY: doc-api-html
doc-api-html: build-all
	make -C doc api/html/index.html

# Build wiki documentation with wikidoc
# requires ocaml 4.03.0 and pinning the repo
# https://github.com/ocsigen/wikidoc
.PHONY: doc-api-wiki
doc-api-wiki: build-all
	make -C doc api/wiki/index.wiki

# Use opam-installer, rather than jbuilder while we need to
# post-process the lwt.install file
.PHONY: install
install:
	ocaml src/util/install_filter.ml
	opam-installer --prefix `opam config var prefix` -i lwt.install

.PHONY: uninstall
uninstall:
	opam-installer --prefix `opam config var prefix` -u lwt.install

.PHONY: reinstall
reinstall: uninstall install

# Packaging tests. These are run with Lwt installed by OPAM, typically during
# CI. To run locally, run the install-for-packaging-test target first.
.PHONY: packaging-test
packaging-test:
	ocamlfind query lwt
	for TEST in `ls -d test/packaging/*/*` ; \
	do \
	    make -wC $$TEST || exit 1 ; \
		echo ; \
		echo ; \
	done

.PHONY: install-for-packaging-test
install-for-packaging-test: clean
	opam pin add --yes --no-action lwt .
	opam pin add --yes --no-action lwt_ppx .
	opam pin add --yes --no-action lwt_react .
	opam pin add --yes --no-action lwt_log .
	opam pin add --yes --no-action lwt_camlp4 .
	opam reinstall --yes lwt lwt_ppx lwt_react lwt_log lwt_camlp4

.PHONY: clean
clean:
	jbuilder clean
	find . -name '.merlin' | xargs rm -f
	rm -fr doc/api
	rm -f src/jbuild-ignore src/unix/lwt_config src/core/flambda.flag
	for TEST in `ls -d test/packaging/*/*` ; \
	do \
	    make -wC $$TEST clean ; \
	done
	rm -rf _coverage/

BISECT_FILES_PATTERN := _build/default/test/*/bisect*.out

.PHONY: coverage
coverage: clean check-config
	BISECT_ENABLE=yes jbuilder runtest --dev --only-packages lwt,lwt_react
	bisect-ppx-report \
	    -I _build/default/ -html _coverage/ \
	    -text - -summary-only \
	    $(BISECT_FILES_PATTERN)
	@echo See _coverage/index.html
