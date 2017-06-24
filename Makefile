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

# run all tests
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
	rm -fr doc/api
	rm -f src/jbuild-ignore src/unix/lwt_config
	rm -rf bisect*.out
	rm -rf _coverage/

coverage: clean test
	bisect-ppx-report -I _build/ -html _coverage/ `find . -name 'bisect*.out'`
	bisect-ppx-report -text - -summary-only `find . -name 'bisect*.out'`
	@echo See _coverage/index.html

.PHONY: \
    default build doc test all install uninstall reinstall clean coverage \
    check-config default-config doc-api-html doc-api-wiki clean-coverage
