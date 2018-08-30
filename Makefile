# Default rule
.PHONY: default
default: build

# build the usual development packages
.PHONY: build
build:
	dune build

# run unit tests for package lwt
.PHONY: test
test: build
	dune runtest -j 1 --no-buffer

# Install dependencies needed during development.
.PHONY : dev-deps
dev-deps :
	opam install --yes --unset-root \
	  bisect_ppx \
	  cppo \
	  dune \
	  ocaml-migrate-parsetree \
	  ocamlfind \
	  ppx_tools_versioned \
	  react \
	  result \

# Use Dune+odoc to generate static html documentation.
# Currenty requires ocaml 4.03.0 to install odoc.
.PHONY: doc
doc:
	dune build @doc

# Build HTML documentation with ocamldoc
.PHONY: doc-api-html
doc-api-html: build-all
	make -C docs api/html/index.html

# Build wiki documentation with wikidoc
# requires ocaml 4.03.0 and pinning the repo
# https://github.com/ocsigen/wikidoc
.PHONY: doc-api-wiki
doc-api-wiki: build-all
	make -C docs api/wiki/index.wiki

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
	opam reinstall --yes lwt lwt_ppx lwt_react

.PHONY: clean
clean:
	dune clean
	find . -name '.merlin' | xargs rm -f
	rm -fr docs/api
	rm -f src/jbuild-ignore src/unix/lwt_config
	for TEST in `ls -d test/packaging/*/*` ; \
	do \
	    make -wC $$TEST clean ; \
	done
	rm -rf _coverage/

BISECT_FILES_PATTERN := _build/default/test/*/bisect*.out

.PHONY: coverage
coverage: clean
	BISECT_ENABLE=yes make build
	BISECT_ENABLE=yes dune runtest -j 1 --no-buffer
	bisect-ppx-report \
	    -I _build/default/ -html _coverage/ \
	    -text - -summary-only \
	    $(BISECT_FILES_PATTERN)
	@echo See _coverage/index.html
