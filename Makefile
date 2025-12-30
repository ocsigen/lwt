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
	dune runtest

# Promote expect test output.
.PHONY : promote
promote :
	for FILE in $$(ls _build/default/test/ppx_expect/cases/*.fixed); \
	do \
		EXPECT=test/ppx_expect/cases/$$(basename $${FILE%.fixed}).expect; \
		cp $$FILE $$EXPECT; \
	done

# Install dependencies needed during development.
.PHONY : dev-deps
dev-deps :
	opam install . --deps-only --yes

# Use Dune+odoc to generate static html documentation.
# Currently requires ocaml 4.03.0 to install odoc.
.PHONY: doc
doc:
	dune build @doc

# Build HTML documentation with ocamldoc
.PHONY: doc-api-html
doc-api-html: build
	$(MAKE) -C docs api/html/index.html

# Build wiki documentation with wikidoc
# requires ocaml 4.03.0 and pinning the repo
# https://github.com/ocsigen/wikidoc
.PHONY: doc-api-wiki
doc-api-wiki: build
	$(MAKE) -C docs api/wiki/index.wiki

# ppx_let integration test.
.PHONY : ppx_let-test
ppx_let-test :
	dune build test/ppx_let/test.exe
	dune exec test/ppx_let/test.exe

.PHONY: clean
clean :
	dune clean
	rm -fr docs/api
	rm -f src/unix/discover_arguments

EXPECTED_FILES := \
    --expect src/core/ \
    --expect src/react/ \
    --expect src/unix/ \
    --do-not-expect src/unix/config/ \
    --do-not-expect src/unix/lwt_gc.ml \
    --do-not-expect src/unix/lwt_throttle.ml \
    --do-not-expect src/unix/unix_c/
