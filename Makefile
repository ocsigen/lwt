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

# Use Dune+odoc to generate static html documentation (manual + API). The themed
# site published at ocsigen.org/lwt/ is built from this by doc/build.sh (wodoc);
# see doc/README.md. This replaces the former ocamldoc/wikidoc pipeline.
.PHONY: doc
doc:
	dune build @doc

# ppx_let integration test.
.PHONY : ppx_let-test
ppx_let-test :
	dune build test/ppx_let/test.exe
	dune exec test/ppx_let/test.exe

.PHONY: clean
clean :
	dune clean
	rm -f src/unix/discover_arguments

EXPECTED_FILES := \
    --expect src/core/ \
    --expect src/react/ \
    --expect src/unix/ \
    --do-not-expect src/unix/config/ \
    --do-not-expect src/unix/lwt_gc.ml \
    --do-not-expect src/unix/lwt_throttle.ml \
    --do-not-expect src/unix/unix_c/
