# Makefile
# --------
# Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of lwt.

MELT := melt -latex xelatex -pdf -no-link myocamlbuild.ml

all:
	$(MELT) manual.mlt

manual.pdf:
	$(MELT) manual.mlt
	cp _melt/manual.pdf manual.pdf

clean-melt:
	melt -clean

clean:
	melt -clean
	rm -f *.pdf
