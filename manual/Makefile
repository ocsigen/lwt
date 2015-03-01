# Makefile
# --------
# Copyright : (c) 2010, Jérémie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of lwt.

all: manual.pdf

manual-wiki.tex: manual.wiki
	latex_of_wiki lwt-manual < manual.wiki > manual-wiki.tex.tmp
	mv manual-wiki.tex.tmp manual-wiki.tex

manual.pdf: manual.tex manual-wiki.tex
	rubber --pdf manual.tex

clean-aux:
	rm -f .latex_of_wiki_offsets *.log *.aux *.out *.toc

clean: clean-aux
	rm -f manual.pdf manual-wiki.tex manual-wiki.tex.tmp
