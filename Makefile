# Copyright (c) 2004-2008 John Goerzen
#

CSOURCES:=tarf-encoder.c
CEXES:= $(patsubst %.c,%,$(CSOURCES))

all: hs c

.PHONY: hs
hs: setup			# GHC build
	./setup configure
	./setup build
	rm -f tarenc tarenc-encoder tarenc-scanner
	for ASDF in tarenc tarenc-encoder tarenc-scanner; do ln dist/build/$$ASDF/$$ASDF; done

c: $(CEXES)

%: %.c
	gcc -Wall -larchive -o $@ $<

hugsbuild: setup
	./setup configure --hugs
	./setup build

setup: Setup.lhs tarfilter.cabal
	ghc -package Cabal Setup.lhs -o setup

doc: man html pdf txt

.PHONY: man
man: datapacker.1

.PHONY: html
html: datapacker.html

.PHONY: pdf
pdf: datapacker.pdf

.PHONY: txt
txt: datapacker.txt

datapacker.html: datapacker.sgml
	docbook2html -u datapacker.sgml

datapacker.ps: datapacker.1
	man -t -l datapacker.1 > datapacker.ps

datapacker.pdf: datapacker.ps
	ps2pdf14 datapacker.ps

datapacker.txt: datapacker.1
	groff -Tascii -man datapacker.1 | sed $$'s/.\b//g' > datapacker.txt

datapacker.1: datapacker.sgml
	docbook2man datapacker.sgml
	docbook2man datapacker.sgml

clean: clean-code clean-doc

clean-code:
	-./setup clean
	-cd libsrc && ../setup clean
	-rm -rf dist libsrc/dist *.ho *.hi *.o *.a setup *~
	-rm -f `find . -name "*~"` `find . -name "*.o"`
	-rm -f `find . -name "*.cm*"` tarenc tarenc-encoder tarenc-scanner
	-rm -f $(CEXES)

clean-doc:
	-rm -f *.1 *.ps *.pdf *.txt *.links *.refs *.html
