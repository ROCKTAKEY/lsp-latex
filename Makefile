EMACS ?= emacs
TESTINGFILE := test/*.el
TESTEDFILES := lsp-latex.el
CASK ?= cask
CURL ?= curl
GIT ?= git

ert:
	${CASK} exec ${EMACS} -batch -Q -L . -l $(wildcard ${TESTINGFILE}) \
	-f  ert-run-tests-batch-and-exit

travis:
	${MAKE} texlab
	${MAKE} test-all

compile:
	${CASK} exec ${EMACS} -batch -Q -L . -eval "(batch-byte-compile)" \
	${TESTEDFILES}

clean:
	rm -f ${addsuffix c, ${TESTEDFILES}}

texlab:
	${CURL} -o ~/texlab.jar \
	"https://github.com/latex-lsp/texlab/releases/download/v0.4.1/texlab.jar"

test-all:
	${MAKE} clean
	${MAKE} ert
	${MAKE} compile
	${MAKE} ert
	${MAKE} clean

.PHONY: easy-test test travis compile clean
