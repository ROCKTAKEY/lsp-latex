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
	${MAKE} latex
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

latex:
	sudo apt update
	sudo apt install -y texlive-full

test-all:
	${MAKE} detect-jar
	${MAKE} clean
	${MAKE} ert
	${MAKE} compile
	${MAKE} ert
	${MAKE} clean

detect-jar:
	${eval TEXLAB-JAR := \
	$(shell ${CASK} exec ${EMACS} -batch -Q -L . -l $(wildcard ${TESTEDFILES}) \
	--eval \
	"(add-to-list 'exec-path \"~/\") (princ (lsp-latex-get-texlab-jar-file))")}
	echo ${TEXLAB-JAR}
	java -jar ${TEXLAB-JAR} < test/inputs

.PHONY: ert travis compile clean texlab latex test-all
