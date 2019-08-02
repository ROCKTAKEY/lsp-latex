EMACS ?= emacs
TESTINGFILE := test/*.el
TESTEDFILES := lsp-latex.el
CASK ?= cask
WGET ?= wget
GIT ?= git

ert:
	${CASK} exec ${EMACS} -batch -Q -L . -l $(wildcard ${TESTINGFILE}) \
	-f  ert-run-tests-batch-and-exit

travis:
	${MAKE} latex
	${MAKE} texlab-old
	${MAKE} test
	${MAKE} clean-texlab
	${MAKE} texlab
	${MAKE} test

compile:
	${CASK} exec ${EMACS} -batch -Q -L . -eval "(batch-byte-compile)" \
	${TESTEDFILES}

clean:
	rm -f ${addsuffix c, ${TESTEDFILES}}

texlab:
	${WGET} -O ~/texlab.tar.gz \
	"https://github.com/latex-lsp/texlab/releases/download/v1.0.0/texlab-x86_64-linux.tar.gz"
	tar -zxvf ~/texlab.tar.gz

clean-texlab:
	rm -f texlab*

latex:
	sudo apt update
	sudo apt install -y texlive-full

test:
	${MAKE} clean
	${MAKE} ert
	${MAKE} compile
	${MAKE} ert
	${MAKE} clean


# For old

texlab-old:
	${WGET} -O ~/texlab.jar \
	"https://github.com/latex-lsp/texlab/releases/download/v0.4.2/texlab.jar"


detect-jar:
	${eval TEXLAB-JAR := \
	$(shell ${CASK} exec ${EMACS} -batch -Q -L . -l $(wildcard ${TESTEDFILES}) \
	--eval \
	"(progn (add-to-list 'exec-path \"~/\") \
	(princ (lsp-latex-get-texlab-jar-file)))")}
	echo ${TEXLAB-JAR}
	java -jar ${TEXLAB-JAR} < test/inputs



# PHONY
.PHONY: ert travis compile clean clean-texlab texlab-old texlab latex test detect-jar
