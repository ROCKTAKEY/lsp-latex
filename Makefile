EMACS ?= emacs
TESTINGFILE := test/*.el
TESTEDFILES := *.el
KEG ?= keg
WGET ?= wget
GIT ?= git

ert:
	${KEG} exec ${EMACS} --batch -Q -L . -l $(wildcard ${TESTINGFILE}) \
	-f  ert-run-tests-batch-and-exit

travis:
	${MAKE} test-all

compile:
	${KEG} exec ${EMACS} --batch -Q -L . -eval "(batch-byte-compile)" \
	${TESTEDFILES}

clean:
	rm -f ${addsuffix c, ${TESTEDFILES}}

test-all:
	${MAKE} clean
	${MAKE} ert
	${MAKE} compile
	${MAKE} ert
	${MAKE} clean

lint:
	${KEG} lint

.PHONY: ert travis compile clean  test-all lint
