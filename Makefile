emacs = emacs

all: test

clean:
	rm -f *.elc
	rm -f *flycheck
	rm -f tests/*.elc
	rm -f tests/*flycheck

test:
	$(emacs) --version
	$(emacs) --batch -l elr.el -l elr-elsp.el -l tests/elr-elisp-tests.el  -f ert-run-tests-batch-and-exit
