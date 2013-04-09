emacs = emacs
testd = $(abspath test)/
tmpd  = $(abspath tmp)/


src         = elr.el elr-elisp.el
tests       = $(wildcard $(testd)*tests.el)
load_files  = $(patsubst %,-l %, $(src))
load_tests  = $(patsubst %,-l %, $(tests))

# ============================================================================

.PHONY : test clean clean-elc clean-flycheck

all : test

# Cleaning tasks.
clean : clean-elc clean-flycheck
clean-elc :
	rm -f *.elc
	rm -f $(testd)*.elc
clean-flycheck :
	rm -f *flycheck
	rm -f $(testd)*flycheck

# Run unit tests.
test : $(tmpd).emacs.d/elpa
	$(emacs) --version
	HOME=$(tmpd) ;\
	$(emacs) --batch $(load_files) $(load_tests) -f run-tests

# Download package dependencies.
$(tmpd).emacs.d/elpa :
	HOME=$(tmpd) ;\
	$(emacs) --batch -l $(testd)test-runner.el -f load-packages

# Create dir to hold test .emacs.d
$(tmpd) :; makedir -p $(tmpd)
