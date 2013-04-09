emacs = emacs
testd = $(abspath test)/
tmpd  = $(abspath tmp)/


test-runner = $(testd)test-runner.el

# ============================================================================

.PHONY : test clean clean-elc clean-flycheck

all : test

# Cleaning tasks.
clean : clean-elc clean-flycheck clean-tmp
clean-elc :
	rm -f *.elc
	rm -f $(testd)*.elc
clean-flycheck :
	rm -f *flycheck
	rm -f $(testd)*flycheck
clean-tmp :
	rm -r $(tmpd)

# Run unit tests.
test : $(tmpd).emacs.d/elpa
	$(emacs) --version
	HOME=$(tmpd) ;\
	$(emacs) --batch -l $(test-runner) -f run-tests

# Download package dependencies.
$(tmpd).emacs.d/elpa :
	HOME=$(tmpd) ;\
	$(emacs) --batch -l $(test-runner) -f load-packages

# Create dir to hold test .emacs.d
$(tmpd) :; makedir -p $(tmpd)
