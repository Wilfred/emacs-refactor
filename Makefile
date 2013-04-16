emacs = emacs
testd = $(abspath test)/
test-runner = $(testd)test-runner.el

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
test :
	$(emacs) --version
	$(emacs) --batch -l $(test-runner) -f run-tests
