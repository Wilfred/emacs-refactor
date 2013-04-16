emacs  = emacs
carton = carton
testd  = $(abspath test)/
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
	$(carton) exec $(emacs) -Q --no-site-lisp --script $(test-runner)
