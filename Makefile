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
	rm -fr $(tmpd)

# Run unit tests.
test : $(tmpd)
	$(emacs) --version
	HOME=$(tmpd) ;\
	$(emacs) --batch -l $(test-runner) -f run-tests

# Create dir to hold test .emacs.d
$(tmpd) :; mkdir -p $(tmpd)
