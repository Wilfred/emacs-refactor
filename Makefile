CASK  ?= cask
EMACS ?= emacs

EMACS_CMD  = $(EMACS) --batch -q -l package
EMACS_D    = $(shell $(EMACS_CMD) --eval '(princ (expand-file-name user-emacs-directory))')
VERSION    = $(shell $(CASK) version)

PACKAGE_DIR = emr-$(VERSION)
PACKAGE_TAR = $(abspath emr-$(VERSION).tar)
MANIFEST    = $(abspath emr-pkg.el)
SRCS        = $(filter-out $(wildcard *-pkg.el), $(wildcard *.el))
PACKAGE_INCLUDES = $(SRCS) $(MANIFEST)

LOAD_EL     = $(patsubst %,-l %, $(SRCS))
TEST_D      = $(abspath ./test)
TEST_RUNNER = $(abspath $(TEST_D)/test-runner.el)

# ============================================================================

.PHONY: default
default : uninstall elpa install clean-package

# Installs the package to .emacs.d/elpa
.PHONY : install
install : package
	$(EMACS_CMD) -f package-initialize \
		--eval "(package-install-file \"$(PACKAGE_TAR)\")"

# Deletes all installed instances in .emacs.d/elpa
.PHONY : uninstall
uninstall :
	rm -rf $(EMACS_D)elpa/emr-*

# Install package dependencies.
elpa :
	$(CASK) install

.PHONY : deps
deps : elpa
	$(CASK) update

# ----------------------------------------------------------------------------
# Cleaning tasks

.PHONY: clean
clean : clean-elc clean-deps clean-package clean-tests

.PHONY: clean-elc
clean-elc :
	rm -f *.elc

.PHONY: clean-tests
clean-tests :
	rm -f $(TEST_D)/*.elc

.PHONY: clean-deps
clean-deps :
	rm -rf elpa

.PHONY: clean-package
clean-package :
	rm -rf $(PACKAGE_DIR) $(MANIFEST) $(PACKAGE_TAR)

# ----------------------------------------------------------------------------
# Build tasks

# Create a package tar and clean up.
.PHONY: package
package : clean-package $(MANIFEST) $(PACKAGE_INCLUDES)
	mkdir -p  $(PACKAGE_DIR)
	cp    -f  $(PACKAGE_INCLUDES) $(PACKAGE_DIR)
	tar   cf  $(PACKAGE_TAR) $(PACKAGE_DIR)
	rm    -rf $(PACKAGE_DIR)

# Generate package file
$(MANIFEST) :
	$(CASK) package
	mv emacs-refactor-pkg.el $(MANIFEST)

# Byte-compile Elisp files
%.elc : .%el
	$(CASK) exec $(EMACS_CMD) $(LOAD_EL) -f batch-byte-compile $<

# ----------------------------------------------------------------------------
# Tests

.PHONY: test
test : elpa
	$(CASK) exec $(EMACS_CMD) -Q --no-site-lisp --script $(TEST_RUNNER)
