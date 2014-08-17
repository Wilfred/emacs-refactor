CASK        ?= cask
EMACS       ?= emacs
DIST        ?= dist
CASK_DIR    ?= .cask
EMACSFLAGS   = --batch -Q -L . -L test

VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)

EMACS_D      = ~/.emacs.d
USER_ELPA_D  = $(EMACS_D)/elpa

SRCS        = $(filter-out %-pkg.el, $(wildcard *.el))
TESTS       = $(wildcard test/*.el)

DIST_SRCS   = $(patsubst %,$(DIST)/% , $(SRCS))
DIST_PKG    = $(DIST)/emr-pkg.el
DIST_README = $(DIST)/emr-readme.txt
DIST_TAR    = $(DIST)/emr-$(VERSION).tar


.PHONY: all check install uninstall reinstall clean-all clean
all : $(PKG_DEPS) $(DIST_TAR)

$(PKG_DEPS) :
	$(CASK) install

check : $(PKG_DEPS)
	$(CASK) exec $(EMACS) $(EMACSFLAGS)  \
	$(patsubst %,-l % , $(SRCS) $(TESTS))\
	-f ert-run-tests-batch-and-exit

install : $(DIST_TAR)
	$(EMACS) $(EMACSFLAGS) -l package \
	-f package-initialize  --eval '(package-install-file "$(DIST_TAR)")'

uninstall :
	rm -rf $(USER_ELPA_D)/emr-*

reinstall : clean uninstall install

clean-all : clean
	rm -rf $(PKG_DEPS)

clean :
	$(CASK) clean-elc
	rm -f *.elc
	rm -rf $(DIST)
	rm -f $(INFO_MANUAL)
	rm -f *-pkg.el

$(DIST_TAR) : $(DIST_README) $(DIST_PKG) $(DIST_SRCS)
	tar -cvf $@ -C $(DIST) --exclude $(@F) .

$(DIST_README) :
	$(CASK) package $(DIST)

$(DIST_SRCS) : $(DIST)
	cp -f $(@F) $@

$(DIST_PKG) : $(DIST)
	cask pkg-file
	cp -f $(@F) $@

$(DIST) :
	mkdir $(DIST)
