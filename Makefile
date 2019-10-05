# EMACS_VERSION should be set in your ~/.profile on your development machine
EMACS_VERSION         ?= 26.1
EMAKE_SHA1            ?= 2dec3626e073c3b4aa88fbedd7ad5d27e530f470
PACKAGE_BASENAME      := ivy-omni-org

# override defaults
PACKAGE_ARCHIVES      := gnu melpa
PACKAGE_TEST_DEPS     := dash
PACKAGE_TEST_ARCHIVES := gnu melpa

.DEFAULT_GOAL: help

clean:
	rm -rf $(EMAKE_WORKDIR)
	rm -f $(PACKAGE_LISP:.el=.elc)

include emake.mk
