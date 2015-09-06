PACKAGE = gh-api-lab
VERSION = $$(grep "^;; Version: " $(PACKAGE).el | cut -f3 -d' ')
ARCHIVE = $(PACKAGE)-$(VERSION).tar
EMACS ?= emacs
CASK ?= cask

.PHONY: clean

pr:
	hub pull-request -b gh-api-lab:master

deps:
	${CASK}

build:
	${CASK} build

clean-dist:
	rm -rf dist/

clean: clean-dist
	rm -rf *.tar
	${CASK} clean-elc

install:
	${CASK} install

test: clean
	${CASK} exec ert-runner

pkg-file:
	${CASK} pkg-file

pkg-el: pkg-file
	${CASK} package

package: clean pkg-el
	cp dist/$(ARCHIVE) .
	make clean-dist

info:
	${CASK} info

install-package-from-melpa:
	./install-package-from.sh melpa

install-file-with-deps-from-melpa: package
	./install-file-with-deps-from.sh melpa $(VERSION)

release:
	./release.sh $(VERSION) $(PACKAGE)

version:
	@echo "application $(PACKAGE): $(VERSION)\npackage: $(ARCHIVE)"

install-cask:
	curl -fsSkL https://raw.github.com/cask/cask/master/go | python

emacs-install-clean: package
	~/bin/emacs/emacs-install-clean.sh ./$(ARCHIVE)

respect-convention:
	./contrib/respect-elisp-conventions.sh

tests:
	./run-tests.sh 24.3-bin 24.4-bin 24.5-bin
