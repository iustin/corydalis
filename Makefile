# this is only used for custom development!
# well, except for the release target

SHELL=/bin/bash

all: build

build:
	stack build

devel:
	HOST=127.0.0.1 yesod devel --host 127.0.0.1

profiling:
	stack build --profile --flag corydalis:dev

%.ps: %.hp
	hp2ps -c $<

%.pdf: %.ps
	ps2pdf $<

view: corydalis.ps
	gv -orientation seascape $<

%.html: %.md
	pandoc -s -o $@ $<

this-week:
	date +'%Y.%V'
.PHONY: this-week

git-version:
	if test -d .git; then \
	  git describe --dirty --always > $@ ; \
	elif test ! -f $@ ; then \
	  echo "Error: cannot generate the '$@' file!"; exit 1; \
	fi

.PHONY: clean-git-version
clean-git-version:
	rm -f git-version

.PHONY: regen-git-version
regen-git-version: clean-git-version git-version

# Target to use after cloning from git
.PHONY: bootstrap
bootstrap:
	@echo Installing npm dependencies...
	npm install
	@echo Fixing font-awesome CSS files...
	bash build/patch-font-awesome
	@echo One-shot converting typescript to JS...
	npx tsc -p js/ --listEmittedFiles

# Continuous typescript compilation
.PHONY: ts-watch
ts-watch:
	npx tsc -w -p js/

# Incremental rebuild and installs in dist/ with the current settings
# (vs. release which is clean build).
dist:
	stack --work-dir .stack-release install --local-bin-path dist/

# An entire clean build and install in dist.
release: clean lint doc regen-git-version bootstrap
	rm -rf .stack-release
	stack --work-dir .stack-release build --pedantic $(FLAGS)
	stack --work-dir .stack-release install --local-bin-path dist/ $(FLAGS)
	rsync -aL static dist/
	rm -rf dist/static/tmp/
	mkdir dist/static/tmp/

.PHONY: demo-release
demo-release:
	$(MAKE) release FLAGS="--flag corydalis:public-site"

doc:
	mkdocs build -s

haddock:
	cabal configure \
	  --package-db=clear \
	  --package-db=global \
	  --package-db=$$(stack path --snapshot-pkg-db) \
	  --package-db=$$(stack path --local-pkg-db)
	cabal haddock --internal --haddock-options=--ignore-all-exports

clean:
	rm -rf site/ dist/
	rm -f static/tmp/* static/combined/*

.PHONY: really-clean
really-clean: clean
	rm -rf .stack-*

test:
	stack --work-dir .stack-coverage test --coverage --flag corydalis:dev
	STACK_WORK=.stack-coverage hpc-lcov

fast-tests:
	# Fast tests with ghc options from
	# https://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/
	stack --work-dir .stack-test test --file-watch --flag corydalis:dev \
		--ghc-options="-j +RTS -A128m -n2m -RTS"

fast-build:
	stack --work-dir .stack-test build --file-watch --flag corydalis:dev

coverage-tests:
	stack --work-dir .stack-coverage test --file-watch --coverage --flag corydalis:dev

lint:
	@rm -f lint-report.html
	hlint \
	  --cross \
	  --report=lint-report.html -c \
	  --cpp-define=DEVELOPMENT=1 \
	  src/ test/

lentil:
	lentil src static/corydalis templates test

.PHONY: build devel profiling view release doc haddock clean test dist
.INTERMEDIATE: %.ps
