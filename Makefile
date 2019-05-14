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

# Incremental rebuild and installs in dist/ with the current settings
# (vs. release which is clean build).
dist:
	stack install --local-bin-path dist/

# An entire clean build and install in dist.
release: clean lint doc regen-git-version
	stack build --pedantic $(FLAGS)
	stack install --local-bin-path dist/ $(FLAGS)
	rsync -a static dist/
	rm -rf dist/static/tmp/

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
	stack clean

test:
	stack test --coverage --flag corydalis:dev

fast-tests:
	stack test --file-watch --flag corydalis:dev

coverage-tests:
	stack test --file-watch --coverage --flag corydalis:dev

lint:
	@rm -f lint-report.html
	hlint \
	  --ignore "Use first" \
	  --ignore "Use &&&" \
	  --report=lint-report.html -c \
	  --cross \
	  --cpp-define=DEVELOPMENT=1 \
	  .

.PHONY: build devel profiling view release doc haddock clean test dist
.INTERMEDIATE: %.ps
