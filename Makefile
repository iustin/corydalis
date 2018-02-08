# this is only used for custom development!
# well, except for the release target

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

# Incremental rebuild and installs in dist/ with the current settings
# (vs. release which is clean build).
dist:
	stack install --local-bin-path dist/

# An entire clean build and install in dist.
release: clean lint doc
	stack build --pedantic
	stack install --local-bin-path dist/
	mkdir -p dist/static/
	rsync -a static/css static/fonts static/js dist/static/

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
	rm -rf site/
	stack clean

test:
	stack test --coverage

lint:
	@rm -f lint-report.html
	hlint \
	  -j \
	  --ignore "Use first" \
	  --ignore "Use &&&" \
	  --report=lint-report.html -c \
	  --cross \
	  --cpp-define=DEVELOPMENT=1 \
	  .

.PHONY: build devel profiling view release doc haddock clean test dist
.INTERMEDIATE: %.ps
