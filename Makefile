# this is only used for custom development!
# well, except for the release target

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

release:
	stack clean
	stack build
	stack install --local-bin-path dist/
	mkdir -p dist/static/
	rsync -a static/css static/fonts static/js dist/static/

doc:
	cabal haddock --internal --haddock-options=--ignore-all-exports

clean:
	rm -f corydalis.aux corydalis.prof corydalis.ps \
	  corydalis.hp corydalis.pdf *.html
	find \( -name '*.hi' -o -name '*.o' \) -delete
	cabal clean

test:
	cabal configure --enable-test --enable-coverage
	cabal test

lint:
	@rm -f lint-report.html
	@hlint \
	  --ignore "Use fewer imports" \
	  --ignore "Use first" \
	  --ignore "Use &&&" \
	  --report=lint-report.html -c \
	  .

.PHONY: build devel profiling view release doc clean test
