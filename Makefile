# this is only used for custom development!
# well, except for the production target

build:
	cabal build

devel:
	yesod devel

profiling:
	# until https://github.com/haskell/cabal/issues/193 is fixed,
	# we need custom ghc options
	cabal configure \
	  --enable-executable-profiling \
	  --enable-library-profiling \
	  --ghc-options=-auto-all
	cabal build -j
	@echo done

%.ps: %.hp
	hp2ps -c $<

view: corydalis.ps
	gv -orientation seascape $<

production:
	cabal configure -fproduction
	cabal build -j

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

run:
	@cabal build
	@echo Running...
	@./dist/build/corydalis/corydalis

.PHONY: build devel profiling view production doc clean test
