# this is only used for custom development!
# well, except for the production target

standard:
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
	  corydalis.hp corydalis.pdf
	cabal clean

.PHONY: standard profiling view production doc clean
