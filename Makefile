# this is only used for custom development!
# well, except for the production target

standard:
	yesod devel

profiling:
	ghc $(EXTENSIONS) $(FLAGS) \
	  -osuf prof_o -prof -auto-all \
	  --make \
	  $(MAIN)
	@echo done

%.ps: %.hp
	hp2ps -c $<

view: main.ps
	gv -orientation seascape $<

production:
	cabal configure -fproduction
	cabal build

doc:
	cabal haddock --internal --haddock-options=--ignore-all-exports

.PHONY: standard profiling view production doc
