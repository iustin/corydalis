# this is only used for custom development!
# well, except for the production target

EXTENSIONS = \
	-XTemplateHaskell \
	-XQuasiQuotes \
	-XCPP \
	-XOverloadedStrings \
	-XMultiParamTypeClasses \
	-XTypeFamilies \
	-XGADTs \
	-XGeneralizedNewtypeDeriving

FLAGS = \
	-funbox-strict-fields \
	-rtsopts \
	-O2

MAIN = app/main.hs

standard:
	ghc $(EXTENSIONS) $(FLAGS) --make $(MAIN)
	@echo done

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
