# this is only used for custom development!

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

.PHONY: standard profiling view
