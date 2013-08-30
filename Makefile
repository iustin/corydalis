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
	-O2

MAIN = app/main.hs

standard:
	ghc $(EXTENSIONS) $(FLAGS) --make $(MAIN)

profiling:
	ghc $(EXTENSIONS) $(FLAGS) \
	  -osuf prof_o -prof -auto-all \
	  --make \
	  $(MAIN)

.PHONY: standard profiling
