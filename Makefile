
CABAL := cabal

build:
	$(CABAL) build

test:
	$(CABAL) test

static:
	ghc \
	  --make src/Runner/Runner.hs \
	  -static \
	  -optl-static \
	  -optl-pthread \
	  -fforce-recomp \
	  -o bin/spinsv

.PHONY: test
