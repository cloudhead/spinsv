
CABAL := cabal

build:
	$(CABAL) build

test:
	$(CABAL) test

static:
	mkdir -p bin
	ghc \
	  --make src/Runner/Runner.hs src/Runner/Main.hs \
	  -static \
	  -optl-static \
	  -optl-pthread \
	  -fforce-recomp \
	  -o bin/spinsv

.PHONY: test
