
CABAL := cabal

build:
	$(CABAL) build

static:
	ghc \
	  --make src/Main.hs \
	  -static \
	  -optl-static \
	  -optl-pthread \
	  -fforce-recomp \
	  -o bin/spinsv
