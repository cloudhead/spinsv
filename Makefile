
CABAL := cabal

build: configure
	$(CABAL) build

dist:
	$(CABAL) configure

configure: ./dist
