execpath := $(shell cabal list-bin cl-player)
start:
	cabal build
	-rm -r build
	mkdir build
	cp $(execpath) build
	cp -r static build
	npm start
