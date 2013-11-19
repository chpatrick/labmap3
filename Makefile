.PHONY: dev prod

dev: dist/build/labmap/labmap
	rm -rf dev
	mkdir -p dev
	ln -s $(shell pwd)/dist/build/labmap/labmap dev/labmap
	ln -s $(shell pwd)/conf/labmap.conf dev/labmap.conf
	ln -s $(shell pwd)/conf/ssh.conf dev/ssh.conf
	ln -s ~/.ssh/id_rsa dev/id_labmap
	ln -s $(shell pwd)/static dev/static
	coffee -w -c -o static js_src/main.coffee

prod: dist/build/labmap/labmap js_src/main.js
	rm -rf prod
	mkdir -p prod
	cp $(shell pwd)/dist/build/labmap/labmap prod/labmap
	cp $(shell pwd)/conf/labmap.conf prod/labmap.conf
	cp $(shell pwd)/conf/ssh.conf prod/ssh.conf
	cp -r $(shell pwd)/static prod/static
	cp js_src/main.js prod/static/main.js

all: js_src/main.js dist/build/labmap/labmap
	mkdir -p dist/pkg
	rm -rf dist/pkg/*
	cp dist/build/labmap/labmap dist/pkg
	cp -r static dist/pkg/static
	cp svg/labmap_plain.svg dist/pkg/static/labmap.svg
	cp js_src/main.js dist/pkg/static
	cp conf/* dist/pkg

js_src/main.js: js_src/main.coffee
	coffee -c -o js_src js_src/main.coffee

dist/build/labmap/labmap: Labmap.hs Labmap/*.hs
	cabal build

