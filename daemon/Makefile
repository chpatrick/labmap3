dev: dist/build/labmap/labmap
	mkdir -p dev
	ln -s $(shell pwd)/dist/build/labmap/labmap dev/labmap
	ln -s $(shell pwd)/conf/labmap.conf dev/labmap.conf
	ln -s $(shell pwd)/conf/ssh.conf dev/ssh.conf
	ln -s $(shell pwd)/static dev/static
	coffee -w -c -o static js_src/main.coffee
	rm -rf dev

all: js_src/main.js dist/build/labmap/labmap
	mkdir -p dist/pkg
	rm -rf dist/pkg/*
	cp dist/build/labmap/labmap dist/pkg
	cp -r static dist/pkg/static
	cp svg/labmap_plain.svg dist/pkg/static/labmap.svg
	cp js_src/main.js dist/pkg/static
	cp conf/* dist/pkg

js_src/main.js:
	coffee -c -o js_src js_src/main.coffee

dist/build/labmap/labmap:
	cabal build