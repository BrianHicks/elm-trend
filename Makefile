SRC_FILES = $(wildcard src/*.elm src/**/*.elm)

.PHONY: all
all: documentation.json test

.PHONY: deps
deps: elm-stuff tests/elm-stuff node_modules

.PHONY: clean
clean:
	rm -rf elm-stuff tests/elm-stuff node_modules documentation.json tests/Doc

node_modules: package.json
	npm install
	touch -m $@

elm-stuff: elm-package.json node_modules
	env PATH=node_modules/.bin:${PATH} elm package install --yes
	touch -m $@

documentation.json: elm-stuff ${SRC_FILES} node_modules
	node_modules/.bin/elm-make --docs=$@

.PHONY: test
test: tests/elm-stuff tests/Doc node_modules
	node_modules/.bin/elm-test --compiler node_modules/.bin/elm-make

tests/Doc: ${SRC_FILES} tests/elm-verify-examples.json node_modules
	node_modules/.bin/elm-verify-examples
	touch -m $@

tests/elm-stuff: tests/elm-package.json node_modules
	cd tests; env PATH=../node_modules/.bin:${PATH} elm package install --yes
	touch -m $@
