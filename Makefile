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
	node_modules/.bin/elm-package install --yes
	touch -m $@

documentation.json: elm-stuff $(wildcard src/*.elm) node_modules
	node_modules/.bin/elm-make --docs=$@

.PHONY: test
test: tests/elm-stuff tests/Doc node_modules
	node_modules/.bin/elm-test --compiler node_modules/.bin/elm-make

tests/Doc: $(wildcard src/*.elm) tests/elm-verify-examples.json node_modules
	node_modules/.bin/elm-verify-examples
	touch -m $@

tests/elm-stuff: tests/elm-package.json node_modules
	cd tests; ../node_modules/.bin/elm-package install --yes
	touch -m $@
