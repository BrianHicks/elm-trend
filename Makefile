SRC_FILES = $(wildcard src/*.elm src/**/*.elm)

.PHONY: all
all: documentation.json test

.PHONY: clean
clean:
	rm -rf elm-stuff node_modules documentation.json tests/Doc

node_modules: package.json
	npm install
	touch -m $@

documentation.json: ${SRC_FILES} node_modules
	node_modules/.bin/elm make --docs=$@

.PHONY: test
test: tests/Doc node_modules
	# Cleanup the tests after elm-verify-examples to work around
	# https://github.com/stoeffel/elm-verify-examples/issues/74
	rm -rf elm-stuff/generated-code/elm-explorations/test/elm-stuff
	node_modules/.bin/elm-test --compiler node_modules/.bin/elm

tests/Doc: ${SRC_FILES} tests/elm-verify-examples.json node_modules
	rm -rf $@
	node_modules/.bin/elm-verify-examples --compiler node_modules/.bin/elm
	touch -m $@
