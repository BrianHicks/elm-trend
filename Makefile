.PHONY: all
all: documentation.json

.PHONY: deps
deps: elm-stuff tests/elm-stuff

elm-stuff: elm-package.json
	elm-package install --yes
	touch -m $@

documentation.json: elm-stuff $(wildcard src/*.elm)
	elm make --docs=$@

.PHONY: test
test: tests/elm-stuff
	elm test

tests/elm-stuff: tests/elm-package.json
	cd tests; elm-package install --yes
	touch -m $@
