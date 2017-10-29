.PHONY: all
all: documentation.json

elm-stuff: elm-package.json
	elm-package install --yes
	touch -m $@

documentation.json: elm-stuff $(wildcard src/*.elm)
	elm make --docs=$@

.PHONY: test
test: elm-package.json
	elm test
