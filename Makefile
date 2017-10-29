.PHONY:
all: documentation.json

elm-stuff: elm-package.json
	elm-package install --yes
	touch -m $@

documentation.json: elm-stuff
	elm make --docs=$@
