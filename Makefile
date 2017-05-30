.PHONY: all build clean tag prepare publish

all: build

build:
	jbuilder build @install

clean: 
	rm -rf _build
	find . -name "*~" | xargs rm -f

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

tag:
	git tag -a "v$(VERSION)" -m "v$(VERSION)."
	git push origin v$(VERSION)

prepare:
	opam publish prepare -r hardcaml $(NAME_VERSION) $(ARCHIVE)

publish:
	opam publish submit -r hardcaml $(NAME_VERSION)
	rm -rf $(NAME_VERSION)


