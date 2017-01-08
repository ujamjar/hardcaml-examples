.PHONY: all clean

all: build

build:
	ocaml pkg/pkg.ml build

clean: cleanjs
	ocaml pkg/pkg.ml clean
	find . -name "*~" | xargs rm -f

