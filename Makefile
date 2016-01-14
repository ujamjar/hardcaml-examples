.PHONY: all install uninstall clean

all: setup.data
	ocaml setup.ml -build

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure 

doc: setup.ml
	ocaml setup.ml -doc

install: all
	ocaml setup.ml -install

uninstall: 
	ocamlfind remove hardcaml-examples

clean:
	ocaml setup.ml -clean
	find . -name "*~" | xargs rm -f

distclean: clean
	ocaml setup.ml -distclean

js:
	ocamlbuild -use-ocamlfind \
		hcjssort.byte hcwwsort.byte 
	js_of_ocaml +nat.js hcjssort.byte
	js_of_ocaml +nat.js hcwwsort.byte


