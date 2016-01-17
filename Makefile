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
	ocamlfind remove hardcaml-framework

clean: cleanjs
	ocaml setup.ml -clean
	find . -name "*~" | xargs rm -f

distclean: clean
	ocaml setup.ml -distclean

######################################################################
# compile javascript apps

html/hcjs%.js: hcjs%.byte
	js_of_ocaml +nat.js -o $@ $<

html/hcww%.js: hcww%.byte
	js_of_ocaml +nat.js -o $@ $<

WEBAPP = sort rac lfsr cordic mul prefix
WEBAPPJS = $(foreach core, $(WEBAPP), html/hcjs$(core).js html/hcww$(core).js) 

cleanjs:
	rm -f $(WEBAPPJS)

js: all $(WEBAPPJS)

