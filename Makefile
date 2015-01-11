_build/HardCamlExamples.cma:
	ocamlbuild -use-ocamlfind HardCamlExamples.cma

doc:
	ocamlbuild -use-ocamlfind $(BUILD_OPTS) HardCamlExamples.docdir/index.html

clean:
	ocamlbuild -use-ocamlfind -clean
	-rm -fr HardCamlExamples.docdir
