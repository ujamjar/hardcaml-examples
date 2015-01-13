_build/HardCamlExamples.cma:
	ocamlbuild -use-ocamlfind HardCamlExamples.cma

hcrac.byte:
	ocamlbuild -use-ocamlfind hcrac.byte

doc:
	ocamlbuild -use-ocamlfind $(BUILD_OPTS) HardCamlExamples.docdir/index.html

clean:
	ocamlbuild -use-ocamlfind -clean
	-rm -fr HardCamlExamples.docdir
