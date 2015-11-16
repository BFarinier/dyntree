all:
	ocamlbuild -package graphics main.native

clean:
	ocamlbuild -clean
