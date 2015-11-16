all:
	ocamlbuild -package graphics -package unix main.native

clean:
	ocamlbuild -clean
