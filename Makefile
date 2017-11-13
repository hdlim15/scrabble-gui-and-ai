compile:
	ocamlbuild -use-ocamlfind state.cmo main.cmo command.cmo ai.cmo trie.cmo

test:
	ocamlbuild -use-ocamlfind test_main.byte && ./test_main.byte
