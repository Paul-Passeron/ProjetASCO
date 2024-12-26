ocamllex lexer.mll
ocamlyacc --explain parser.mly
ocamlc tpscrpt.mli -c
ocamlc parser.mli -c
ocamlc lexer.mli -c
ocamlc -i tpscrpt.ml > tpscrpt.mli
ocamlc -i parser.ml > parser.mli
ocamlc -i lexer.ml > lexer.mli
ocamlc tpscrpt.ml parser.ml lexer.ml main.ml -o main
