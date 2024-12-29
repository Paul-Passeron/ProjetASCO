ocamllex lexer.mll
ocamlyacc -v parser.mly
ocamlc tpscrpt.mli -c
ocamlc parser.mli -c
ocamlc lexer.mli -c
ocamlc -i tpscrpt.ml > tpscrpt.mli
ocamlc tpscrpt.ml parser.ml lexer.ml main.ml -o main
