eval $(opam env)
find -name "*.ml" -exec ocamlformat --enable-outside-detected-project -i {} \; 2> /dev/null
ocamllex lexer.mll
ocamlyacc -v parser.mly
ocamlc -g tpscrpt.mli -c
ocamlc -g parser.mli -c
ocamlc -g lexer.mli -c
ocamlc -g -i tpscrpt.ml > tpscrpt.mli
ocamlc -g tpscrpt.ml parser.ml lexer.ml main.ml -o main
