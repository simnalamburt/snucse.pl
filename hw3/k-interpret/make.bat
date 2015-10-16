ocamlc -c k.ml
ocamlc -c pp.ml
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c main.ml
ocamlc -o run.exe pp.cmo parser.cmo lexer.cmo k.cmo main.cmo
