ocamlc -c m.ml
ocamlc -c pp.ml
ocamlc -c error.ml
ocamlyacc parser.mly
ocamllex lexer.mll
ocamlc -c parser.mli
ocamlc -c parser.ml
ocamlc -c lexer.ml
ocamlc -c poly_checker.mli
ocamlc -c poly_checker.ml
ocamlc -c main.ml
ocamlc -o run.exe m.cmo pp.cmo error.cmo parser.cmo lexer.cmo poly_checker.cmo main.cmo
