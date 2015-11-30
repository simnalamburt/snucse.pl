ocamlc -c xexp.mli
ocamlc -c xexp.ml

ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c parser.ml

ocamllex lexer.mll
ocamlc -c lexer.ml

ocamlc -c desugar.ml

ocamlc -c main.ml
ocamlc -o run.exe xexp.cmo lexer.cmo parser.cmo desugar.cmo main.cmo
