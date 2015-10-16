all: run

run: lexer.cmo parser.cmo k.cmo pp.cmo main.cmo
	ocamlc -o run lexer.cmo pp.cmo parser.cmo k.cmo main.cmo

k.cmo : k.ml
	ocamlc -c k.ml

pp.cmo : pp.ml k.cmo
	ocamlc -c pp.ml

parser.ml: parser.mly k.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : k.cmo main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
