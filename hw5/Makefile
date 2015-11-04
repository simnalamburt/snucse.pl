all: run

run: lexer.cmo parser.cmo sm5.cmo k.cmo translate.cmo pp.cmo main.cmo
	ocamlc -o run lexer.cmo pp.cmo parser.cmo sm5.cmo translate.cmo k.cmo main.cmo

k.cmo : k.ml
	ocamlc -c k.ml

sm5.cmo : sm5.ml
	ocamlc -c sm5.ml

translate.cmo : translate.ml k.cmo sm5.cmo
	ocamlc -c translate.ml

pp.cmo : pp.ml translate.cmo
	ocamlc -c pp.ml

parser.ml: parser.mly translate.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : translate.cmo main.ml
	ocamlc -c main.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
