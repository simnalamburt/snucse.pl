all: run

run: m.cmo error.cmo lexer.cmo parser.cmo main.cmo
	ocamlc -o run m.cmo error.cmo lexer.cmo parser.cmo pp.cmo main.cmo

error.cmo : error.ml m.cmo 
	ocamlc -c error.ml

m.cmo : m.ml
	ocamlc -c m.ml

pp.cmo : pp.ml m.cmo
	ocamlc -c pp.ml

parser.ml: parser.mly m.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly m.cmo
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : main.ml m.cmo pp.cmo
	ocamlc -c main.ml

lexer.cmo: lexer.ml error.cmo
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo m.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
