all: run

run: lexer.cmo parser.cmo xexp.cmo desugar.cmo main.cmo
	ocamlc -o run xexp.cmo lexer.cmo parser.cmo desugar.cmo main.cmo

xexp.cmo : xexp.ml xexp.mli
	ocamlc -c xexp.mli
	ocamlc -c xexp.ml

desugar.cmo : desugar.ml xexp.cmo
	ocamlc -c desugar.ml

parser.ml: parser.mly xexp.cmo 
	ocamlyacc parser.mly

parser.mli: parser.mly
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

main.cmo : main.ml xexp.cmo desugar.cmo parser.cmo
	ocamlc -c main.ml

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

lexer.ml: lexer.mll parser.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmo *.cmi *.cmx  parser.mli parser.ml lexer.ml run *.o
