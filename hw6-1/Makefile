all: run

run: parser.cmo sm5.cmo sonata.cmo rozetta.cmo main.cmo
	ocamlc -o run sm5.cmo str.cma parser.cmo sonata.cmo rozetta.cmo main.cmo

sm5.cmo : sm5.ml sm5.mli
	ocamlc -c sm5.mli
	ocamlc -c sm5.ml

sonata.cmo : sonata.ml sonata.mli
	ocamlc -c sonata.mli
	ocamlc -c sonata.ml

rozetta.cmo : rozetta.ml sm5.cmo sonata.cmo
	ocamlc -c rozetta.ml

parser.cmo: parser.ml sm5.cmo
	ocamlc -c parser.ml

main.cmo : sm5.cmo parser.cmo sonata.cmo rozetta.cmo main.ml
	ocamlc -c main.ml

clean:
	rm -f *.cmx *.cmi run *.o *.cmo
