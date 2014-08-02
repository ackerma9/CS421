CC=gcc
OCAMLC=ocamlc

OCAML_C_INTERFACE_INCLUDE=/usr/local/lib/ocaml

all: grader.cma

%.cmo: %.ml
	$(OCAMLC) -c -o $@ $?

%.cmi: %.mli
	$(OCAMLC) -c -o $@ $?

test.cmo: grader.cmo util.cmo

grader.cma: grader.cmo test.cmo util.cmo main.cmo
	$(OCAMLC) -a -o $@ -custom grader.cmo unix.cma util.cmo test.cmo main.cmo

clean:
	$(RM) -f *.cm? grader.cma

