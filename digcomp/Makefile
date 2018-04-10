.POSIX:
.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .mll .mly
.PHONY: all doc clean distclean


OCAMLC=ocamlc
OCAMLFLAGS=-w A
LIBS=
OBJS=asm.cmo parser.cmo lexer.cmo digcomp.cmo
BIN=digcomp



all: $(BIN)

doc: asm.mli asm.ml lexer.ml parser.ml digcomp.ml
	@if [ ! -e ./doc ] ; then mkdir doc ; fi
	@ocamldoc -d doc -html asm.mli asm.ml lexer.ml parser.ml digcomp.ml

$(BIN): $(OBJS)
	$(OCAMLC) $(OCAMLFLAGS) $(LIBS) $(OBJS) -o $@


.mll.ml:
	ocamllex $< -q -o $@

.mly.ml:
	ocamlyacc $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@


clean:
	@rm -vf *.cm[io] *~ .*~ parser.ml parser.mli lexer.ml lexer.mli

distclean: clean
	@rm -fr doc $(BIN)

parser.mli: parser.ml
asm.cmo: asm.cmi
parser.cmo: asm.cmi parser.cmi
lexer.cmi: lexer.ml
lexer.cmo: parser.cmi lexer.cmi
digcomp.cmo: asm.cmi parser.cmi lexer.cmo
