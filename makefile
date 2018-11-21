# TODO: test the opt versions for speed
OC=ocamlc
OO=ocamlopt
OL=ocamllex
OFLAGS=-I lib -I src

CMI=lib/proto.cmi    \
    lib/scan.cmi     \
    lib/parsec.cmi   \
    lib/validate.cmi \

CMO=lib/scan.cmo     \
    lib/parsec.cmo   \
    lib/validate.cmo \

.PHONY: all clean

clean:
	rm -rf lib repl.out

repl.out: $(CMO)
	ocamlmktop $(OFLAGS) $^ -o $@

lib/proto.cmi   : src/proto.mli
lib/scan.cmi    : src/scan.mli     lib/proto.cmi
lib/parsec.cmi  : src/parsec.mli   lib/proto.cmi
lib/validate.cmi: src/validate.mli lib/proto.cmi

lib/scan.cmo    : lib/scan.ml     lib/scan.cmi     lib/proto.cmi
lib/parsec.cmo  : src/parsec.ml   lib/parsec.cmi   lib/proto.cmi
lib/validate.cmo: src/validate.ml lib/validate.cmi lib/proto.cmi

$(CMI) $(CMO):
	@mkdir -p $(@D)
	$(OC) $(OFLAGS) -c $< -o $@

lib/scan.ml: src/scan.mll
	@mkdir -p $(@D)
	$(OL) $< -o $@
