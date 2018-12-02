# TODO: test the opt versions for speed
OC=ocamlc
OO=ocamlopt
OL=ocamllex
OFLAGS=-I lib -I src

CORE_I=lib/proto.cmi    \
       lib/scan.cmi     \
       lib/parsec.cmi   \
       lib/validate.cmi \
       lib/gen_spec.cmi \

CORE_O=lib/scan.cmo     \
       lib/parsec.cmo   \
       lib/validate.cmo \
       lib/gen_spec.cmo \

CMI=$(CORE_I) \
    lib/main.cmi

CMO=$(CORE_O) \
    lib/main.cmo

.PHONY: all clean

all: gen repl.out

clean:
	rm -rf lib repl.out gen.out

gen: gen.out
	mkdir -p gen
	./gen.out < protocols.txt > gen/spec.md

gen.out: $(CMO)
	$(OC) $(OFLAGS) $^ -o $@

repl.out: $(CORE_O)
	ocamlmktop $(OFLAGS) $^ -o $@

lib/proto.cmi   : src/proto.mli
lib/scan.cmi    : src/scan.mli     lib/proto.cmi
lib/parsec.cmi  : src/parsec.mli   lib/proto.cmi
lib/validate.cmi: src/validate.mli lib/proto.cmi
lib/gen_spec.cmi: src/gen_spec.mli lib/proto.cmi
lib/main.cmi    : src/main.cmi     $(CORE_I)

lib/scan.cmo    : lib/scan.ml     lib/scan.cmi     lib/proto.cmi
lib/parsec.cmo  : src/parsec.ml   lib/parsec.cmi   lib/proto.cmi
lib/validate.cmo: src/validate.ml lib/validate.cmi lib/proto.cmi
lib/gen_spec.cmo: src/gen_spec.ml lib/gen_spec.cmi lib/proto.cmi
lib/main.cmo    : src/main.ml     $(CORE_O)

$(CMI) $(CMO):
	@mkdir -p $(@D)
	$(OC) $(OFLAGS) -c $< -o $@

lib/scan.ml: src/scan.mll
	@mkdir -p $(@D)
	$(OL) $< -o $@
