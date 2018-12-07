OFLAGS = -I src

SRC = src/proto.ml    \
      src/scan.ml     \
      src/parsec.ml   \
      src/validate.ml \
      src/gen_spec.ml \

MLI = $(patsubst %.ml, %.mli, $(SRC))
CMI = $(patsubst %.ml, %.cmi, $(SRC))
CMO = $(patsubst %.ml, %.cmo, $(SRC))

.PHONY: all clean repl

all: gen
repl: src/repl.out
clean:
	rm -f src/*.cmi src/*.cmo
	rm -f src/scan.ml src/proto.mli
	rm -f gen.out src/repl.out
	rm -rf gen

gen: gen.out protocols.txt
	mkdir -p gen
	./gen.out < protocols.txt > gen/spec.md

gen.out: src/main.ml $(SRC)
	cp src/proto.ml src/proto.mli
	ocamlc $(OFLAGS) $(MLI) -c
	ocamlc $(OFLAGS) $(SRC) -c
	ocamlc $(OFLAGS) $(CMO) $< -o $@

src/repl.out: $(CORE_SRC)
	cp src/proto.ml src/proto.mli
	ocamlc     $(OFLAGS) $(MLI) -c
	ocamlc     $(OFLAGS) $(SRC) -c
	ocamlmktop $(OFLAGS) $(CMO) -o $@

src/scan.ml: src/scan.mll
	ocamllex $< -o $@
