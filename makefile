OFLAGS = -I src

SRC = src/utils.ml    \
      src/proto.ml    \
      src/scan.ml     \
      src/parsec.ml   \
      src/validate.ml \
      src/gen_spec.ml \
      src/gen_code.ml \

MLI = $(patsubst %.ml, %.mli, $(SRC))
CMI = $(patsubst %.ml, %.cmi, $(SRC))
CMO = $(patsubst %.ml, %.cmo, $(SRC))

.PHONY: all clean repl

all: gen
repl: src/repl.out
clean:
	rm -f src/*.cmi src/*.cmo
	rm -f src/scan.ml
	rm -f gen.out src/repl.out
	rm -rf gen

gen: gen.out protocols.txt
	mkdir -p gen
	./gen.out gen < protocols.txt

gen.out: src/main.ml $(SRC)
	ocamlc $(OFLAGS) $(MLI) -c
	ocamlc $(OFLAGS) $(SRC) -c
	ocamlc $(OFLAGS) $(CMO) $< -o $@

src/repl.out: $(SRC)
	ocamlc     $(OFLAGS) $(MLI) -c
	ocamlc     $(OFLAGS) $(SRC) -c
	ocamlmktop $(OFLAGS) $(CMO) -o $@

src/scan.ml: src/scan.mll
	ocamllex $< -o $@
