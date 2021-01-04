OFLAGS = -I src

SRC = src/utils.ml       \
      src/proto.ml       \
      src/scan.ml        \
      src/parsec.ml      \
      src/validate.ml    \
      src/proto_log.ml   \
      src/proto_run.ml   \
      src/gen_spec.ml    \
      src/gen_code.ml    \
      src/gen_test.ml    \

MLI = $(patsubst %.ml, %.mli, $(SRC))
CMI = $(patsubst %.ml, %.cmi, $(SRC))
CMO = $(patsubst %.ml, %.cmo, $(SRC))

CC     = gcc -std=gnu99
CFLAGS = -pedantic -Wall -Wextra

.PHONY: all clean repl gen

all: gen src/repl.out
repl: src/repl.out
clean:
	rm -f src/*.cmi src/*.cmo
	rm -f src/scan.ml
	rm -f *.out src/*.out
	rm -rf classic elligator

gen: gen.out protocols.txt
	./gen.out < protocols.txt
	(cd classic;   make test)
	(cd elligator; make test)

gen.out: src/main.ml $(CMO)
	ocamlc $(OFLAGS) str.cma unix.cma $(CMO) $< -o $@

src/repl.out: $(CMO) $(CMI)
	ocamlmktop $(OFLAGS) str.cma unix.cma $(CMO) -o $@

%.cmi : %.mli $(MLI)
	ocamlc $< $(OFLAGS) -c
%.cmo : %.ml $(CMI)
	ocamlc $< $(OFLAGS) -c

src/scan.ml: src/scan.mll
	ocamllex $< -o $@
