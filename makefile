OFLAGS = -I src

SRC = src/utils.ml       \
      src/proto.ml       \
      src/scan.ml        \
      src/parsec.ml      \
      src/validate.ml    \
      src/proto_log.ml   \
      src/gen_spec.ml    \
      src/gen_code.ml    \
      src/gen_test.ml    \

MLI = $(patsubst %.ml, %.mli, $(SRC))
CMI = $(patsubst %.ml, %.cmi, $(SRC))
CMO = $(patsubst %.ml, %.cmo, $(SRC))

CC     = gcc -std=gnu99
CFLAGS = -pedantic -Wall -Wextra

.PHONY: all clean repl

all: gen src/repl.out
repl: src/repl.out
clean:
	rm -f src/*.cmi src/*.cmo
	rm -f src/scan.ml
	rm -f *.out src/*.out
	rm -rf gen

gen: gen.out protocols.txt gen/test_core.c gen/test_core.h
	mkdir -p gen
	./gen.out gen < protocols.txt
	$(CC) $(CFLAGS) -fPIC -I gen -o test.out     \
            gen/test.c gen/test_core.c gen/monokex.c \
             $$(pkg-config monocypher --cflags)      \
             $$(pkg-config monocypher --libs)
	./test.out

gen/test_core.c: src/test_core.c
gen/test_core.h: src/test_core.h
gen/test_core.c gen/test_core.h:
	@mkdir -p $(@D)
	cp $< $@

gen.out: src/main.ml $(CMO)
	ocamlc $(OFLAGS) str.cma  $(CMO) $< -o $@

src/repl.out: $(CMO) $(CMI)
	ocamlmktop $(OFLAGS) str.cma $(CMO) -o $@

%.cmi : %.mli $(MLI)
	ocamlc $< $(OFLAGS) -c
%.cmo : %.ml $(CMI)
	ocamlc $< $(OFLAGS) -c

src/scan.ml: src/scan.mll
	ocamllex $< -o $@
