CC           = gcc -std=c99
CFLAGS       = -pedantic -Wall -Wextra -O3 -march=native
DESTDIR      =
PREFIX       = usr/local
LIBDIR       = $(PREFIX)/lib
INCLUDEDIR   = $(PREFIX)/include
PKGCONFIGDIR = $(LIBDIR)/pkgconfig
MANDIR       = $(PREFIX)/share/man/man3
SONAME       = libmonokex.so.1

CRYPTO_CFLAGS = $$(pkg-config monocypher --cflags)
CRYPTO_LIBS   = $$(pkg-config monocypher --libs)

.PHONY: all library static-library dynamic-library \
        install clean uninstall dist               \
        check test

all            : library
library        : static-library dynamic-library
static-library : libmonokex.a
dynamic-library: libmonokex.so $(SONAME)
check          : test

test: test.out
	./test.out

clean:
	rm -f *.o *.a *.so *.so.* *.out

install: library monokex.h monokex.pc
	mkdir -p $(DESTDIR)/$(INCLUDEDIR)
	mkdir -p $(DESTDIR)/$(LIBDIR)
	mkdir -p $(DESTDIR)/$(PKGCONFIGDIR)
	cp -P libmonokex.a libmonokex.so* $(DESTDIR)/$(LIBDIR)
	cp monokex.h $(DESTDIR)/$(INCLUDEDIR)
	$(INSTALL_ED25519)
	sed "s|PREFIX|$(PREFIX)|"  monokex.pc \
            > $(DESTDIR)/$(PKGCONFIGDIR)/monokex.pc

libmonokex.a: monokex.o
	ar cr $@ $^
libmonokex.so: $(SONAME)
	ln -sf `basename $<` $@
$(SONAME): monokex.o
	$(CC) $(CFLAGS) $(LDFLAGS) -shared -Wl,-soname,$(SONAME) -o $@ $^

monokex.o: monokex.c monokex.h
	$(CC) $(CFLAGS) -fPIC -c -o $@ $< $(CRYPTO_CFLAGS)

test.out: test.c test_core.c monokex.o
	$(CC) $(CFLAGS) -fPIC -o $@                  \
            test.c test_core.c test_core.h monokex.o \
             $(CRYPTO_CFLAGS) $(CRYPTO_LIBS)
