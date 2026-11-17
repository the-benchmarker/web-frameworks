CC = cc
CFLAGS = -std=gnu11 -O3 -Wall -Wextra -pthread \
	-include lwan/build/lwan-build-config.h -Ilwan/src/lib
# Handlers and modules are found through an ELF section, so the whole
# archive has to be linked in, as Lwan's own build does.
LDLIBS = -Wl,-whole-archive lwan/build/src/lib/liblwan.a -Wl,-no-whole-archive \
	-pthread -lz -lm

all: simple

simple: simple.c lwan/build/src/lib/liblwan.a
	$(CC) $(CFLAGS) -o $@ simple.c $(LDLIBS)

clean:
	$(RM) simple

.PHONY: all clean
