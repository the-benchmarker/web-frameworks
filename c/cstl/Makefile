CC = cc
CFLAGS = -std=c11 -O3 -Wall -Wextra -Wpedantic -pthread
LDLIBS = -pthread -lm -ldl

all: simple

simple: simple.c cstl/fio-stl/include.h
	$(CC) $(CFLAGS) -I cstl -o $@ simple.c $(LDLIBS)

clean:
	$(RM) simple

.PHONY: all clean
