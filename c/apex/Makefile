CC ?= gcc
CFLAGS ?= -std=gnu11 -O3 -flto -D_GNU_SOURCE -pthread -Wall -Wextra -Iapex/include
LDFLAGS ?= -pthread

TARGET = simple
SRCS = server.c

all: $(TARGET)

$(TARGET): $(SRCS) apex/libapex.a
	$(CC) $(CFLAGS) -o $@ $(SRCS) apex/libapex.a $(LDFLAGS)

clean:
	rm -f $(TARGET)

.PHONY: all clean
