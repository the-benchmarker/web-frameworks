CC=cc
CV=$(shell if [ `uname` = "Darwin" ]; then echo "c11"; elif [ `uname` = "Linux" ]; then echo "gnu11"; fi;)
OS=$(shell echo `uname`)
CFLAGS=-c -Wall -O3 -std=$(CV) -pedantic -D$(OS)

SRC_DIR=.
LIB_DIRS=-Lagoo-c/lib
INC_DIRS=-Iagoo-c/include
SRCS=simple.c
LIBS=-lagoo -lpthread -lm
OBJS=$(SRCS:.c=.o)
TARGET=simple

all: $(TARGET)

clean:
	$(RM) *.o
	$(RM) *~
	$(RM) .#*
	$(RM) $(TARGET)

$(TARGET): $(OBJS)
	$(CC) -g -o $@ $(OBJS) $(LIB_DIRS) $(LIBS)

%.o : %.c
	$(CC) -I. $(INC_DIRS) $(CFLAGS) -o $@ $<

test: $(TARGET)
	$(TARGET)
