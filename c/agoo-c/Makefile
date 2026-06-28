CC=cc
CV=$(shell if [ `uname` = "Darwin" ]; then echo "c11"; elif [ `uname` = "Linux" ]; then echo "gnu11"; fi;)
OS=$(shell echo `uname`)

# Production optimization flags
OPTFLAGS=-O3 -march=native -mtune=native
WARNFLAGS=-Wall -Wextra -Wpedantic -Wshadow -Wconversion -Werror
SECFLAGS=-D_FORTIFY_SOURCE=2 -fstack-protector-strong -fPIE
STDFLAGS=-std=$(CV)

# Disable debug symbols for production
DEBUGFLAGS=

# Position Independent Executable for ASLR
PIEFLAGS=-fPIE
LDPIEFLAGS=-pie

CFLAGS=-c $(OPTFLAGS) $(WARNFLAGS) $(SECFLAGS) $(STDFLAGS) $(DEBUGFLAGS) $(PIEFLAGS) -D$(OS)

SRC_DIR=.
LIB_DIRS=-Lagoo-c/lib
INC_DIRS=-Iagoo-c/include
SRCS=simple.c
LIBS=-lagoo -lpthread -lm
OBJS=$(SRCS:.c=.o)
TARGET=simple

all: $(TARGET)

# Linker flags for production
LDFLAGS=-s $(LDPIEFLAGS)  # Strip debug symbols and enable PIE

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS) $(LIB_DIRS) $(LIBS)

%.o : %.c
	$(CC) -I. $(INC_DIRS) $(CFLAGS) -o $@ $<

# Development build with debug symbols and no PIE for faster builds
develop: CFLAGS=-c $(OPTFLAGS) $(WARNFLAGS) $(SECFLAGS) $(STDFLAGS) -g -D$(OS)
develop: LDFLAGS=
develop: $(TARGET)

# Production build (default)
production: $(TARGET)

# Run the server
test: $(TARGET)
	$(TARGET)

# Clean all build artifacts
clean:
	$(RM) *.o
	$(RM) *~
	$(RM) .#*
	$(RM) $(TARGET)

# Clean and rebuild
rebuild: clean $(TARGET)
