# Simple makefile to build a square.c in the labs.
#
# Path to the OpenCL libraries.  In this case AMD SDK
# is being used, as system uses AMD graphics card.
OPENCL        := /opt/AMDAPP

# C flags with strictest warnings.
CFLAGS        += -O3 -Wall -Wextra -I$(OPENCL)/include -std=c99 -D_GNU_SOURCE

# Linker flags.
LDFLAGS += -L$(OPENCL)/lib/x86_64  -l OpenCL -lrt


all: seqTotient openclTotient

# Build a binary from C source.
simple.o: simple.c
	$(CC) $(CFLAGS) -std=c99 -c $^

seqTotient: TotientRange.c
	$(CC) $(CFLAGS) -std=c99 -o $@ $^

openclTotient: OpenCLTotient.c simple.o
	$(CC) $(CFLAGS) -std=c99 $(LDFLAGS) -o $@ $^

# Remove the binary.
clean:
	$(RM) seqTotient openclTotient simple.o

