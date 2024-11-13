# Library Name
LIBRARY_NAME = libGenerate.so

# Targets
LIBRARY = $(LIBRARY_NAME)

# Compiler and linker flags
CFLAGS = -fPIC  # Position-independent code for shared libraries
LDFLAGS = -shared -dynamic

# Include and library paths
INCLUDES = -I${HOME}/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/include
LIBRARY_PATHS = \
    -L../dist-newstyle/build/aarch64-osx/ghc-8.10.7/haskell-diagrams-cellular-automata-0.1.0.0/build \
    -L${HOME}/.ghcup/ghc/8.10.7/lib/ghc-8.10.7/rts

# Libraries to link against
LIBS = \
    -lHShaskell-diagrams-cellular-automata-0.1.0.0-inplace-ghc8.10.7 \
    -lHSrts-ghc8.10.7 \
    -lpthread -lm

# Create shared library
$(LIBRARY): generate.c
	gcc $(CFLAGS) generate.c \
		$(INCLUDES) \
		$(LIBRARY_PATHS) \
		$(LIBS) \
		$(LDFLAGS) -o $(LIBRARY)

# Clean up generated files
clean:
	rm -f $(LIBRARY)

# Phony targets
.PHONY: all clean
