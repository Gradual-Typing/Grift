ROOT_DIR=..
GRIFTDIR=../../../..
LIBGC=$(GRIFTDIR)/src/backend-c/runtime/boehm-gc-install/lib/libgc.a
COMPILE_LIBRARIES=$(LIBGC) -pthread -lm 
COMPILE_VERSION=clang --version
COMPILE=clang
COMPILE_OPTIONS=-O3
CLEANED:=src/*~ log bin out
MADE_CLEAN:=
SRC_EXT=c

include ../shared.mk

all: $(BINS) $(OUTS) ;





