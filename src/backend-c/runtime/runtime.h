#ifndef RUNTIME_INCLUDED
#define RUNTIME_INCLUDED
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <math.h>

/* floatptr_t is a floating point number that is the same width as a pointer */
#if   (__SIZEOF_POINTER__ == __SIZEOF_DOUBLE__)
typedef double floatptr_t;
#elif (__SIZEOF_POINTER__ == __SIZEOF_FLOAT__)
typedef float  floatptr_t;
#else
#error __file__ ":" __line__ ": unsupported floating point size"
#endif

typedef union grift_obj {
  intptr_t         as_int;
  uintptr_t        as_uint;
  floatptr_t       as_float;
  union grift_obj *as_ptr;
  void            *as_voidptr;
} grift_obj;


typedef union {
  intptr_t *p;
  intptr_t i;
  floatptr_t f;
} imdt;
  
#define imdt_to_float(x) (((imdt)(x)).f)
#define float_to_imdt(x) (((imdt)(x)).i)

#include "boehm-gc-install/include/gc/gc.h"
// TODO these should be moved to runtime.h
#define NULL_GRIFT_OBJ ((grift_obj)(uintptr_t)0)
#define GRIFT_MALLOC(size) ((grift_obj)GC_MALLOC(size));


#define GRIFT_ERROR_IN_RUNTIME 1

#include "hashcons.h"
extern table types_ht;
extern int64_t types_unique_index_counter;

#include "io.h"

#include "assoc_stack.h"

#define max(a,b)\
({ __typeof__ (a) _a = (a);\
   __typeof__ (b) _b = (b);\
   _a > _b ? _a : _b; })

#define min(a,b)\
({ __typeof__ (a) _a = (a);\
   __typeof__ (b) _b = (b);\
   _a < _b ? _a : _b; })


#endif /* RUNTIME_INCLUDED */
