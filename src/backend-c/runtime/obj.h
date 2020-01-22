#ifndef OBJ_INCLUDED
#define OBJ_INCLUDED
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

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

#define NULL_GRIFT_OBJ ((grift_obj)(uintptr_t)0)

#endif /* OBJ_INCLUDED */
