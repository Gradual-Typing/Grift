#ifndef RUNTIME_INCLUDED
#define RUNTIME_INCLUDED
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <math.h>
#include "hashcons.h"
#include "castprofiler.h"

extern table types_ht;
extern int64_t types_unique_index_counter;

/* 
read_int
runtime function for read-int primitive : (-> Int)
reads a integer from the stdin port.
*/
int64_t read_int();

/* 
read_float
runtime function for read-float primitive : (-> Float)
reads a floating point number from the stdin port.
*/
double read_float();

/* 
read_bool
runtime function for read-bool primitive : (-> Bool)
reads a #t or #f from the stdin port.
*/
int64_t read_bool();

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
  int64_t *p;
  int64_t i;
  double f;
} imdt;
  
#define imdt_to_float(x) (((imdt)(x)).f)
#define float_to_imdt(x) (((imdt)(x)).i)

/*
print_ascii_char
prints out the character literal for a character
*/
void print_ascii_char(int64_t);

/*
display_char
print a character to stdout
*/
void display_ascii_char(int64_t);

int64_t read_ascii_char();

double neg_float(double n);

#endif
