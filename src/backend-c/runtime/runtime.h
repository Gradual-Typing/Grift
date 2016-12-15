#ifndef RUNTIME_INCLUDED
#define RUNTIME_INCLUDED
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>
#include <math.h>

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

#endif
