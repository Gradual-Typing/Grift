#ifndef RUNTIME_INCLUDED
#define RUNTIME_INCLUDED
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

/* 
read_int
runtime function for read-int primitive : (-> Int)
reads a integer from the stdin port.
*/
int64_t read_int();


#endif
