#ifndef RUNTIME_INCLUDED
#define RUNTIME_INCLUDED

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include "obj.h"

#define imdt_to_float(x) (((imdt)(x)).f)
#define float_to_imdt(x) (((imdt)(x)).i)

#include "boehm-gc-install/include/gc/gc.h"
#define GRIFT_MALLOC(size) ((grift_obj)GC_MALLOC(size));


#define GRIFT_ERROR_IN_RUNTIME 1

#include "hashcons.h"
#include "castprofiler.h"
#include "cast_queue.h"
#include "suspended_cast.h"

extern table types_ht;
extern int64_t types_unique_index_counter;
extern cast_queue* mref_cast_q;
extern cast_queue* mvect_cast_q;

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
