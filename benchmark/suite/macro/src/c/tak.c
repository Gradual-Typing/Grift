// TAK -- A vanilla version of the TAKeuchi function.

#define GC_INITIAL_HEAP_SIZE 1048576
#include "../../../../../src/backend-c/runtime/boehm-gc-install/include/gc/gc.h"
#include <inttypes.h>
#include <stdio.h>

int64_t tak_code(int64_t,int64_t,int64_t);

typedef int64_t(*tak_code_t)(int64_t,int64_t,int64_t);

typedef struct{
  tak_code_t code;
} tak_clos;

tak_clos* tak;

int64_t tak_code(int64_t x, int64_t y, int64_t z){
  if (! (y < x)) {
    return z;
  } else {
    return tak->code(tak->code(x-1,y,z),tak->code(y-1,z,x),tak->code(z-1,x,y));
  }
}

int main(){
  GC_INIT();

  tak = GC_MALLOC(sizeof(tak_clos));
  tak->code = tak_code;

  printf("%" PRId64 " ",tak->code(40,20,12));
  
  return 0;
}
