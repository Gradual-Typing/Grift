#include "runtime.h"

int64_t read_int(){
  int64_t i;
  if(EOF == scanf("%" PRId64, &i)){
    fputs("Error in read_int\n", stderr);
    exit(-1);
  }
  return i;
}

double read_float(){
  double d;
  if(EOF == scanf("%lf", &d)){
    fputs("Error in read_float\n", stderr);
    exit(-1);
  }
  return d;
}
