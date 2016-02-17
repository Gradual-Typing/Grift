#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

int64_t read_int(){
  int64_t i;
  if(EOF == scanf("%" PRId64, &i)){
    printf("Error in read_int\n");
    exit(-1);
  }
  return i;
}
