#include <stdlib.h>

int main(int argc, char *argv[]){
  unsigned long long bound = strtoul(argv[1],NULL,10);
  unsigned long long * x = malloc(sizeof(unsigned long long));
  *x = 0;

  for(unsigned long long i=0;i<bound;++i){
    __asm__("");
  }
  
  return *x;
}
