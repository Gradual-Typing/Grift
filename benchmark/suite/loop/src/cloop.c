#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#define NANO_INV 1000000000

int main(){
  struct timeval start, stop, elapsed;

  if (gettimeofday(&start, NULL))  exit(1);
  
  for(unsigned long long i=0;i<999999999;++i){
    __asm__("");
  }

  if (gettimeofday(&stop, NULL)) exit(2);
  
  // convert to nanoseconds per iteration and print
  printf("time (ns): %ld\n",
         (((long) elapsed.tv_sec) * NANO_INV)
         + ((long) elapsed.tv_usec));
  return 0;
}
