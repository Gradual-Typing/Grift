#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

//long __attribute__((noinline)) fun (long x) {
long fun (long x) {
  // this asm block keeps the compiler from optimizing
  // away the function call
  __asm__("");
  return x;
}

int main (int argc, char* argv[]) {

  unsigned long iters;
 
  // get the number of iterations
  if (2 != argc || 1 != sscanf(argv[1], "%lu", &iters)){
    printf("The test expects the number of iterations\n");
    exit(-1);
  }

  struct timeval start;
  struct timeval stop;
  
  // clock in
  if (gettimeofday(&start, NULL)){
    printf("couldn't get the start time\n");
    exit(-1);
  }

  // run test
  for(long i = 0; i < iters; i++){
    fun(0);
  }

  // clock out
  if (gettimeofday(&stop, NULL)){
    printf("couldn't get the stop time\n");
    exit(-1);
  }

  // find the difference
  struct timeval result;
  timersub(&stop, &start, &result);
  printf("time (sec): %lf\n", ((double) result.tv_sec) +
         (((double) result.tv_usec) / 1000000.0)); 
  
  return 0;
}
