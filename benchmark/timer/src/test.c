#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

void __attribute__((noinline)) fun () {
  __asm__("");
  return;
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
  // the smallest test is no test

  // clock out
  if (gettimeofday(&stop, NULL)){
    printf("couldn't get the stop time\n");
    exit(-1);
  }

  // find the difference
  struct timeval result;
  timersub(&stop, &start, &result);
  printf("time (sec): %f\n", ((double) result.tv_sec) +
         (((double) result.tv_usec) / 1000000.0)); 
  
  return 0;
}
