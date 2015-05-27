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
  struct timezone ust;
  
  // clock in
  if (gettimeofday(&start, NULL)){
    printf("couldn't get the start time\n");
    exit(-1);
  }

  // run test
  for(long i = 0; i < iters; i++){
    fun();
  }

  // clock out
  if (gettimeofday(&stop, NULL)){
    printf("couldn't get the stop time\n");
    exit(-1);
  }

  // find the difference
  struct timeval result;
  timersub(&stop, &start, &result);
  
  printf("time: %ld.%d\n", result.tv_sec, result.tv_usec); 
  
  return 0;
}
