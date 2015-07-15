#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

void __attribute__((noinline)) fun () {
  __asm__("");
  return;
}

/*
  This test is suppose to measure the time that it takes to
  make to load a pointer from memory, and call the function
  at that pointer. This is roughly a c-indirect call as I
  understand it.
 
*/

int main (int argc, char* argv[]) {

  // The problem with this test is that ifun will allways be
  // in the cache, and that page will always be in memory.
  void (* register rfun)() = &fun;
  
  // get the number of iterations
  unsigned long iters;
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
    rfun();
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
