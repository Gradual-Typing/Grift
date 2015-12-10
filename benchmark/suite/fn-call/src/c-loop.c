#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#define MICRO_INV 1000000

int main (int argc, char* argv[]) {
  long iters;
  struct timeval start, stop, elapsed;
  
  // get the number of iterations
  if (2 != argc || 1 != sscanf(argv[1], "%lu", &iters)) exit(1);
  
  // clock in
  if (gettimeofday(&start, NULL)) exit(2);

  // run test
  for(long i = 0; i < iters; i++) __asm__("");

  // clock out
  if (gettimeofday(&stop, NULL)) exit(3);

  // find the elapsed time and print
  timersub(&stop, &start, &elapsed);
  printf("time (us): %ld\n",
         ((long) elapsed.tv_sec) * MICRO_INV
         + elapsed.tv_usec);
}
