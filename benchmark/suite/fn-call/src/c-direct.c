#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#define MICRO_INV 1000000

// The identity function with enough decorations
// to keep it from getting optimized away.
long __attribute__((noinline)) fn(long x) {
  __asm__(""); 
  return x;
}

int main(int argc, char* argv[]) {
  long iters;
  struct timeval start, stop, elapsed;

  // get the number of iterations
  if (2 != argc || 1 != sscanf(argv[1], "%ld", &iters))
    exit(1);

  // clock in
  if (gettimeofday(&start, NULL))  exit(2);   

  // The actual test
  for(long i = 0; i < iters; i++) fn(0);

  // clock out
  if (gettimeofday(&stop, NULL)) exit(3);

  // find the elapsed time
  timersub(&stop, &start, &elapsed);

  // convert to microseconds per iteration and print
  printf("time (us): %ld\n",
         (((long) elapsed.tv_sec) * MICRO_INV)
         + ((long) elapsed.tv_usec));
}
