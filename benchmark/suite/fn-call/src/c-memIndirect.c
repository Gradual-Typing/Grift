#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#define MICRO_INV 1000000

// The identity function with enough decorations
// to keep it from getting optimized away.
long __attribute__((noinline)) fun (long x) {
  __asm__("");
  return x;
}

int main (int argc, char* argv[]) {
  long iters;
  struct timeval start, stop, elapsed;
  
  // get the number of iterations
  if (2 != argc || 1 != sscanf(argv[1], "%ld", &iters))
    exit(1);

  // write the function pointer across a swath of memory
  long(** volatile clos)(long) = 
    (long(**)(long))malloc((sizeof(long(*)(long)) * iters));

  for(long i = 0; i < iters; i++) clos[i] = fun;

  // clock in
  if (gettimeofday(&start, NULL)) exit(2);

  // run test
  for(long i = 0; i < iters; i++) clos[i](0);

  // clock out
  if (gettimeofday(&stop, NULL)) exit(3);

  // find the elapsed time
  timersub(&stop, &start, &elapsed);

  printf("time (us): %ld\n",
         ((long) elapsed.tv_sec) * MICRO_INV
         + ((long) elapsed.tv_usec));
}
