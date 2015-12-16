#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

long __attribute__((noinline)) fn(long x) {
  __asm__(""); 
  return x;
}

int main(int argc, char* argv[]) {
  long iters = 10000000;
  struct timeval start, stop, elapsed;

  if (gettimeofday(&start, NULL))  exit(2);   

  for(long i = 0; i < iters; i++) fn(0);

  if (gettimeofday(&stop, NULL)) exit(3);
  timersub(&stop, &start, &elapsed);
  printf("time (us): %lf\n",
         ((((double) elapsed.tv_sec) * 1000000.0)
          + ((double) elapsed.tv_usec))
          / (double) iters);
}
