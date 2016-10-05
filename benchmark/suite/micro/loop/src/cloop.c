#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#define NANO_INV 1000000

int main(int argc, char *argv[]){
  struct timeval start, stop, elapsed;

  if (gettimeofday(&start, NULL))  exit(1);

  long iter = atoi(argv[1]),k=0;
  volatile long* sum=&k;
  *sum = 0;
  for(unsigned long long i=0;i<iter;++i){
    *sum = *sum + i;
  }

  if (gettimeofday(&stop, NULL)) exit(2);
  
  // convert to nanoseconds per iteration and print
  printf("%d",*sum);
  printf("time (ns): %ld\n",
         (((long) elapsed.tv_sec) * NANO_INV)
         + ((long) elapsed.tv_usec * 100));
  return *sum;
}
