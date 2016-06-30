#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include <sys/time.h>

#define LEN 10000

struct timeval timer_start_time;
struct timeval timer_stop_time;
struct timeval timer_result_time;
int timer_started = 1;
int timer_stopped = 1;


void timer_report() {

  // some very minor error checking
  if (timer_started) {
    printf("error starting timer");
    exit(-1);
  }
  if (timer_stopped) {
    printf("error stopping timer");
    exit(-1);
  }

  double t1 = timer_start_time.tv_sec + (timer_start_time.tv_usec / 1000000.0);
  double t2 = timer_stop_time.tv_sec + (timer_stop_time.tv_usec / 1000000.0);
  printf("time (sec): %lf\n", t2 - t1);
}


int sort(int *a, int p, int r){
  if (p < r)
    {
      int q = partition(a, p, r);
      sort(a,p,q-1);
      sort(a,q+1,r);
    }
  return 0;
}

int partition(int *a, int p, int r){
  int i = p-1;
  int x = a[r];
  for(int j = p; j < r; ++j)
    {
      if (a[j] <= x)
	{
	  ++i;
	  swap(a,i,j);
	}
    }
  swap(a,i+1,r);
  return (i+1);
}

int swap(int *a, int i, int j){
  if(i != j)
    {
      int t = a[i];
      a[i]=a[j];
      a[j]=t;
    }
  return 0;
}

int main(){
  int *x = malloc(sizeof(int)*LEN);
  /* for (int i = 0; i < LEN; ++i){ */
  /*   x[i] = LEN - i; */
  /* } */
  FILE* stream  = fopen("nums", "r");

  for (int i = 0; i < LEN; i++)
  {
    fscanf(stream, "%d", &x[i]);
  } 

  fclose(stream);

  timer_started = gettimeofday(&timer_start_time, NULL);
  sort(x,0,9999);
  timer_stopped = gettimeofday(&timer_stop_time, NULL);
  timer_report();

  for (int i=0; i<LEN; i++)
    printf ("%d ",x[i]);
  
  return 0;
}
