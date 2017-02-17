#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include <sys/time.h>
#include <inttypes.h>

void *alloc_ptr;
long free_ptr;
long limit;
unsigned long allocd_mem;

struct timeval timer_start_time;
struct timeval timer_stop_time;
struct timeval timer_result_time;
int timer_started = 1;
int timer_stopped = 1;
int64_t ar,ac,bc,br;


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

long alloc(int n) {
  long result = free_ptr;
  long newFree = result + n;
  allocd_mem += n;
  if (newFree >= limit) {
    puts("Requesting more memory\n");
    free_ptr = (long)(posix_memalign(&alloc_ptr, 8, 9999999999), alloc_ptr);
    if (!free_ptr) {
      fputs("couldn't allocate any more memory", stderr);
    }
    limit = free_ptr + 9999999999;
    return alloc(n);
  }
  free_ptr = newFree;
  return result;
}

int64_t** create (int64_t l1, int64_t l2){
  int64_t tmp_value;
  int64_t *t;
  int64_t *xi;
  int64_t **x = malloc(sizeof(int64_t)*(1+l1));
  ((long *)x)[0] = l1+1;
  for (int i=1;i<=l1;++i){
    t = malloc(sizeof(int64_t*)*(1+l2));
    t[0]=l2+1;
    for (int j=1;j<=l2;++j){
      t[j]=0;
    }
    x[i]=t;
  }
  for (int i=1;i<=l1;++i){
    xi = malloc(sizeof(int64_t)*(1+l2));
    xi[0]=l2+1;
    for (int j=1;j<=l2;++j){
      xi[j]=0;
    }
    for (int j=1;j<=l2;++j){
      if (j >= 0){
	tmp_value = xi[0];
	if (j < tmp_value){
	  xi[j]=j+i-2;
	} else {
	  printf("index out of bound %d\n", j);
	  exit(-1);
	}
      } else {
	printf("index out of bound %d\n", j);
	exit(-1);
      }
    }
    if (i >= 0){
      tmp_value = ((long *)x)[0];
      if (i < tmp_value){
	x[i]=xi;
      } else {
	printf("index out of bound %d\n", i);
	exit(-1);
      }
    } else {
      printf("index out of bound %d\n", i);
      exit(-1);
    }
  }
  return x;
}

int64_t** mult(int64_t **x, int64_t x1,
	       int64_t **y, int64_t y1, int64_t y2){
  int64_t *t,*ri;
  int64_t tmp_value;
  int64_t **r = malloc(sizeof(int64_t)*(1+ar));
  ((long *)r)[0] = ar+1;
  for (int i=1;i<=ar;++i){
    t = malloc(sizeof(int64_t*)*(1+bc));
    t[0]=bc+1;
    for (int j=1;j<=bc;++j){
      t[j]=0;
    }
    r[i]=t;
  }

  for (int i=1;i<=x1;++i){
    ri = malloc(sizeof(int64_t)*(1+y2));
    ri[0]=y2+1;
    for (int j=1;j<=y2;++j){
      ri[j]=0;
    }
    for (int j=1;j<=y2;++j){
      for (int k=1;k<=y1;++k){
	// bound check for writing ri[j]
	if (j >= 0){
	  tmp_value = ri[0];
	  if (j < tmp_value){
	    // bound check for reading from ri[j]
	    if (j >= 0){
	      tmp_value = ri[0];
	      if (j < tmp_value){
		// bound check for reading from x[i]
		if (i >= 0){
		  tmp_value = ((long *)x)[0];
		  if (i < tmp_value){
		    // bound check for reading from x[i][k]
		    if (k >= 0){
		      tmp_value = ((long *)((long *)x)[1])[0];
		      if (k < tmp_value){
			if (k >= 0){
			  tmp_value = ((long *)y)[0];
			  if (k < tmp_value){
			    if (j >= 0){
			      tmp_value = ((long *)((long *)y)[1])[0];
			      if (j < tmp_value){
				ri[j]=ri[j]+(((long *)((long *)x)[i])[k] * ((long *)((long *)y)[k])[j]);
			      } else {
				printf("index out of bound %d\n", j);
				exit(-1);
			      }
			    } else {
			      printf("index out of bound %d\n", j);
			      exit(-1);
			    }
			  } else {
			    printf("index out of bound %d\n", k);
			    exit(-1);
			  }
			} else {
			  printf("index out of bound %d\n", k);
			  exit(-1);
			}
		      } else {
			printf("index out of bound %d\n", k);
			exit(-1);
		      }
		    } else {
		      printf("index out of bound %d\n", k);
		      exit(-1);
		    }
		  } else {
		    printf("index out of bound %d\n", i);
		    exit(-1);
		  }
		} else {
		  printf("index out of bound %d\n", i);
		  exit(-1);
		}
	      } else {
		printf("index out of bound %d\n", j);
		exit(-1);
	      }
	    } else {
	      printf("index out of bound %d\n", j);
	      exit(-1);
	    }
	  } else {
	    printf("index out of bound %d\n", j);
	    exit(-1);
	  }
	} else {
	  printf("index out of bound %d\n", j);
	  exit(-1);
	}
      }
    }

    
    if (i >= 0){
      tmp_value = ((long *)r)[0];
      if (i < tmp_value){
	r[i]=ri;
      } else {
	printf("index out of bound %d\n", i);
	exit(-1);
      }
    } else {
      printf("index out of bound %d\n", i);
      exit(-1);
    }
  }
  return r;
}

int main(int argc, char *argv[]){
  int64_t size;

  if(!scanf("%" PRId64 , &size)){
    exit(-1);
  }

  int64_t ar=size;
  int64_t ac=size;
  int64_t br=size;
  int64_t bc=size;

  int64_t **a = create (ar,ac);
  int64_t **b = create (br,bc);

  int64_t **r = mult(a, ar, b, br, bc);
  int64_t rf = ((long *)((long *)r)[ar])[ac];
  printf ("%" PRId64 , rf);

  return 0;
}
