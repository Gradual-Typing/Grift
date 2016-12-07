#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include <sys/time.h>
#include <inttypes.h>
#define GC_INITIAL_HEAP_SIZE 1048576
#include "gc.h"

int64_t ar,ac,bc,br;

int64_t** create (int64_t l1, int64_t l2){
  int64_t tmp_value;
  int64_t *t;
  int64_t *xi;
  int64_t **x = GC_MALLOC(sizeof(int64_t)*(1+l1));
  ((long *)x)[0] = l1+1;
  for (int i=1;i<=l1;++i){
    t = GC_MALLOC(sizeof(int64_t*)*(1+l2));
    t[0]=l2+1;
    for (int j=1;j<=l2;++j){
      t[j]=0;
    }
    x[i]=t;
  }
  for (int i=1;i<=l1;++i){
    xi = GC_MALLOC(sizeof(int64_t)*(1+l2));
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
  int64_t **r = GC_MALLOC(sizeof(int64_t)*(1+ar));
  ((long *)r)[0] = ar+1;
  for (int i=1;i<=ar;++i){
    t = GC_MALLOC(sizeof(int64_t*)*(1+bc));
    t[0]=bc+1;
    for (int j=1;j<=bc;++j){
      t[j]=0;
    }
    r[i]=t;
  }

  for (int i=1;i<=x1;++i){
    ri = GC_MALLOC(sizeof(int64_t)*(1+y2));
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

int main(){
  int64_t size = 0;
  scanf("%ld", &size);
  ar=size;ac=size;br=size;bc=size;

  GC_INIT();

  int64_t **a = create (ar,ac);
  int64_t **b = create (br,bc);
  
  int64_t **r = mult(a, ar, b, br, bc);
  int64_t rf = ((long *)((long *)r)[ar])[ac];
  printf ("%ld ",rf);
  
  /* for (int i=1;i<=ar;++i) */
  /*     for (int j=1;j<=bc;++j) */
  /* 	printf ("%ld ",r[i][j]); */
  
  return 0;
}
