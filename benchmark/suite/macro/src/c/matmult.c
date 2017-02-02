#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include <inttypes.h>
#include <math.h>
#define GC_INITIAL_HEAP_SIZE 1048576
#include "gc.h"

#define vector_ref(t, x, i)			\
  if ((i) >= 0 && (i) < x->size){		\
    t = (x)->data[(i)];				\
  } else {					\
    printf("index out of bound");		\
    exit(-1);					\
  }						\

#define vector_set(x, i, v)			\
  if ((i) >= 0 && (i) < (x)->size){		\
    (x)->data[(i)] = v;				\
  } else {					\
    printf("index out of bound");		\
    exit(-1);					\
  }						\

typedef struct{
  int64_t size;
  int64_t data[];
} vect;

vect* create (int64_t l1, int64_t l2){
  vect* x = GC_MALLOC((l1*l2 + 1) * sizeof(int64_t));
  x->size = l1*l2;
  for (int i = 0; i < l1; ++i){
    for (int j = 0; j < l2; ++j){
      vector_set(x,l2*i+j,j+i);
    }
  }
  return x;
}

vect* mult(vect* x, int x1, int x2,
	   vect* y, int y1, int y2){
  int64_t tmp1;
  int64_t tmp2;
  int64_t tmp3;
  vect* r = GC_MALLOC((y2*x1+1)*sizeof(int64_t));
  r->size=x1*y2;
  for (int i = 0; i < x1; ++i){
    for (int j = 0; j < y2; ++j){
      for (int k = 0; k < y1; ++k){
	vector_ref(tmp1, r, (i * y2) + j);
	vector_ref(tmp2, x, (i * x2) + k);
	vector_ref(tmp3, y, (k * y2) + j);
	vector_set(r, ((i * y2) + j), (tmp1 + (tmp2 * tmp3)));
      }
    }
  }
  return r;
}

int main(){
  int64_t size = 0, ar,ac,br,bc;
  scanf("%ld", &size);
  ar=size;ac=size;br=size;bc=size;

  GC_INIT();

  vect* a = create (ar,ac);
  vect* b = create (br,bc);
  
  vect* r = mult(a, ar, ac, b, br, bc);
  int64_t rf;
  vector_ref(rf,r,(ar*bc)-1);
  printf ("%ld ",rf);
  
  /* for (int i=1;i<=ar;++i) */
  /*     for (int j=1;j<=bc;++j) */
  /* 	printf ("%ld ",r[i][j]); */
  
  return 0;
}
