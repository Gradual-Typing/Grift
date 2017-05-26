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

vect* create_code(int64_t l1, int64_t l2);

typedef vect*(*create_code_t)(int64_t,int64_t);

typedef struct{
  create_code_t code;
} create_clos;

create_clos* create;

vect* create_code(int64_t l1, int64_t l2){
  vect* x = GC_MALLOC((l1*l2 + 1) * sizeof(int64_t));
  x->size = l1*l2;
  for (int i = 0; i < l1; ++i){
    for (int j = 0; j < l2; ++j){
      vector_set(x,l2*i+j,j+i);
    }
  }
  return x;
}

vect* mult_code(vect* x, int64_t x1, int64_t x2,
		vect* y, int64_t y1, int64_t y2);

typedef vect*(*mult_code_t)(vect*,int64_t,int64_t,vect*,int64_t,int64_t);

typedef struct{
  mult_code_t code;
} mult_clos;

mult_clos* mult;

vect* mult_code(vect* x, int64_t x1, int64_t x2,
		vect* y, int64_t y1, int64_t y2){
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
  scanf("%" PRId64 "", &size);
  ar=size;ac=size;br=size;bc=size;

  GC_INIT();

  create = GC_MALLOC(sizeof(create_clos));
  create->code = create_code;

  mult = GC_MALLOC(sizeof(mult_clos));
  mult->code = mult_code;

  vect* a = create->code(ar,ac);
  vect* b = create->code(br,bc);
  
  vect* r = mult->code(a, ar, ac, b, br, bc);
  int64_t rf;
  vector_ref(rf,r,(ar*bc)-1);
  printf ("%" PRId64 " ",rf);
  
  /* for (int i=1;i<=ar;++i) */
  /*     for (int j=1;j<=bc;++j) */
  /* 	printf ("%ld ",r[i][j]); */
  
  return 0;
}
