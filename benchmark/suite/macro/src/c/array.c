#define GC_INITIAL_HEAP_SIZE 1048576
#include "gc.h"
#include<stdio.h>
#include <inttypes.h>
#include<stdlib.h>

// ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.

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

vect* create_x_code(int64_t n);

typedef vect*(*create_x_code_t)(int64_t);

typedef struct{
  create_x_code_t code;
} create_x_clos;

create_x_clos* create_x;


vect* create_x_code(int64_t n){
  vect* result = GC_MALLOC((n+1)*sizeof(int64_t));
  result->size = n;
  for (int64_t i = 0; i < n; ++i){
    vector_set(result, i, i);
  }
  return result;
}

vect* create_y_code(vect* x);

typedef vect*(*create_y_code_t)(vect*);

typedef struct{
  create_y_code_t code;
} create_y_clos;

create_y_clos* create_y;


vect* create_y_code(vect* x){
  int64_t n = x->size;
  vect* result = GC_MALLOC((n+1)*sizeof(int64_t));
  result->size = n;
  int64_t tmp;
  for (int64_t i = 0; i < n; ++i){
    vector_ref(tmp, x, n-i-1);
    vector_set(result, n-i-1, tmp);
  }
  return result;
}

int64_t my_try_code(int64_t n);

typedef int64_t(*my_try_code_t)(int64_t);

typedef struct{
  my_try_code_t code;
} my_try_clos;

my_try_clos* my_try;


int64_t my_try_code(int64_t n){
  return create_y->code(create_x->code(n))->size;
}

int64_t go_code(int64_t m,int64_t n, int64_t r);

typedef int64_t(*go_code_t)(int64_t,int64_t,int64_t);

typedef struct{
  go_code_t code;
} go_clos;

go_clos* go;

int64_t go_code(int64_t m, int64_t n,int64_t r){
  if (m > 0){
    return go->code(m-1,n,my_try->code(n));
  } else {
    return r;
  }
}

int main(){
  int64_t input1,input2;
  scanf("%" PRId64 "", &input1);
  scanf("%" PRId64 "", &input2);

  GC_INIT();

  create_x = GC_MALLOC(sizeof(create_x_clos));
  create_x->code = create_x_code;

  create_y = GC_MALLOC(sizeof(create_y_clos));
  create_y->code = create_y_code;

  my_try = GC_MALLOC(sizeof(my_try_clos));
  my_try->code = my_try_code;

  go = GC_MALLOC(sizeof(go_clos));
  go->code = go_code;
  
  printf ("%" PRId64 " ",go->code(input1,input2,0));
  
  return 0;
}
