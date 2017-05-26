#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include <sys/time.h>
#include <inttypes.h>
#define GC_INITIAL_HEAP_SIZE 1048576
#include "gc.h"

int sort(int64_t sort_clos, int64_t *a, int64_t p, int64_t r){
  int64_t tmp_closure1;
  int64_t tmp_value_partition1;
  int64_t tmp_closure2;
  int64_t tmp_value_sort1;
  int64_t tmp_closure3;
  int64_t tmp_value_sort2;
  
  if (p < r)
    {
      // int64_t q = partition(a, p, r);
      tmp_closure1 = ((long *)sort_clos)[1];
      tmp_value_partition1 = ((long *)tmp_closure1)[0];
      int64_t q = (((int64_t (*)(int64_t, int64_t*, int64_t, int64_t))tmp_value_partition1)(
											    tmp_closure1, a,p,r));
      // sort(sort_clos,a,p,q-1);

      tmp_closure2 = ((long *)sort_clos)[2];
      tmp_value_sort1 = ((long *)tmp_closure2)[0];
      (((int64_t (*)(int64_t, int64_t*, int64_t, int64_t))tmp_value_sort1)(
									   tmp_closure2, a, p, q-1));
      // sort(sort_clos,a,q+1,r);
      tmp_closure3 = ((long *)sort_clos)[2];
      tmp_value_sort2 = ((long *)tmp_closure3)[0];
      (((int64_t (*)(int64_t, int64_t*, int64_t, int64_t))tmp_value_sort2)(
									   tmp_closure3, a, q+1, r));
    }
  return 0;
}

int64_t partition(int64_t partition_clos, int64_t *a, int64_t p, int64_t r){
  int64_t tmp_closure1;
  int64_t tmp_value_swap1;
  int64_t tmp_closure2;
  int64_t tmp_value_swap2;
  int64_t tmp_value_len;
  int64_t tmp_value_r;
  int64_t u12_x;
  int64_t tmp_value_j;
  
  int64_t i = p-1;
  if (r >= 0) {
    tmp_value_len = a[0];
    if (r < tmp_value_len) {
      tmp_value_r = r + 1;
      u12_x = a[tmp_value_r];
    } else {
      printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, r);
      exit(-1);
      u12_x = 0;
    };
  } else {
    printf("index out of bound %" PRId64 "\n", r);
    exit(-1);
    u12_x = 0;
  }
  int64_t x = u12_x;
  tmp_closure1 = ((long *)partition_clos)[1];
  tmp_value_swap1 = ((long *)tmp_closure1)[0];
  for(int64_t j = p; j < r; ++j)
    {
      if (j >= 0) {
	tmp_value_len = a[0];
	if (j < tmp_value_len) {
	  tmp_value_j = j + 1;
	  u12_x = a[tmp_value_j];
	} else {
	  printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, j);;
	  exit(-1);
	  u12_x = 0;
	};
      } else {
	printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, j);;
	exit(-1);
	u12_x = 0;
      }
      if (u12_x <= x)
	{
	  ++i;
	  // swap(a,i,j);
	  (((int64_t (*)(int64_t, int64_t*, int64_t, int64_t))tmp_value_swap1)(
									       tmp_closure1, a, i, j));
	}
    }
  tmp_closure2 = ((long *)partition_clos)[1];
  tmp_value_swap2 = ((long *)tmp_closure2)[0];
  // swap(a,i+1,r);
  (((int64_t (*)(int64_t, int64_t*, int64_t, int64_t))tmp_value_swap2)(
								       tmp_closure2, a, i+1, r));
  return (i+1);
}

int64_t swap(int64_t swap_clos, int64_t *a, int64_t i, int64_t j){
  int64_t tmp_value_i;
  int64_t tmp_value_j;
  int64_t tmp_value_len;
  int64_t u12_x;
  int64_t u13_x;
  
  if(i != j)
    {
      if (i >= 0) {
	tmp_value_len = a[0];
	if (i < tmp_value_len) {
	  tmp_value_i = i + 1;
	  u12_x = a[tmp_value_i];
	} else {
	  printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, i);;
	  exit(-1);
	  u12_x = 0;
	};
      } else {
	printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, i);;
	exit(-1);
	u12_x = 0;
      }
      
      int64_t t = u12_x;

      if (j >= 0) {
	tmp_value_len = a[0];
	if (j < tmp_value_len) {
	  tmp_value_j = j + 1;
	  u13_x = a[tmp_value_j];
	} else {
	  printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, j);;
	  exit(-1);
	  u12_x = 0;
	};
      } else {
	printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, j);;
	exit(-1);
	u13_x = 0;
      }

      if (i >= 0) {
	tmp_value_len = a[0];
	if (i < tmp_value_len) {
	  tmp_value_i = i + 1;
	  a[tmp_value_i]=a[tmp_value_j];
	} else {
	  printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, i);;
	  exit(-1);
	  u12_x = 0;
	};
      } else {
	printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, i);;
	exit(-1);
	u12_x = 0;
      }

      if (j >= 0) {
	tmp_value_len = a[0];
	if (j < tmp_value_len) {
	  tmp_value_j = j + 1;
	  a[tmp_value_j]=t;
	} else {
	  printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, j);;
	  exit(-1);
	  u12_x = 0;
	};
      } else {
	printf("index out of bound %" PRId64 ",%" PRId64 "\n", tmp_value_len, j);;
	exit(-1);
	u13_x = 0;
      }
      
    }
  return 0;
}

int main(){
  int64_t swap_clos;
  int64_t partition_clos;
  int64_t sort_clos;
  int64_t tmp_value_sort1;
  int64_t LEN = 0;
  int64_t num=0;
  scanf("%" PRId64 , &LEN);

  GC_INIT();
  
  int64_t *x = (int64_t*) GC_MALLOC(sizeof(int64_t)*(1+LEN));
  x[0]=LEN;
  for (int64_t i = 1; i <= LEN; ++i){
    scanf("%" PRId64 , &num);
    x[i] = num;
  }

  sort_clos = (int64_t*) GC_MALLOC(3*sizeof(int64_t));
  partition_clos = (int64_t*) GC_MALLOC(8 * 2);
  swap_clos = (int64_t*) GC_MALLOC(sizeof(int64_t));
  ((long *)sort_clos)[0] = ((long)sort);
  ((long *)sort_clos)[1] = partition_clos;
  ((long *)sort_clos)[2] = sort_clos;
  ((long *)partition_clos)[0] = ((long)partition);
  ((long *)partition_clos)[1] = swap_clos;
  ((long *)swap_clos)[0] = ((long)swap);
  
  // sort(sort_clos,x,0,9999);
  tmp_value_sort1 = ((long *)sort_clos)[0];
  (((int64_t (*)(int64_t, int64_t*, int64_t, int64_t))tmp_value_sort1)(sort_clos, x, 0, LEN - 1));
  
  for (int64_t i=1; i<=LEN; i++)
    printf ("%ld ",x[i]);
  
  return 0;
}
