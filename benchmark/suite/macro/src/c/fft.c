// Fast Fourier Transform (FFT)
// This implementation is an adoption of the one in Numerical Recipes
// book but in a functional style. It is written this way to be
// comparable to the implementations in functional languages.

// Authored by Deyaaeldeen Almahallawi

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
  double data[];
} vect;

int n;
vect* data;
const double pi2 = 6.28318530717959; // to compute the inverse, negate this value

void loop2_code(int m, int j, int i);

typedef void(*loop1_code_t)(int,int);

typedef struct{
  loop1_code_t code;
} loop1_clos;

loop1_clos* loop1;

typedef void (*loop2_code_t)(int,int,int);

typedef struct{
  loop2_code_t code;
} loop2_clos;

loop2_clos* loop2;

void loop1_code(int i, int j){
  double temp;
  double c_temp;
  if (i < n) {
    if (i < j) {
      vector_ref(temp,data,i);
      vector_ref(c_temp,data,j);
      vector_set(data,i,c_temp);
      vector_set(data,j,temp);

      vector_ref(temp,data,i+1);
      vector_ref(c_temp,data,j+1);
      vector_set(data,i+1,c_temp);
      vector_set(data,j+1,temp);
    }
    loop2->code(n / 2,j,i);
  }
}

void loop2_code(int m, int j, int i){
  if (m >= 2 && j >= m) 
    loop2->code(m / 2, j-m,i);
  else
    loop1->code(i+2,j+m);
}

typedef void (*loop3_code_t)(int);

typedef struct{
  loop3_code_t code;
} loop3_clos;

loop3_clos* loop3;

void loop4_code(double,double,int,int,double,double);

typedef void (*loop4_code_t)(double,double,int,int,double,double);

typedef struct{
  loop4_code_t code;
} loop4_clos;

loop4_clos* loop4;

void loop3_code(int mmax){
  if (mmax < n){
    double theta = pi2 / mmax;
    double x = sin(0.5 * theta);
    double wpr = -2.0 * x * x;
    double wpi = sin(theta);
    loop4->code(1.0,0.0,0,mmax,wpr,wpi);
    loop3->code(mmax*2);
  }
}

void loop5_code(int,int,double,double,int,double,double);

typedef void (*loop5_code_t)(int,int,double,double,int,double,double);

typedef struct{
  loop5_code_t code;
} loop5_clos;

loop5_clos* loop5;

void loop4_code(double wr,double wi,
		int m,int mmax,
		double wpr,double wpi){
  if (m < mmax)
    loop5->code(m,mmax,wr,wi,m,wpr,wpi);
}

void loop5_code(int i,int mmax,
		double wr,double wi,
		int m,
		double wpr,double wpi){
  if (i < n) {
    int j = i+mmax;
    double c_temp1,c_temp2;
    vector_ref(c_temp1,data,j);
    vector_ref(c_temp2,data,j+1);
    double tempr = (wr * c_temp1) - (wi * c_temp2);
    vector_ref(c_temp1,data,j+1);
    vector_ref(c_temp2,data,j);
    double tempi = (wr * c_temp1) + (wi * c_temp2);

    vector_ref(c_temp1,data,i);
    vector_set(data,j,c_temp1-tempr);

    vector_ref(c_temp1,data,i+1);
    vector_set(data,j+1,c_temp1-tempi);

    vector_ref(c_temp1,data,i);
    vector_set(data,i,c_temp1+tempr);

    vector_ref(c_temp1,data,i+1);
    vector_set(data,i+1,c_temp1+tempi);

    loop5->code(j+mmax,mmax,wr,wi,m,wpr,wpi);
    
  } else {
    loop4->code((wr*wpr)-(wi*wpi) + wr, (wi*wpr)+(wr*wpi) + wi,m+2,mmax,wpr,wpi);
  }
}

int main(int argc, char* argv[]){
  if (argc > 1) {
    n = atoi(argv[1]);
  } else {
    if (!scanf("%d", &n) || n < 0){
      fputs("invalid input", stderr);
      exit(-1);
    } 
  }

  GC_INIT();

  data = GC_MALLOC(sizeof(int64_t)+n*sizeof(double));
  data->size = n;
  for (int i = 0; i < data->size; ++i)
    data->data[i] = 0;

  loop1 = GC_MALLOC(sizeof(loop1_clos));
  loop1->code = loop1_code;
  
  loop2 = GC_MALLOC(sizeof(loop2_clos));
  loop2->code = loop2_code;

  loop3 = GC_MALLOC(sizeof(loop3_clos));
  loop3->code = loop3_code;

  loop4 = GC_MALLOC(sizeof(loop4_clos));
  loop4->code = loop4_code;

  loop5 = GC_MALLOC(sizeof(loop5_clos));
  loop5->code = loop5_code;

  loop1->code(0,0); // bit-reversal section
  loop3->code(2); // Danielson-Lanczos section

  printf("%lf\n",data->data[0]);
  
  return 0;
}
