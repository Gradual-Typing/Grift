// Copyright (c) 2007 Intel Corp.

// Black-Scholes
// Analytical method for calculating European Options
//
// 
// Reference Source: Options, Futures, and Other Derivatives, 3rd Edition, Prentice 
// Hall, John C. Hull,

// Edited for use in The Schml Benchmark Project By Andre Kuhlenschmidt
#define GC_INITIAL_HEAP_SIZE 1048576
#include "../../../../../src/backend-c/runtime/boehm-gc-install/include/gc/gc.h"

#define apply(c, args...) ((c)*(args)) 

#define vector_ref(t, x, i)                   \
  if ((i) >= 0 && (i) < x->size){             \
    t = (x)->data[(i)];                       \
  } else {                                    \
    printf("index out of bound");             \
    exit(-1);                                 \
  }                                           \
  
#define vector_set(x, i, v)                   \
  if ((i) >= 0 && (i) < (x)->size){           \
    (x)->data[(i)] = v;                       \
  } else {                                    \
    printf("index out of bound");             \
    exit(-1);                                 \
  }                                           \

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

//Precision to use for calculations
#define fptype float

#define NUM_RUNS 100

typedef struct OptionData_ {
        fptype s;          // spot price
        fptype strike;     // strike price
        fptype r;          // risk-free interest rate
        fptype divq;       // dividend rate
        fptype v;          // volatility
        fptype t;          // time to maturity or option expiration in years 
                           //     (1yr = 1.0, 6mos = 0.5, 3mos = 0.25, ..., etc)  
        char OptionType;   // Option type.  "P"=PUT, "C"=CALL
        fptype divs;       // dividend vals (not used in this test)
        fptype DGrefval;   // DerivaGem Reference Value
} OptionData;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
// Cumulative Normal Distribution Function
// See Hull, Section 11.8, P.243-244
#define inv_sqrt_2xPI 0.39894228040143270286

typedef fptype(*CDNF_code_t)(fptype InputX);

typedef struct{
  CDNF_code_t code;
} CDNF_clos;

fptype CNDF ( fptype InputX ) 
{
    int sign;

    fptype OutputX;
    fptype xInput;
    fptype xNPrimeofX;
    fptype expValues;
    fptype xK2;
    fptype xK2_2, xK2_3;
    fptype xK2_4, xK2_5;
    fptype xLocal, xLocal_1;
    fptype xLocal_2, xLocal_3;

    // Check for negative value of InputX
    if (InputX < 0.0) {
        InputX = -InputX;
        sign = 1;
    } else 
        sign = 0;

    xInput = InputX;
 
    // Compute NPrimeX term common to both four & six decimal accuracy calcs
    expValues = exp(-0.5f * InputX * InputX);
    xNPrimeofX = expValues;
    xNPrimeofX = xNPrimeofX * inv_sqrt_2xPI;

    xK2 = 0.2316419 * xInput;
    xK2 = 1.0 + xK2;
    xK2 = 1.0 / xK2;
    xK2_2 = xK2 * xK2;
    xK2_3 = xK2_2 * xK2;
    xK2_4 = xK2_3 * xK2;
    xK2_5 = xK2_4 * xK2;
    
    xLocal_1 = xK2 * 0.319381530;
    xLocal_2 = xK2_2 * (-0.356563782);
    xLocal_3 = xK2_3 * 1.781477937;
    xLocal_2 = xLocal_2 + xLocal_3;
    xLocal_3 = xK2_4 * (-1.821255978);
    xLocal_2 = xLocal_2 + xLocal_3;
    xLocal_3 = xK2_5 * 1.330274429;
    xLocal_2 = xLocal_2 + xLocal_3;

    xLocal_1 = xLocal_2 + xLocal_1;
    xLocal   = xLocal_1 * xNPrimeofX;
    xLocal   = 1.0 - xLocal;

    OutputX  = xLocal;
    
    if (sign) {
        OutputX = 1.0 - OutputX;
    }

#ifdef NDEBUG
    printf("InputX = %lf (mutated after call)\n", InputX);
    printf("sign = %d\n", sign);
    printf("xInput = %lf\n", xInput);
    printf("xNPrimeofX = %lf\n", xNPrimeofX);
    printf("xK2 = %lf\n", xK2);
    printf("xLocal = %lf\n", xLocal);
    printf("OutputX = %lf\n", OutputX);
    printf("\n");
#endif
    
    return OutputX;
} 

//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////
typedef fptype*(*BlkSchlsEqEuroNoDiv_code_t) (fptype, fptype, fptype, fptype,
                                              fptype, int, float);

typedef struct{
  BlkSchlsEqEuroNoDiv_code_t code;
} BlkSchlsEqEuroNoDiv_clos;


fptype BlkSchlsEqEuroNoDiv(fptype sptprice,
                           fptype strike,
                           fptype rate,
                           fptype volatility,
                           fptype time,
                           int otype,
                           float timet)
{
  fptype OptionPrice;
  
  // local private working variables for the calculation
  fptype xStockPrice;
  fptype xStrikePrice;
  fptype xRiskFreeRate;
  fptype xVolatility;
  fptype xTime;
  fptype xSqrtTime;

  fptype logValues;
  fptype xLogTerm;
  fptype xD1; 
  fptype xD2;
  fptype xPowerTerm;
  fptype xDen;
  fptype d1;
  fptype d2;
  fptype FutureValueX;
  fptype NofXd1;
  fptype NofXd2;
  fptype NegNofXd1;
  fptype NegNofXd2;    
    
  xStockPrice = sptprice;
  xStrikePrice = strike;
  xRiskFreeRate = rate;
  xVolatility = volatility;

  xTime = time;
  xSqrtTime = sqrt(xTime);

  logValues = log( sptprice / strike );
        
  xLogTerm = logValues;
        
    
  xPowerTerm = xVolatility * xVolatility;
  xPowerTerm = xPowerTerm * 0.5;
        
  xD1 = xRiskFreeRate + xPowerTerm;
  xD1 = xD1 * xTime;
  xD1 = xD1 + xLogTerm;

  xDen = xVolatility * xSqrtTime;
  xD1 = xD1 / xDen;
  xD2 = xD1 -  xDen;

  d1 = xD1;
  d2 = xD2;
    
  NofXd1 = CNDF( d1 );
  NofXd2 = CNDF( d2 );

  FutureValueX = strike * ( exp( -(rate)*(time) ) );        
  if (otype == 0) {            
    OptionPrice = (sptprice * NofXd1) - (FutureValueX * NofXd2);
  } else { 
    NegNofXd1 = (1.0 - NofXd1);
    NegNofXd2 = (1.0 - NofXd2);
    OptionPrice = (FutureValueX * NegNofXd2) - (sptprice * NegNofXd1);
  }

#ifdef NDEBUG
  printf("sptprice = %lf\n", sptprice);
  printf("strike = %lf\n", strike);
  printf("rate = %lf\n", rate);
  printf("volatility = %lf\n", volatility);
  printf("time = %lf\n", time);
  printf("otype = %d\n", otype);
  printf("timet = %lf\n", timet);
  printf("FutureValueX = %lf\n", FutureValueX);
  printf("price = %lf\n", OptionPrice);
#endif 
    
  return OptionPrice;
}

int main (int argc, char **argv)
{
  // localized globals of the original program
  // data has been deoptimized to an array of pointers to option data
  // This is because we are uninterested in the additional overhead
  // of following pointers in our language. 
  OptionData **data;
  fptype *prices;
  int numOptions;
  int    * otype;
  fptype * sptprice;
  fptype * strike;
  fptype * rate;
  fptype * volatility;
  fptype * otime;
  int numError = 0;
  FILE *file;
  int i;
  int loopnum;
  fptype * buffer;
  int * buffer2;
  int rv;
  // variables from inlined bs_thread
  int /* i,*/ j;
  fptype price;
#ifdef ERR_CHK
  fptype priceDelta;
#endif

  GC_INIT();
  
  rv = scanf("%i", &numOptions);

  if(rv != 1) {
    printf("ERROR: unable to read from stdin\n");
    exit(1);
  }
  
  // alloc spaces for the option data
  data = (OptionData**)GC_MALLOC(numOptions*sizeof(OptionData*));
  /* data = (OptionData*)malloc(numOptions*sizeof(OptionData));  */

  /* prices = (fptype*)malloc(numOptions*sizeof(fptype)); */
  
  for ( loopnum = 0; loopnum < numOptions; ++ loopnum )
    {
      OptionData *datum = (OptionData*)GC_MALLOC(sizeof(OptionData));
      rv = scanf("%f %f %f %f %f %f %c %f %f",
		 &datum->s,
		 &datum->strike,
		 &datum->r,
		 &datum->divq,
		 &datum->v,
		 &datum->t,
		 &datum->OptionType,
		 &datum->divs,
		 &datum->DGrefval); 
      data[loopnum] = datum; 
      if(rv != 9) {
	printf("Error: Unable to read from stdin\n");
	exit(1);
      }
    }

#ifdef NDEBUG    
  printf("Num of Options: %d\n", numOptions);
  printf("Num of Runs: %d\n", NUM_RUNS);
#endif
    
#define PAD 256
#define LINESIZE 64

  // This is dumb ... TODO rewrite this allocation in terms of
  // GC_MALLOC
  /* buffer = (fptype *) malloc(5 * numOptions * sizeof(fptype) + PAD); */
  /* sptprice = (fptype *) (((unsigned long long)buffer + PAD) & ~(LINESIZE - 1)); */
  sptprice = (fptype*)GC_MALLOC(numOptions * sizeof(fptype));
  strike = (fptype*)GC_MALLOC(numOptions * sizeof(fptype));
  sptprice = (fptype*)GC_MALLOC(numOptions * sizeof(fptype));
  rate = (fptype*)GC_MALLOC(numOptions * sizeof(fptype));
  /* rate = strike + numOptions; */
  /* volatility = rate + numOptions; */
  /* otime = volatility + numOptions; */
  volatility = (fptype*)GC_MALLOC(numOptions * sizeof(fptype));
  otime = (fptype*)GC_MALLOC(numOptions * sizeof(fptype));

  otype = (int*)GC_MALLOC(numOptions * sizeof(fptype));
  /* buffer2 = (int *) malloc(numOptions * sizeof(fptype) + PAD); */
  /* otype = (int *) (((unsigned long long)buffer2 + PAD) & ~(LINESIZE - 1)); */

  for (i=0; i<numOptions; i++) {
    otype[i]      = (data[i]->OptionType == 'P') ? 1 : 0;
    sptprice[i]   = data[i]->s;
    strike[i]     = data[i]->strike;
    rate[i]       = data[i]->r;
    volatility[i] = data[i]->v;    
    otime[i]      = data[i]->t;
  }

#ifdef NDEBUG
  printf("Size of data: %lu\n",
	 numOptions * (sizeof(OptionData) + sizeof(int)));
#endif

  prices = (fptype*)GC_MALLOC(numOptions*sizeof(fptype));
  
  // inlined bs_thread from the original benchmark to remove
  // globally mutable state which isn't available in Schml
  for (j=0; j<NUM_RUNS; j++) {
    for (i=0; i < numOptions; i++) {

#ifdef NDEBUG    
      rv = printf("run %d option %d\n", j, i);
#endif
      
      price = BlkSchlsEqEuroNoDiv( sptprice[i], strike[i],
				   rate[i], volatility[i], otime[i], 
				   otype[i], 0);
      prices[i] = price;

#ifdef ERR_CHK
      priceDelta = data[i]->DGrefval - price;
      if( fabs(priceDelta) >= 1e-4 ){
	printf("Error on %d. Computed=%.5f, Ref=%.5f, Delta=%.5f\n",
	       i, price, data[i]->DGrefval, priceDelta);
	numError ++;
      }
#endif
    }
  }


#ifdef NDEBUG    
  rv = printf("%i\n", numOptions);
  if(rv < 0) {
    printf("ERROR: Unable to write to stdout\n");
    exit(1);
  }
#endif

  for(i=0; i<numOptions; i++) {
  rv = printf("%.18f\n", prices[i]);
  if(rv < 0) {
  printf("ERROR: Unable to write to stdout\n");
  exit(1);
}
}

#ifdef ERR_CHK
  printf("Num Errors: %d\n", numError);
#endif
  /* free(data); */
  /* free(prices); */

  return 0;
}

