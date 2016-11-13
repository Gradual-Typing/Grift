#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include <sys/time.h>
#include <inttypes.h>
#define GC_INITIAL_HEAP_SIZE 1048576
#include "../../../../../src/backend-c/runtime/boehm-gc-install/include/gc/gc.h"
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

typedef struct{
  int64_t size;
  double data[];
} body;

typedef struct{
  int64_t size;
  body*  data[];
} bodies;

#define apply(c, args...) ((c)*(c , ##args)) 

#define vector_ref(t, x, i)                  \
  if ((i) >= 0 && (i) < x->size){            \
    t = (x)->data[(i)];                       \
  } else {                                   \
    printf("index out of bound");	     \
    exit(-1);                                \
  }                                          \

#define vector_set(x, i, v)                  \
    if ((i) >= 0 && (i) < (x)->size){	     \
    (x)->data[(i)] = v;                       \
  } else {                                   \
    printf("index out of bound");	     \
    exit(-1);                                \
  }                                          \


  
const double pi = 3.141592653589793;
const double days_per_year = 365.24;
const double solar_mass    = 4.0 * pi * pi;
const double dt            = 0.01;
body* sun;
body* jupiter;
body* saturn;
body* uranus;
body* neptune;
bodies* solar_system;
int system_size = 5;

body* make_body(double x, double y, double z,
		 double vx, double vy, double vz,
		 double mass){
  body* v = GC_MALLOC(8 * sizeof(int64_t));
  v->size = 7;
  v->data[0] = x;
  v->data[1] = y;
  v->data[2] = z;
  v->data[3] = vx;
  v->data[3] = vy;
  v->data[3] = vz;
  v->data[4] = mass;
  return v;
}

void offset_momentum_loop(int i1, double px, double py, double pz){
  if (i1 == system_size) {
    body* sun;
    vector_ref(sun, solar_system, 0);
    vector_set(sun, 3, (0.0 - px) / solar_mass);
    vector_set(sun, 4, (0.0 - py) / solar_mass);
    vector_set(sun, 5, (0.0 - pz) / solar_mass);
  } else {
    body* j;
    double j_vx, j_vy, j_vz, j_mass;
    vector_ref(j, solar_system, i1);
    vector_ref(j_vx, j, 3);
    vector_ref(j_vy, j, 4);
    vector_ref(j_vz, j, 5);
    vector_ref(j_mass, j, 6);
    offset_momentum_loop(i1+1,
			 px + (j_vx * j_mass),
			 py + (j_vy * j_mass),
			 pz + (j_vz * j_mass));
  }
}

#define sq(x) ((x)*(x))

double energy_loop_i(int o, double e, body* ob, int i);
double energy_loop_o(int o, double e){
  if (o == system_size) {
    return e;
  } else {
    body* ob;
    double ob_vx, ob_vy, ob_vz, ob_mass;
    vector_ref(ob, solar_system, o);
    vector_ref(ob_vx, ob, 3);
    vector_ref(ob_vy, ob, 4);
    vector_ref(ob_vz, ob, 5);
    vector_ref(ob_mass, ob, 6);
    e = e + (0.5 * ob_mass * (sq(ob_vx) + sq(ob_vy) + sq(ob_vz)));
    return energy_loop_i(o, e, ob, o + 1);
  }
}

double energy_loop_i(int o, double e, body* ob, int i){
  if (i == system_size) {
    return energy_loop_o(o + 1, e);
  } else {
    return 0.0;     
  }
}

  
double energy(){
  return energy_loop_o(0, 0.0);
}

void offset_momentum(){
  offset_momentum_loop(0, 0.0, 0.0, 0.0);
}

int main(int argc, char *argv[]){
  int64_t size = strtol(argv[1], (char **)NULL, 10);
  timer_started = gettimeofday(&timer_start_time, NULL);

  GC_INIT();
  
  sun = make_body(0.0, 0.0, 0.0,
		  0.0, 0.0, 0.0, solar_mass);

  jupiter = make_body( 4.84143144246472090,
		      -1.16032004402742839,
		      -1.03622044471123109e-1,
		       1.66007664274403694e-3 * days_per_year,
		       7.69901118419740425e-3 * days_per_year,
		      -6.90460016972063023e-5 * days_per_year,
		       9.54791938424326609e-4 * solar_mass);

  saturn = make_body( 8.34336671824457987,
		      4.12479856412430479,
		     -4.03523417114321381e-1,
		     -2.76742510726862411e-3 * days_per_year,
		      4.99852801234917238e-3 * days_per_year,
		      2.30417297573763929e-5 * days_per_year,
		      2.85885980666130812e-4 * solar_mass);

  uranus = make_body( 1.28943695621391310e1,
		     -1.51111514016986312e1,
		     -2.23307578892655734e-1,
		      2.96460137564761618e-03 * days_per_year,
		      2.37847173959480950e-03 * days_per_year,
		     -2.96589568540237556e-05 * days_per_year,
		      4.36624404335156298e-05 * solar_mass);

  
  neptune = make_body( 1.53796971148509165e+01,
		      -2.59193146099879641e+01,
		       1.79258772950371181e-01,
		       2.68067772490389322e-03 * days_per_year,
		       1.62824170038242295e-03 * days_per_year,
		      -9.51592254519715870e-05 * days_per_year,
		       5.15138902046611451e-05 * solar_mass);
  // Allocate the solar system
  solar_system = GC_MALLOC(sizeof(int64_t) + sizeof(body*) * 5);
  solar_system->size = 5;
  vector_set(solar_system, 0, sun);
  vector_set(solar_system, 1, jupiter);
  vector_set(solar_system, 2, saturn);
  vector_set(solar_system, 3, uranus);
  vector_set(solar_system, 4, neptune);
  
  offset_momentum();
  printf ("%.9lf\n", energy());
  
  timer_stopped = gettimeofday(&timer_stop_time, NULL);
  
  /* for (int i=1;i<=ar;++i) */
  /*     for (int j=1;j<=bc;++j) */
  /* 	printf ("%ld ",r[i][j]); */

  timer_report();
  
  return 0;
}

