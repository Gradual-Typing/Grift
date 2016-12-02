#include<stdlib.h>
#include<stdio.h>
#include<stdint.h>
#include <inttypes.h>
#include <math.h>
#define GC_INITIAL_HEAP_SIZE 1048576
#include "gc.h"

typedef struct{
  int64_t size;
  double data[];
} body;

typedef struct{
  int64_t size;
  body*  data[];
} bodies;

#define apply(c, args...) ((c)*(args)) 

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


  
double pi = 3.141592653589793;
double days_per_year = 365.24;
double solar_mass    = 39.47841760435743; //4.0 * pi * pi;
double dt            = 0.01;
body* sun;
body* jupiter;
body* saturn;
body* uranus;
body* neptune;
bodies* solar_system;
int system_size = 5;

typedef body*(*make_body_code_t)(double, double, double,
				 double, double, double,
				 double);

typedef struct{
  make_body_code_t code;
} make_body_clos;

make_body_clos* make_body;
body* make_body_code(double x, double y, double z,
		     double vx, double vy, double vz,
		     double mass){
  body* v = GC_MALLOC(8 * sizeof(int64_t));
  v->size = 7;
  vector_set(v, 0, x);
  vector_set(v, 1, y);
  vector_set(v, 2, z);
  vector_set(v, 3, vx);
  vector_set(v, 4, vy);
  vector_set(v, 5, vz);
  vector_set(v, 6, mass);
  return v;
}

typedef void(*offset_momentum_loop_t)(int, double, double, double);
typedef struct{
  offset_momentum_loop_t code;
} offset_momentum_loop_clos;

offset_momentum_loop_clos* offset_momentum_loop;

void offset_momentum_loop_code(int i1, double px, double py, double pz){
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
    offset_momentum_loop->code(i1+1,
			       px + (j_vx * j_mass),
			       py + (j_vy * j_mass),
			       pz + (j_vz * j_mass));
  }
}

#define sq(x) ((x)*(x))

typedef double(*energy_loop_i_t)(int, double, body*, int);
typedef struct {
  energy_loop_i_t code;
} energy_loop_i_clos;

energy_loop_i_clos* energy_loop_i;

typedef double(*energy_loop_o_t)(int, double);
typedef struct {
  energy_loop_o_t code;
} energy_loop_o_clos;

energy_loop_o_clos* energy_loop_o;

double energy_loop_o_code(int o, double e){
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
    double sum_sqs = sq(ob_vx) + sq(ob_vy) + sq(ob_vz);
    // printf("loop_o sqs %.9lf\n", sum_sqs);
    double new_e   = e + (0.5 * ob_mass * sum_sqs);
    // printf("loop_o new_e %.9lf\n", new_e);
    return energy_loop_i->code(o, new_e, ob, o + 1);
  }
}

double energy_loop_i_code(int o, double e, body* ob, int i){
  if (i == system_size) {
    return energy_loop_o->code(o + 1, e);
  } else {
    body* ib;
    double ib_x, ib_y, ib_z, ib_mass;
    double ob_x, ob_y, ob_z, ob_mass;
    vector_ref(ib, solar_system, i);
    vector_ref(ib_x, ib, 0);
    vector_ref(ib_y, ib, 1);
    vector_ref(ib_z, ib, 2);
    vector_ref(ib_mass, ib, 6);
    vector_ref(ob_x, ob, 0);
    vector_ref(ob_y, ob, 1);
    vector_ref(ob_z, ob, 2);
    vector_ref(ob_mass, ob, 6);
    double dx = ob_x - ib_x;
    double dy = ob_y - ib_y;
    double dz = ob_z - ib_z;
    double dist = sqrt(sq(dx) + sq(dy) + sq(dz));
    //printf("loop_i dist %.9lf\n", dist);
    double new_e = e - ((ob_mass * ib_mass) / dist);
    //printf("loop_i new_e %.9lf\n", new_e);
    return energy_loop_i->code(o, new_e, ob, i + 1);    
  }
}

typedef double(*energy_t)();
typedef struct {
  energy_t code;
} energy_clos;

energy_clos* energy;
  
double energy_code(){
  return energy_loop_o->code(0, 0.0);
}

typedef void(*offset_momentum_t)();
typedef struct {
  offset_momentum_t code;
} offset_momentum_clos;

offset_momentum_clos* offset_momentum;

void offset_momentum_code(){
  offset_momentum_loop->code(0, 0.0, 0.0, 0.0);
}

typedef void(*advance_loop_i_t)(int, body*, double, double, double);
typedef struct{
  advance_loop_i_t code;
} advance_loop_i_clos;
advance_loop_i_clos* advance_loop_i;

void advance_loop_i_code(int i, body* ob, double vx, double vy, double vz){
  if (i < system_size) {
    body* ib;
    double ib_x, ib_y, ib_z, ib_vx, ib_vy, ib_vz, ib_mass;
    double ob_x, ob_y, ob_z, ob_mass;
    vector_ref(ib, solar_system, i);
    vector_ref(ib_x, ib, 0);
    vector_ref(ib_y, ib, 1);
    vector_ref(ib_z, ib, 2);
    vector_ref(ib_vx, ib, 3);
    vector_ref(ib_vy, ib, 4);
    vector_ref(ib_vz, ib, 5);
    vector_ref(ib_mass, ib, 6);
    vector_ref(ob_x, ob, 0);
    vector_ref(ob_y, ob, 1);
    vector_ref(ob_z, ob, 2);
    vector_ref(ob_mass, ob, 6);
    double dx = ob_x - ib_x;
    double dy = ob_y - ib_y;
    double dz = ob_z - ib_z;
    double dist2 = sq(dx) + sq(dy) + sq(dz);
    double mag   = dt / (dist2 * sqrt(dist2));
    double dxmag = dx * mag;
    double dymag = dy * mag;
    double dzmag = dz * mag;
    vector_set(ib, 3, ib_vx + (dxmag * ob_mass));
    vector_set(ib, 4, ib_vy + (dymag * ob_mass));
    vector_set(ib, 5, ib_vz + (dzmag * ob_mass));
    advance_loop_i->code(i + 1,
			 ob,
			 vx - (dxmag * ib_mass),
			 vy - (dymag * ib_mass),
			 vz - (dzmag * ib_mass));
  }else{
    double ob_x, ob_y, ob_z;
    vector_ref(ob_x, ob, 0);
    vector_ref(ob_y, ob, 1);
    vector_ref(ob_z, ob, 2);
    vector_set(ob, 3, vx);
    vector_set(ob, 4, vy);
    vector_set(ob, 5, vz);
    vector_set(ob, 0, ob_x + (dt * vx));
    vector_set(ob, 1, ob_y + (dt * vy));
    vector_set(ob, 2, ob_z + (dt * vz));
  }
}

typedef void(*advance_loop_o_t)(int);
typedef struct{
  advance_loop_o_t code;
} advance_loop_o_clos;
advance_loop_o_clos* advance_loop_o;

void advance_loop_o_code(int o){
  if (o != system_size){
    body* ob;
    //double ob_x, ob_y, ob_z, ob_mass;
    double ob_vx, ob_vy, ob_vz;
    vector_ref(ob, solar_system, o);
    vector_ref(ob_vx, ob, 3);
    vector_ref(ob_vy, ob, 4);
    vector_ref(ob_vz, ob, 5);
    advance_loop_i->code(o+1, ob, ob_vx, ob_vy, ob_vz);
    /* vector_ref(ob_x, ob, 0); */
    /* vector_ref(ob_y, ob, 1); */
    /* vector_ref(ob_z, ob, 2); */
    /* vector_ref(ob_vx, ob, 3); */
    /* vector_ref(ob_vy, ob, 4); */
    /* vector_ref(ob_vz, ob, 5); */
    /* vector_ref(ob_mass, ob, 6); */
    /* printf ("{o = %d,\nx = %lf,\ny = %lf,\nz = %lf,\n", */
    /* 	    o, ob_x, ob_y, ob_z); */
    /* printf ("vx = %lf,\nvy = %lf,\nvz = %lf,\nmass = %lf}\n", */
    /* 	    ob_vx, ob_vy, ob_vz,ob_mass); */
    advance_loop_o->code(o+1);
  }
}

typedef void(*advance_t)();
typedef struct{
  advance_t code;
} advance_clos;
advance_clos* advance;

void advance_code(){
  advance_loop_o->code(0);
}
  

int main(int argc, char* argv[]){
  unsigned long iters;
  if (argc > 1) {
    iters = atoi(argv[1]);
  } else {
    if (!scanf("%lu", &iters)){
      fputs("invalid input", stderr);
      exit(-1);
    } 
  }

  GC_INIT();

  /* Immitate closures */
  make_body = GC_MALLOC(sizeof(make_body_clos));
  make_body->code = make_body_code;
  energy = GC_MALLOC(sizeof(energy_clos));
  energy->code = energy_code;
  energy_loop_o = GC_MALLOC(sizeof(energy_loop_o_clos));
  energy_loop_o->code = energy_loop_o_code;
  energy_loop_i = GC_MALLOC(sizeof(energy_loop_i_clos));
  energy_loop_i->code = energy_loop_i_code;
  offset_momentum_loop = GC_MALLOC(sizeof(offset_momentum_loop_clos));
  offset_momentum_loop->code = offset_momentum_loop_code;
  offset_momentum = GC_MALLOC(sizeof(offset_momentum_clos));
  offset_momentum->code = offset_momentum_code;
  advance = GC_MALLOC(sizeof(advance_clos));
  advance->code = advance_code;
  advance_loop_o = GC_MALLOC(sizeof(advance_loop_o_clos));
  advance_loop_o->code = advance_loop_o_code;
  advance_loop_i = GC_MALLOC(sizeof(advance_loop_i_clos));
  advance_loop_i->code = advance_loop_i_code;
  
  sun = make_body->code(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, solar_mass);

  jupiter = make_body->code( 4.84143144246472090,
			     -1.16032004402742839,
			     -1.03622044471123109e-1,
			     1.66007664274403694e-3 * days_per_year,
			     7.69901118419740425e-3 * days_per_year,
			     -6.90460016972063023e-5 * days_per_year,
			     9.54791938424326609e-4 * solar_mass);

  saturn = make_body->code( 8.34336671824457987,
			    4.12479856412430479,
			    -4.03523417114321381e-1,
			    -2.76742510726862411e-3 * days_per_year,
			    4.99852801234917238e-3 * days_per_year,
			    2.30417297573763929e-5 * days_per_year,
			    2.85885980666130812e-4 * solar_mass);

  uranus = make_body->code( 1.28943695621391310e1,
			    -1.51111514016986312e1,
			    -2.23307578892655734e-1,
			    2.96460137564761618e-03 * days_per_year,
			    2.37847173959480950e-03 * days_per_year,
			    -2.96589568540237556e-05 * days_per_year,
			    4.36624404335156298e-05 * solar_mass);

  
  neptune = make_body->code( 1.53796971148509165e+01,
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

  offset_momentum->code();
  printf ("%.9lf\n", energy->code());
  for(unsigned long i = 0; i < iters; i++) {
    advance->code();
  }
  printf ("%.9lf\n", energy->code());
  
  return 0;
}

