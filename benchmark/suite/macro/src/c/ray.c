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
    printf("index out of bound %d",i);		\
    exit(-1);					\
  }						\

#define vector_set(x, i, v)			\
  if ((i) >= 0 && (i) < (x)->size){		\
    (x)->data[(i)] = v;				\
  } else {					\
    printf("index out of bound %d", i);		\
    exit(-1);					\
  }						\

typedef struct{
  double x;
  double y;
  double z;
} point;


typedef struct{
  double color;
  double radius;
  point* center;
} entry;

typedef struct{
  entry* e;
  point* p;
} looprec;

typedef struct{
  int64_t size;
  entry* data[];
} vect;

typedef point*(*make_point_code_t)(double,double,double);

typedef struct{
  make_point_code_t code;
} make_point_clos;

make_point_clos* make_point;

point* make_point_code(double x, double y, double z){
  point* p = GC_MALLOC(sizeof(point));
  p->x = x;
  p->y = y;
  p->z = z;
  return p;
}

typedef double(*point_x_code_t)(point*);

typedef struct{
  point_x_code_t code;
} point_x_clos;

point_x_clos* point_x;

double point_x_code(point* p){
  return p->x;
}

typedef double(*point_y_code_t)(point*);

typedef struct{
  point_y_code_t code;
} point_y_clos;

point_y_clos* point_y;

double point_y_code(point* p){
  return p->y;
}

typedef double(*point_z_code_t)(point*);

typedef struct{
  point_z_code_t code;
} point_z_clos;

point_z_clos* point_z;

double point_z_code(point* p){
  return p->z;
}

typedef double(*sq_code_t)(double);

typedef struct{
  sq_code_t code;
} sq_clos;

sq_clos* sq;

double sq_code(double x){
  return (x*x);
}

typedef double(*mag_code_t)(double,double,double);

typedef struct{
  mag_code_t code;
} mag_clos;

mag_clos* mag;

double mag_code(double x,double y,double z){
  return sqrt(sq->code(x)+sq->code(y)+sq->code(z));
}

typedef point*(*unit_vector_code_t)(double,double,double);

typedef struct{
  unit_vector_code_t code;
} unit_vector_clos;

unit_vector_clos* unit_vector;

point* unit_vector_code(double x,double y,double z){
  double d = mag->code(x,y,z);
  return make_point->code(x/d,y/d,z/d);
}

typedef double(*distance_code_t)(point*,point*);

typedef struct{
  distance_code_t code;
} distance_clos;

distance_clos* distance;

double distance_code(point* p1, point* p2){
  return mag->code(point_x->code(p1) - point_x->code(p2),
		   point_y->code(p1) - point_y->code(p2),
		   point_z->code(p1) - point_z->code(p2));
}

vect* world;
point* eye;

typedef void(*tracer_code_t)(int);

typedef struct{
  tracer_code_t code;
} tracer_clos;

tracer_clos* tracer;

typedef int(*color_at_code_t)(double,double);

typedef struct{
  color_at_code_t code;
} color_at_clos;

color_at_clos* color_at;

void tracer_code(int res){
  int extent = res * 100;
  double fres = (double) res;
  printf("P2 %d %d 255\n", extent, extent);
  for (int y = 0; y < extent; ++y){
    for (int x = 0; x < extent; ++x){
      double fx = (double) x;
      double fy = (double) y;
      printf("%d\n",color_at->code(-50 + (fx / fres),
				   -50 + (fy / fres)));
    }
  }
}

typedef double(*sendray_code_t)(point*,point*);

typedef struct{
  sendray_code_t code;
} sendray_clos;

sendray_clos* sendray;

int color_at_code(double x, double y){
  point* ray = unit_vector->code(x - point_x->code(eye),
				 y - point_y->code(eye),
				 - (point_z->code(eye)));
  return ((int) roundf(sendray->code(eye,ray) * 255));
}

typedef looprec*(*loop_code_t)(point*,point*,int,int,vect*,entry*,point*,double);

typedef struct{
  loop_code_t code;
} loop_clos;

loop_clos* loop;

typedef double(*lambert_code_t)(entry*,point*,point*);

typedef struct{
  lambert_code_t code;
} lambert_clos;

lambert_clos* lambert;

typedef double(*sphere_color_code_t)(entry*);

typedef struct{
  sphere_color_code_t code;
} sphere_color_clos;

sphere_color_clos* sphere_color;

double sendray_code(point* pt, point* ray){
  looprec* x = loop->code(pt,ray,0,world->size,world,NULL,NULL,1e308);
  entry* s = x->e;
  point* Int = x->p;
  if (s != NULL) {
    return (lambert->code(s,Int,ray) * sphere_color->code(s));
  } else {
    return 0;
  }
}

typedef point*(*sphere_center_code_t)(entry*);

typedef struct{
  sphere_center_code_t code;
} sphere_center_clos;

sphere_center_clos* sphere_center;

typedef double(*sphere_radius_code_t)(entry*);

typedef struct{
  sphere_radius_code_t code;
} sphere_radius_clos;

sphere_radius_clos* sphere_radius;

looprec* loop_code(point* pt,
		   point* ray,
		   int index,
		   int lst_len,
		   vect* lst,
		   entry* surface,
		   point* hit,
		   double dist){
  if (index == lst_len){
    looprec* out = GC_MALLOC(sizeof(looprec*));
    out->e = surface;
    out->p = hit;
    return out;
  } else {
    entry* s;
    vector_ref(s,lst,index);
    double xr = point_x->code(ray);
    double yr = point_y->code(ray);
    double zr = point_z->code(ray);
    point* sc = sphere_center->code(s);
    double a = sq->code(xr) + sq->code(yr) + sq->code(zr);
    double b = 2.0 * (((point_x->code(pt) - point_x->code(sc)) * xr) +
		      (((point_y->code(pt) - point_y->code(sc)) * yr) +
		       ((point_z->code(pt) - point_z->code(sc)) * zr)));
    double c = sq->code(point_x->code(pt) - point_x->code(sc)) +
      sq->code(point_y->code(pt) - point_y->code(sc)) +
      sq->code(point_z->code(pt) - point_z->code(sc)) +
      (-sq->code(sphere_radius->code(s)));
    if (a == 0) {
      double n = -c/b;
      point* h = make_point->code(point_x->code(pt) + (n*xr),
				  point_y->code(pt) + (n*yr),
				  point_z->code(pt) + (n*zr));
      double d = distance->code(h,pt);
      if (d < dist){
	return loop->code(pt,ray,index+1,lst_len,lst,s,h,d);
      } else {
	return loop->code(pt,ray,index+1,lst_len,lst,surface,hit,dist);
      }
    } else {
      double disc = sq->code(b) - (4.0 * a * c);
      if (disc < 0){
	return loop->code(pt,ray,index+1,lst_len,lst,surface,hit,dist);
      } else {
	double discrt = sqrt(disc);
	double minus_b = -b;
	double two_a = a*2;
	double n = fmin((minus_b + discrt) / two_a,
			(minus_b - discrt) / two_a);
	point* h = make_point->code(point_x->code(pt) + (n*xr),
				    point_y->code(pt) + (n*yr),
				    point_z->code(pt) + (n*zr));
	double d = distance->code(h,pt);
	if (d < dist){
	  return loop->code(pt,ray,index+1,lst_len,lst,s,h,d);
	} else {
	  return loop->code(pt,ray,index+1,lst_len,lst,surface,hit,dist);
	}
      }
    }
  }
}

typedef point*(*sphere_normal_code_t)(entry*,point*);

typedef struct{
  sphere_normal_code_t code;
} sphere_normal_clos;

sphere_normal_clos* sphere_normal;

double lambert_code(entry* s, point* Int, point* ray){
  point* n = sphere_normal->code(s,Int);
  return fmax(0.0,
	      (point_x->code(ray) * point_x->code(n)) +
	      (point_y->code(ray) * point_y->code(n)) +
	      (point_z->code(ray) * point_z->code(n)));
}


typedef entry*(*make_sphere_code_t)(double,double,point*);

typedef struct{
  make_sphere_code_t code;
} make_sphere_clos;

make_sphere_clos* make_sphere;

entry* make_sphere_code(double color,double radius, point* center){
  entry* out = GC_MALLOC(sizeof(entry));
  out->color = color;
  out->radius = radius;
  out->center = center;
  return out;
}

double sphere_color_code(entry* s){
  return s->color;
}

double sphere_radius_code(entry* s){
  return s->radius;
}

point* sphere_center_code(entry* s){
  return s->center;
}

point* sphere_normal_code(entry* s, point* pt){
  point* c = sphere_center->code(s);
  return unit_vector->code(point_x->code(c)-point_x->code(pt),
			   point_y->code(c)-point_y->code(pt),
			   point_z->code(c)-point_z->code(pt));
}

typedef entry*(*defsphere_code_t)(int,double,double,double,double,double);

typedef struct{
  defsphere_code_t code;
} defsphere_clos;

defsphere_clos* defsphere;

entry* defsphere_code(int i, double x, double y, double z,
		      double r, double c){
  entry* s = make_sphere->code(c,r,make_point->code(x,y,z));
  vector_set(world,i,s);
  return s;
}

int main(int argc, char* argv[]){

  GC_INIT();

  /* Immitate closures */
  make_point = GC_MALLOC(sizeof(make_point_clos));
  make_point->code = make_point_code;

  point_x = GC_MALLOC(sizeof(point_x_clos));
  point_x->code = point_x_code;

  point_y = GC_MALLOC(sizeof(point_y_clos));
  point_y->code = point_y_code;

  point_z = GC_MALLOC(sizeof(point_z_clos));
  point_z->code = point_z_code;

  sq = GC_MALLOC(sizeof(sq_clos));
  sq->code = sq_code;

  mag = GC_MALLOC(sizeof(mag_clos));
  mag->code = mag_code;

  unit_vector = GC_MALLOC(sizeof(unit_vector_clos));
  unit_vector->code = unit_vector_code;

  distance = GC_MALLOC(sizeof(distance_clos));
  distance->code = distance_code;

  tracer = GC_MALLOC(sizeof(tracer_clos));
  tracer->code = tracer_code;

  color_at = GC_MALLOC(sizeof(color_at_clos));
  color_at->code = color_at_code;

  sendray = GC_MALLOC(sizeof(sendray_clos));
  sendray->code = sendray_code;

  lambert = GC_MALLOC(sizeof(lambert_clos));
  lambert->code = lambert_code;

  sphere_radius = GC_MALLOC(sizeof(sphere_radius_clos));
  sphere_radius->code = sphere_radius_code;

  sphere_center = GC_MALLOC(sizeof(sphere_center_clos));
  sphere_center->code = sphere_center_code;

  sphere_color = GC_MALLOC(sizeof(sphere_color_clos));
  sphere_color->code = sphere_color_code;

  make_sphere = GC_MALLOC(sizeof(make_sphere_clos));
  make_sphere->code = make_sphere_code;

  loop = GC_MALLOC(sizeof(loop_clos));
  loop->code = loop_code;

  sphere_normal = GC_MALLOC(sizeof(sphere_normal_clos));
  sphere_normal->code = sphere_normal_code;

  defsphere = GC_MALLOC(sizeof(defsphere_clos));
  defsphere->code = defsphere_code;

  int n = 33;
  world = GC_MALLOC(sizeof(int64_t) + n*sizeof(entry*));
  world->size = 33;
  defsphere->code(32,0,-300,-1200,200,0.8);
  defsphere->code(31,-80,-150,-1200,200,0.7);
  defsphere->code(30,70,-100,-1200,200,0.9);
  int counter = 29;
  double fx;
  double fz;
  for (int x = -2; x < 3; ++x){
    for (int z = 2; z < 8; ++z){
      fx = (double) x;
      fz = (double) z;
      defsphere->code(counter,fx*200,300,fz*-400,40,0.75);
      counter--;
    }
  }

  eye = GC_MALLOC(sizeof(point));
  eye->x = 0;
  eye->y = 0;
  eye->z = 200;

  tracer->code(1);
  
  return 0;
}
