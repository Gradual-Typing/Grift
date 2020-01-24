// These Macros are usually defined by grift when the c compiler is invoked
// but this adds some sane defaults for by hand debugging
#ifndef GC_INITIAL_HEAP_SIZE
#define GC_INITIAL_HEAP_SIZE (1024*1024)
#endif

#include "runtime.h"
// TODO consider putting this in the runtime
#include <sys/time.h>
struct timeval timer_start_time;
struct timeval timer_stop_time;
struct timeval timer_result_time;
#define get_subseconds(t) (t.tv_usec / 1.0e6)

int timer_started = 1;
int timer_stopped = 1;
void timer_report(){
    // some very minor error checking
    if(timer_started){
      printf("error starting timer");
      exit(-1);
    }
    if(timer_stopped){
      printf("error stopping timer");
      exit(-1);
    }
    double t1 = timer_start_time.tv_sec + (timer_start_time.tv_usec / 1.0e6);
    double t2 = timer_stop_time.tv_sec + (timer_stop_time.tv_usec / 1.0e6);
    printf("time (sec): %lf\n", t2 - t1);
}




#ifndef INIT_TYPES_HT_SLOTS
#define INIT_TYPES_HT_SLOTS 50
#endif

#ifndef TYPES_HT_LOAD_FACTOR
#define TYPES_HT_LOAD_FACTOR .75
#endif

table types_ht;
int64_t types_unique_index_counter = 0;
cast_queue* mref_cast_q;
cast_queue* mvect_cast_q;

void grift_main();

int main(int argc, char** argv){
  
  GC_INIT();
  types_ht = alloc_hash_table(INIT_TYPES_HT_SLOTS, TYPES_HT_LOAD_FACTOR);
  mref_cast_q = allocate_cast_queue();
  mvect_cast_q = allocate_cast_queue();

  grift_main();

#ifdef CAST_PROFILER
  print_cast_profiler_results();
  write_cast_profiler_results_file(argv[0]);
#endif

  return 0;
}
