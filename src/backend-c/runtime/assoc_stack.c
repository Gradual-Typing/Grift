// uncomment to disable assert()
// #define NDEBUG

// uncomment to compile with testing main
// #define GRIFT_TEST_ASSOC_STACK_C

#include "runtime.h"
#include <stdio.h>
#include <assert.h>
#include "boehm-gc-install/include/gc/gc.h"

// TODO these should be moved to runtime.h
#define NULL_GRIFT_OBJ ((grift_obj)(uintptr_t)0)
#define GRIFT_MALLOC(size) ((grift_obj)GC_MALLOC(size));

typedef struct {
  grift_obj fst;
  grift_obj snd;
  grift_obj mapsto;
} grift_assoc_triple; 

typedef struct {
  unsigned int size;// The total capacity of triples
  unsigned int next; // end is the index for the next free slot
  grift_assoc_triple *triples; // Pointer to Array of triples
} grift_assoc_stack;


#define GRIFT_ERROR_IN_RUNTIME 1
#define STACK_SIZE_TO_WARN       100
#define STACK_INIT_SIZE          20


void grift_assoc_stack_push(grift_assoc_stack *m,
                               grift_obj f,
                               grift_obj s,
                               grift_obj t){
  assert(m->size >= m->next);

  // if adding another association will overflow the stack
  // then double the size of the stack.
  if (m->size < m->next + 1) {

    unsigned int new_size = m->size * 2;
    size_t bytes = new_size * sizeof(grift_assoc_triple); 
    grift_assoc_triple *new_triples = GC_MALLOC(bytes);

    for(unsigned int i = 0; i < m->next; i++)
      new_triples[i] = m->triples[i];

    m->size = new_size;
    m->triples = new_triples;

    // This is a linear lookup algorithm with small expected inputs
    // warn if we get large inputs.
    if (m->size > STACK_SIZE_TO_WARN) {
      fprintf(stderr,
              __FILE__ ":[warning] assoc stack size = %u\n",
              m->size);
    }
  }

  m->triples[m->next].fst    = f;
  m->triples[m->next].snd    = s;
  m->triples[m->next].mapsto = t;
  m->next += 1;
  
  return;
}

// finds the grift_obj associated with `f` and `s`, and
// returns NULL if `f` and `s` do not have an entry.
grift_obj grift_assoc_stack_find(grift_assoc_stack *m,
                                      grift_obj f,
                                      grift_obj s){
  assert(m->size >= m->next);
  // linear time scan for matching association
  // I don't think that order actually matters here
  for(int i = 0; i < m->next; i++){
    grift_assoc_triple *t = &(m->triples[i]);
    if (t->fst.as_uint == f.as_uint &&
        t->snd.as_uint == s.as_uint   ){
      return t->mapsto;
    }
  }
  
  return NULL_GRIFT_OBJ;
}

grift_obj grift_assoc_stack_pop(grift_assoc_stack *m){ 
  assert(m->size >= m->next);
  assert(m->size > 0);
  
  if (m->next < 1) {
    fprintf(stderr,
            __FILE__ ":error tried to pop stack when \"next\" = 0\n");
    exit(GRIFT_ERROR_IN_RUNTIME);
  }

  m->next -= 1;
  return m->triples[m->next].mapsto;
}

#ifdef GRIFT_TEST_ASSOC_STACK_C
int main(){
  GC_INIT();

  // A bunch of unitialized grift objects
  grift_obj o0  = GRIFT_MALLOC(sizeof(grift_obj));
  grift_obj o1  = GRIFT_MALLOC(sizeof(grift_obj));
  grift_obj o2  = GRIFT_MALLOC(sizeof(grift_obj));
  grift_obj o3  = GRIFT_MALLOC(sizeof(grift_obj));
  grift_obj o4  = GRIFT_MALLOC(sizeof(grift_obj));
  grift_obj o5  = GRIFT_MALLOC(sizeof(grift_obj));
  grift_obj o6  = GRIFT_MALLOC(sizeof(grift_obj));
  grift_obj o7  = GRIFT_MALLOC(sizeof(grift_obj));
  grift_obj o8  = GRIFT_MALLOC(sizeof(grift_obj));
  grift_obj o9  = GRIFT_MALLOC(sizeof(grift_obj));

  // Sanity Check
  assert(o1.as_int == o1.as_int);
  assert(o1.as_int != o2.as_int);

  unsigned int size = 2;
  grift_assoc_triple *triples = GC_MALLOC(sizeof(grift_assoc_stack[size]));
  grift_assoc_stack m = {size, 0, triples};

  grift_assoc_stack_push(&m, o0, o1, o0);
  grift_assoc_stack_push(&m, o1, o1, o1);

  // Initial lookups
  assert(grift_assoc_stack_find(&m, o0, o1).as_int == o0.as_int);
  assert(grift_assoc_stack_find(&m, o1, o1).as_int == o1.as_int);
  // Lookups that should fail
  assert(grift_assoc_stack_find(&m, o0, o2).as_int ==
         NULL_GRIFT_OBJ.as_int);
  assert(grift_assoc_stack_find(&m, o1, o0).as_int ==
         NULL_GRIFT_OBJ.as_int);
  
  // Should resize the stack
  grift_assoc_stack_push(&m, o1, o2, o2);
  grift_assoc_stack_push(&m, o2, o3, o3);
  
  // Lookups that should succeed
  assert(grift_assoc_stack_find(&m, o0, o1).as_int == o0.as_int);
  assert(grift_assoc_stack_find(&m, o1, o1).as_int == o1.as_int);
  assert(grift_assoc_stack_find(&m, o1, o2).as_int == o2.as_int);
  assert(grift_assoc_stack_find(&m, o2, o3).as_int == o3.as_int);
  
  // Lookups that should fail
  assert(grift_assoc_stack_find(&m, o0, o0).as_int == 0);
  assert(grift_assoc_stack_find(&m, o2, o1).as_int == 0);
  assert(grift_assoc_stack_find(&m, o3, o2).as_int == 0);
  assert(grift_assoc_stack_find(&m, o3, o3).as_int == 0);

  grift_assoc_stack_push(&m, o3, o2, o4);
  grift_assoc_stack_push(&m, o3, o4, o5);
  grift_assoc_stack_push(&m, o4, o5, o6);
  grift_assoc_stack_push(&m, o5, o6, o7);
  grift_assoc_stack_push(&m, o6, o6, o8);
  grift_assoc_stack_push(&m, o7, o9, o9);

  // Lookups that should succeed
  assert(grift_assoc_stack_find(&m, o0, o1).as_int == o0.as_int);
  assert(grift_assoc_stack_find(&m, o1, o1).as_int == o1.as_int);
  assert(grift_assoc_stack_find(&m, o1, o2).as_int == o2.as_int);
  assert(grift_assoc_stack_find(&m, o2, o3).as_int == o3.as_int);
  assert(grift_assoc_stack_find(&m, o5, o6).as_int == o7.as_int);
  assert(grift_assoc_stack_find(&m, o3, o2).as_int == o4.as_int);

  assert(grift_assoc_stack_pop(&m).as_int == o9.as_int);
  assert(grift_assoc_stack_pop(&m).as_int == o8.as_int);
  assert(grift_assoc_stack_pop(&m).as_int == o7.as_int);

  // Lookups that should fail
  assert(grift_assoc_stack_find(&m, o5, o6).as_int == 0);
  assert(grift_assoc_stack_find(&m, o6, o6).as_int == 0);

  // But these should all work
  assert(grift_assoc_stack_find(&m, o0, o1).as_int == o0.as_int);
  assert(grift_assoc_stack_find(&m, o1, o1).as_int == o1.as_int);
  assert(grift_assoc_stack_find(&m, o1, o2).as_int == o2.as_int);
  assert(grift_assoc_stack_find(&m, o2, o3).as_int == o3.as_int);
  
  return 0;
}
#endif /* GRIFT_TEST_ASSOC_STACK_C */
