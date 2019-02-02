// uncomment to compile with testing main
// #define GRIFT_TEST_ASSOC_STACK_C

#include "runtime.h"
#include <stdio.h>
#include <assert.h>

grift_assoc_stack* grift_make_assoc_stack(){
  grift_assoc_stack *as = GC_NEW(grift_assoc_stack);
  as->size = 0;
  as->next = 0;
  as->triples = NULL;
  return as;
}

void grift_assoc_stack_resize(grift_assoc_stack *as, uintptr_t new_size){
  assert(new_size >= as->next);
  assert(new_size > 0);
  
  grift_assoc_triple *ts = GC_MALLOC(sizeof(grift_assoc_triple[new_size]));

  int i = 0;
  for(; i < as->next; i++){
    ts[i] = as->triples[i];
  }
  
#ifndef NDEBUG
  for (; i < new_size; i++) {
    ts[i].fst = NULL_GRIFT_OBJ;
    ts[i].snd = NULL_GRIFT_OBJ;
    ts[i].mapsto = NULL_GRIFT_OBJ;
  }
  // This is a linear lookup algorithm with small expected inputs
  // warn if we get large inputs.
  if (new_size > STACK_SIZE_TO_WARN) {
    fprintf(stderr,
            __FILE__ ":[warning] assoc stack size = %"PRIuPTR"\n",
            new_size);
  }
#endif

  if(as->triples){
    GC_FREE(as->triples);
  }

  as->triples = ts;
  as->size = new_size;
}

void grift_assoc_stack_push(grift_assoc_stack *as,
                            grift_obj f,
                            grift_obj s,
                            grift_obj t){
  assert(as->next <= as->size);
  
  // if adding another association will overflow the stack
  // then double the size of the stack.
  if (as->next == as->size) {
    if (as->size == 0) {
      grift_assoc_stack_resize(as, STACK_INIT_SIZE);
    } else {
      grift_assoc_stack_resize(as, 2 * as->size);
    }
  }

  as->triples[as->next].fst    = f;
  as->triples[as->next].snd    = s;
  as->triples[as->next].mapsto = t;
  as->next += 1;  
}

// finds the grift_obj associated with `f` and `s`, and
// returns its index in the stack. It returns -1
// if `f` and `s` do not have an entry.
intptr_t grift_assoc_stack_find(grift_assoc_stack *as,
                                grift_obj f,
                                grift_obj s){
  assert(as->size >= as->next);
  // linear time scan for matching association
  // I don't think that order actually matters here
  for(int i = 0; i < as->next; i++){
    grift_assoc_triple *t = &(as->triples[i]);
    if (t->fst.as_uint == f.as_uint &&
        t->snd.as_uint == s.as_uint   ){
      return i;
    }
  }
  return -1;
}

grift_obj grift_assoc_stack_pop(grift_assoc_stack *as){ 
  assert(as->size >= as->next);
  assert(as->next > 0);

  as->next -= 1;
  grift_obj tmp = as->triples[as->next].mapsto;
  
  as->triples[as->next].fst = NULL_GRIFT_OBJ;
  as->triples[as->next].snd = NULL_GRIFT_OBJ;
  as->triples[as->next].mapsto = NULL_GRIFT_OBJ;

  return tmp;
}

grift_obj grift_assoc_stack_ref(grift_assoc_stack *as, intptr_t i){
  assert(i > -1);
  assert(i < as->size);
  return as->triples[i].mapsto;
}

void grift_assoc_stack_set(grift_assoc_stack *as, intptr_t i, grift_obj v){
  assert(i > -1);
  assert(i < as->size);
  as->triples[i].mapsto = v;
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

  grift_assoc_stack *s = grift_make_assoc_stack();

  grift_assoc_stack_resize(s, 2);
  
  grift_assoc_stack_push(s, o0, o1, o0);
  grift_assoc_stack_push(s, o1, o1, o1);

  // Initial lookups
  assert(grift_assoc_stack_find(s, o0, o1) == 0);
  assert(grift_assoc_stack_find(s, o1, o1) == 1);
  // Lookups that should fail
  assert(grift_assoc_stack_find(s, o0, o2) == -1);
  assert(grift_assoc_stack_find(s, o1, o0) == -1);
  
  // Should resize the stack
  grift_assoc_stack_push(s, o1, o2, o2);
  grift_assoc_stack_push(s, o2, o3, o3);
  
  // Lookups that should succeed
  assert(grift_assoc_stack_find(s, o0, o1) == 0);
  assert(grift_assoc_stack_find(s, o1, o1) == 1);
  assert(grift_assoc_stack_find(s, o1, o2) == 2);
  assert(grift_assoc_stack_find(s, o2, o3) == 3);
  
  // Lookups that should fail
  assert(grift_assoc_stack_find(s, o0, o0) == -1);
  assert(grift_assoc_stack_find(s, o2, o1) == -1);
  assert(grift_assoc_stack_find(s, o3, o2) == -1);
  assert(grift_assoc_stack_find(s, o3, o3) == -1);

  grift_assoc_stack_push(s, o3, o2, o4);
  grift_assoc_stack_push(s, o3, o4, o5);
  grift_assoc_stack_push(s, o4, o5, o6);
  grift_assoc_stack_push(s, o5, o6, o7);
  grift_assoc_stack_push(s, o6, o6, o8);
  grift_assoc_stack_push(s, o7, o9, o9);

  // Lookups that should succeed
  assert(grift_assoc_stack_find(s, o0, o1) == 0);
  assert(grift_assoc_stack_find(s, o1, o1) == 1);
  assert(grift_assoc_stack_find(s, o1, o2) == 2);
  assert(grift_assoc_stack_find(s, o2, o3) == 3);
  assert(grift_assoc_stack_find(s, o5, o6) == 7);
  assert(grift_assoc_stack_find(s, o3, o2) == 4);

  assert(grift_assoc_stack_pop(s).as_int == o9.as_int);
  assert(grift_assoc_stack_pop(s).as_int == o8.as_int);
  assert(grift_assoc_stack_pop(s).as_int == o7.as_int);

  // Lookups that should fail
  assert(grift_assoc_stack_find(s, o5, o6) == -1);
  assert(grift_assoc_stack_find(s, o6, o6) == -1);

  // But these should all work
  assert(grift_assoc_stack_find(s, o0, o1) == 0);
  assert(grift_assoc_stack_find(s, o1, o1) == 1);
  assert(grift_assoc_stack_find(s, o1, o2) == 2);
  assert(grift_assoc_stack_find(s, o2, o3) == 3);
  
  return 0;
}
#endif /* GRIFT_TEST_ASSOC_STACK_C */
