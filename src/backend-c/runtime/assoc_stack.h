// TODO Need to abstract over allocation at the C level
typedef struct {
  grift_obj fst;
  grift_obj snd;
  grift_obj mapsto;
} grift_assoc_triple; 

typedef struct {
  unsigned int size;// The total capacity of triples
  unsigned int next; // next is the index for the next free slot
  grift_assoc_triple *triples; // Pointer to Array of triples
} grift_assoc_stack;

#define STACK_SIZE_TO_WARN       100
#define STACK_INIT_SIZE          20

/*
  returns a pointer to an initialized assoc_stack with size 0
 */
grift_assoc_stack* grift_make_assoc_stack();

/*
  Update the size of an assoc_stack and reallocate data.
  requires that new_size >= s->next
  requires that new_size > 0
 */
void grift_assoc_stack_resize(grift_assoc_stack *s, uintptr_t new_size);

/*
  Push a new triple on the assoc_stack, it resizes automatically if
  need.
*/
void grift_assoc_stack_push(grift_assoc_stack *m,
                            grift_obj f,
                            grift_obj s,
                            grift_obj t);

/*
  return the indice of the association of f and s, if f and s
  are not in the assoc stack return -1;
 */
intptr_t grift_assoc_stack_find(grift_assoc_stack *m,
                                grift_obj f,
                                grift_obj s);

/*
  return the object mapped to by the most recent association
  and pop the triple off the stack.
  requires that the assoc_stack isn't empty
*/
grift_obj grift_assoc_stack_pop(grift_assoc_stack *m);

/* 
   return the grift_object mapped to by index i
   requires 0 <= i < s->next
*/
grift_obj grift_assoc_stack_ref(grift_assoc_stack *m, intptr_t i);

/*
   mutate the grift_object mapped to by index i to v.
   requires 0 <= i < s->next
*/
void grift_assoc_stack_set(grift_assoc_stack *m, intptr_t i, grift_obj v);
