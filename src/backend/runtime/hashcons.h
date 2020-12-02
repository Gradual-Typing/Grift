#ifndef HASHCONS_INCLUDED
#define HASHCONS_INCLUDED
#include <stdint.h>

struct list {
  int64_t data;
  struct list* next;
};
typedef struct list* list;

struct chain {
  list list;
};

typedef struct chain* chain;

struct table {
  int64_t slots;
  int64_t num_elems;
  float load_factor;
  chain* array;
};

typedef struct table* table;

table alloc_hash_table(int64_t slots, float load_factor);
int64_t hashcons(table ht, int64_t e, int64_t hcode);
int types_equal(int64_t t1, int64_t t2);
void types_reinsert(table ht, int64_t ty);

#endif
