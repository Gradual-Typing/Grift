#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include "boehm-gc-install/include/gc/gc.h"
#include "hashcons.h"
#include "constants.h"

table alloc_hash_table(int64_t slots, float load_factor)
{
  table ht = GC_MALLOC(8 * 4);
  ht->slots = slots;
  ht->num_elems = 0;
  ht->load_factor = load_factor;
  ht->array = GC_MALLOC(8 * slots);
  return ht;
}

int64_t hashcons(table ht, int64_t* e, int64_t hcode)
{
  float current_load = (float) ht->num_elems/(float) ht->slots;
  if (current_load > ht->load_factor) {
    int64_t old_slots = ht->slots;
    chain* old_array = ht->array;
    int64_t new_slots = old_slots * 2;
    ht->slots = new_slots;
    ht->array = GC_MALLOC(8 * new_slots);
    int i;
    for (i = 0; i < old_slots; ++i) {
      chain C = old_array[i];
      if (C != NULL) {
	list p = C->list;
	while (p != NULL) {
	  types_reinsert(ht, p->data);
	  p = p->next;
	}
      }
    }
  }
  int h = hcode % ht->slots;
  h = h < 0 ? h + ht->slots : h;
  chain C = ht->array[h];
  if (C == NULL) {
    C = GC_MALLOC(8 * 1);
    ht->array[h] = C;
    list new_item = GC_MALLOC(8 * 2);
    new_item->data = e;
    new_item->next = NULL;
    C->list = new_item;
    ht->num_elems++;
    return (int64_t) e;
  }
  list p = C->list;
  while (p != NULL) {
    if (types_equal(e, p->data))
      return (int64_t) p->data;
    p = p->next;
  }
  list new_item = GC_MALLOC(8 * 2);
  new_item->data = e;
  new_item->next = C->list;
  C->list = new_item;
  ht->num_elems++;
  return (int64_t) e;
}

int types_equal(int64_t* t1, int64_t* t2)
{
  int64_t tag1 = t1[TYPE_TAG_INDEX];
  int64_t tag2 = t2[TYPE_TAG_INDEX];
  int64_t count, i;
  if (tag1 == tag2) {
    switch (tag1) {
    case TYPE_GREF_TAG ... TYPE_MVECT_TAG:
	  // the type index is the same for gref,gvect,mref, and mvect.
      return t1[TYPE_GREF_TYPE_INDEX] == t2[TYPE_GREF_TYPE_INDEX];
      break;
    case TYPE_TUPLE_TAG:
      count = t1[TYPE_TUPLE_COUNT_INDEX];
      // the loop checks count along with the elements
      for (i = TYPE_TUPLE_ELEMENTS_OFFSET-1; i < (count + TYPE_TUPLE_ELEMENTS_OFFSET); ++i) {
	if (t1[i] != t2[i])
	  return false;
      }
      return true;
      break;
    case 0:
      count = t1[TYPE_FN_ARITY_INDEX];
      // the loop checks the arity and the return type along with the elements
      for (i = TYPE_FN_FMLS_OFFSET-2; i < (count + TYPE_FN_FMLS_OFFSET); ++i) {
	if (t1[i] != t2[i])
	  return false;
      }
      return true;
      break;
    default:
      (printf("hashcons/types-equal: invalid tag: %" PRId64, tag1));
      exit(1);
    }
  }
  return false;
}

void types_reinsert(table ht, int64_t* ty)
{
  int64_t tag = ty[TYPE_TAG_INDEX];
  int64_t h;
  switch (tag) {
  case TYPE_FN_TAG ... TYPE_TUPLE_TAG:
    h = ty[TYPE_HASH_INDEX] % ht->slots;
    h = h < 0 ? h + ht->slots : h;
    chain C = ht->array[h];
    if (C == NULL) {
      C = GC_MALLOC(8 * 1);
      ht->array[h] = C;
      C->list = NULL;
    }
    list new_item = GC_MALLOC(8 * 2);
    new_item->data = ty;
    new_item->next = C->list;
    C->list = new_item;
    break;
  default:
    (printf("hashcons/types-reinsert: invalid tag: %" PRId64, tag));
    exit(1);
  }
}
