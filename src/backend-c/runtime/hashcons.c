#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include "boehm-gc-install/include/gc/gc.h"
#include "hashcons.h"

table alloc_hash_table(int64_t slots, float load_factor)
{
  table ht = GC_MALLOC(8 * 4);
  ht->slots = slots;
  ht->num_elems = 0;
  ht->load_factor = load_factor;
  ht->array = GC_MALLOC(8 * slots);
  return ht;
}

int64_t hashcons(table ht, int64_t e, int64_t hcode)
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
    return e;
  }
  list p = C->list;
  while (p != NULL) {
    if (types_equal(e, p->data))
      return p->data;
    p = p->next;
  }
  list new_item = GC_MALLOC(8 * 2);
  new_item->data = e;
  new_item->next = C->list;
  C->list = new_item;
  ht->num_elems++;
  return e;
}

int types_equal(int64_t t1, int64_t t2)
{
  int64_t tag1 = (t1 & 7);
  int64_t tag2 = (t2 & 7);
  int64_t count, untagged_t1, untagged_t2, i;
  if (tag1 == tag2) {
    untagged_t1 = (t1 ^ tag1);
    untagged_t2 = (t2 ^ tag1);
    switch (tag1) {
    case 1 ... 4:
      return ((int64_t*)untagged_t1)[2] == ((int64_t*)untagged_t2)[2];
      break;
    case 5:
      count = ((int64_t*)untagged_t1)[2];
      for (i = 2; i < (count + 3); ++i) {
	if (((int64_t*)untagged_t1)[i] != ((int64_t*)untagged_t2)[i])
	  return false;
      }
      return true;
      break;
    case 0:
      count = ((int64_t*)untagged_t1)[2];
      for (i = 2; i < (count + 4); ++i) {
	if (((int64_t*)untagged_t1)[i] != ((int64_t*)untagged_t2)[i])
	  return false;
      }
      return true;
      break;
    default:
      (printf("hashcons/types-equal: invalid tag: %ld", tag1));
      exit(1);
    }
  }
  return false;
}

void types_reinsert(table ht, int64_t ty)
{
  int64_t tag = (ty & 7);
  int64_t untagged_ty = (ty ^ tag);
  int64_t h;
  switch (tag) {
  case 0 ... 5:
    h = ((int64_t*)untagged_ty)[1] % ht->slots;
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
    (printf("hashcons/types-reinsert: invalid tag: %ld", tag));
    exit(1);
  }
}
