#ifndef CAST_QUEUE_INCLUDED
#define CAST_QUEUE_INCLUDED

#include "suspended_cast.h"

typedef struct {
  int capacity;
  int size;
  int front;
  int rear;
  suspended_cast* casts;
} cast_queue;

#define CAST_QUEUE_INIT_CAPACITY 1000
#define CAST_QUEUE_CAPACITY_MULTIPLE_FACTOR 2

cast_queue* allocate_cast_queue();
void cast_queue_resize(cast_queue*);
void cast_queue_enqueue(cast_queue*, int64_t, int64_t);
suspended_cast cast_queue_dequeue(cast_queue*);
int64_t cast_queue_peek_address(cast_queue*);
int64_t cast_queue_peek_type(cast_queue*);
int cast_queue_size(cast_queue*);
int cast_queue_is_empty(cast_queue*);
int cast_queue_is_not_empty(cast_queue*);

#endif
