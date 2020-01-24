#include <assert.h>
#include "boehm-gc-install/include/gc/gc.h"
#include "cast_queue.h"


cast_queue* allocate_cast_queue() {
  cast_queue *cq = GC_NEW(cast_queue);
  cq->casts = GC_MALLOC(sizeof(suspended_cast[CAST_QUEUE_INIT_CAPACITY]));
  cq->capacity = CAST_QUEUE_INIT_CAPACITY;
  cq->front = 0;
  cq->rear = 0;
  cq->size = 0;
  return cq;
}

void cast_queue_enqueue(cast_queue* cq, int64_t addr, int64_t ty){
  assert(cq->rear <= cq->capacity);
  assert(cq->front <= cq->capacity);

  if (cast_queue_size(cq) == cq->capacity) {
    cast_queue_resize(cq);
  }

  cq->casts[cq->rear] = (suspended_cast) { .address = addr, .type = ty };
  cq->rear = (cq->rear + 1) % cq->capacity;
  cq->size++;
}

suspended_cast cast_queue_dequeue(cast_queue* cq){ 
  assert(cq->rear <= cq->capacity);
  assert(cq->front <= cq->capacity);

  suspended_cast rv = cq->casts[cq->front];
  cq->casts[cq->front] = (suspended_cast) { .address = 0, .type = 0 };
  cq->front = (cq->front + 1) % cq->capacity;
  cq->size--;
  return rv;
}

int64_t cast_queue_peek_address(cast_queue* cq) {
  return suspended_cast_get_address(&(cq->casts[cq->front]));
}

int64_t cast_queue_peek_type(cast_queue* cq) {
  return suspended_cast_get_type(&(cq->casts[cq->front]));
}

void cast_queue_resize(cast_queue* cq){
  int capacity = CAST_QUEUE_CAPACITY_MULTIPLE_FACTOR * cq->capacity;

  suspended_cast* casts = GC_MALLOC(sizeof(suspended_cast[capacity]));

  int i = 0;
  for(; i < cq->capacity; ++i){
    casts[i] = cq->casts[(cq->front + i) % cq->capacity];
  }

  if(cq->casts){
    GC_FREE(cq->casts);
  }

  cq->casts = casts;
  cq->rear = cq->capacity;
  cq->capacity = capacity;
  cq->front = 0;
}

int cast_queue_size(cast_queue* cq){
  return cq->size;
}

int cast_queue_is_empty(cast_queue* cq) {
  return (cq->size == 0);
}

int cast_queue_is_not_empty(cast_queue* cq) {
  return !cast_queue_is_empty(cq);
}
