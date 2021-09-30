#include <assert.h>
#include "cast_queue.h"

void dequeue_1(){
  cast_queue* q = allocate_cast_queue();
  assert(q->capacity == CAST_QUEUE_INIT_CAPACITY);
  assert(q->front == 0);
  assert(q->rear == 0);
  grift_obj addr, type;
  ENQUEUE(q, addr, type, 991, 5);
  assert(q->capacity == CAST_QUEUE_INIT_CAPACITY);
  suspended_cast sc = cast_queue_dequeue(q);
  assert(addr.as_int == sc.address.as_int);
  assert(type.as_int == sc.type.as_int);
}

void dequeue_2(){
  cast_queue* q = allocate_cast_queue();
  grift_obj addr1, type1;
  ENQUEUE(q, addr1, type1, 991, 5);
  assert(q->front == 0 && q->rear == 1);
  grift_obj addr2, type2;
  ENQUEUE(q, addr2, type2, 892, 8);
  assert(q->front == 0 && q->rear == 2);
  suspended_cast sc = cast_queue_dequeue(q);
  assert(q->front == 1 && q->rear == 2);
  assert(addr1.as_int == sc.address.as_int);
  assert(type1.as_int == sc.type.as_int);
  sc = cast_queue_dequeue(q);
  assert(q->front == 2 && q->rear == 2);
  assert(addr2.as_int == sc.address.as_int);
  assert(type2.as_int == sc.type.as_int);
}

void dequeue_3(){
  cast_queue* q = allocate_cast_queue();
  grift_obj addr1, type1;
  ENQUEUE(q, addr1, type1, 991, 5);
  suspended_cast sc = cast_queue_dequeue(q);
  assert(q->front == 1 && q->rear == 1);
  assert(addr1.as_int == sc.address.as_int);
  grift_obj addr2, type2;
  ENQUEUE(q, addr2, type2, 892, 8);
  sc = cast_queue_dequeue(q);
  assert(q->front == 2 && q->rear == 2);
  assert(addr2.as_int == sc.address.as_int);
  assert(type2.as_int == sc.type.as_int);
}

void peek_1(){
  cast_queue* q = allocate_cast_queue();
  assert(q->capacity == CAST_QUEUE_INIT_CAPACITY);
  assert(q->front == 0);
  assert(q->rear == 0);
  grift_obj addr, type;
  ENQUEUE(q, addr, type, 991, 5);
  assert(cast_queue_peek_address(q).as_int == addr.as_int);
  assert(cast_queue_peek_type(q).as_int == type.as_int);
  grift_obj _addr, _type;  
  ENQUEUE(q, _addr, _type, 992, 6);
  assert(cast_queue_peek_address(q).as_int == addr.as_int);
  assert(cast_queue_peek_type(q).as_int == type.as_int);  
}

void resize_1(){
  cast_queue* q = allocate_cast_queue();
  int i = 0;
  grift_obj addr, type;
  for (; i < CAST_QUEUE_INIT_CAPACITY; ++i) {
    ENQUEUE(q, addr, type, i, i);
    assert(q->capacity == CAST_QUEUE_INIT_CAPACITY);
  }
  ENQUEUE(q, addr, type, i, i);
  assert(q->front == 0 && q->rear == (CAST_QUEUE_INIT_CAPACITY + 1));
  assert(q->capacity == (2 * CAST_QUEUE_INIT_CAPACITY));
}

void resize_2(){
  cast_queue* q = allocate_cast_queue();
  int i = 0;
  grift_obj addr, type;
  for (; i < CAST_QUEUE_INIT_CAPACITY; ++i) {
    ENQUEUE(q, addr, type, i, i);
    cast_queue_dequeue(q);
    assert(q->capacity == CAST_QUEUE_INIT_CAPACITY);
    assert(q->front == q->rear);
  }
}

void emptyness_1(){
  cast_queue* q = allocate_cast_queue();
  assert(cast_queue_is_empty(q));
  grift_obj addr, type;
  ENQUEUE(q, addr, type, 0, 0);
  assert(cast_queue_is_not_empty(q));
}
