#include "../cast_queue.h"

#define ENQUEUE(q, addr, type, addr_val, type_val) {\
  addr.as_int = addr_val;\
  type.as_int = type_val;\
  cast_queue_enqueue(q, addr, type); }

void dequeue_1();
void dequeue_2();
void dequeue_3();
void resize_1();
void resize_2();
void emptyness_1();
void peek_1();
