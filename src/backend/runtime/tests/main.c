#include <stdio.h>
#include "cast_queue.h"
#include "gc.h"


int main() {
  GC_INIT();

  dequeue_1();
  dequeue_2();
  dequeue_3();
  peek_1();  
  resize_1();
  resize_2();
  emptyness_1();

  printf("Unit tests finished running successfully.\n");
  return 0;
}
