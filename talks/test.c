
#include <stdlib.h>
#include <stdio.h>

long blame(long cp){
  // 3
  // By C standards this is undefined behavior but we are about to crash
  // the program anyway.
  puts((char*)((long*)cp)[1]);
  exit(1);
}

int main (){

  // 1
  // The closure is allocated by some means
  // In the case of a closure that is just a lazy blame then
  // this would be performed in the caster procedure.
  long *clos = (long*) malloc(2 * sizeof(long));
  clos[0] = (long) blame;
  clos[1] = (long) "This is the implementation of blame labels";

  // 2
  // At a function call the code pointer is casted to a function that
  // takes the number of arguments that we externally think it should take.
  ((long (*) (long, long, long)) clos[0])((long) clos, 1, 2);
}
