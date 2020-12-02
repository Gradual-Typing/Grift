#include "nonegc.h"
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

int64_t nonegc_malloc(int64_t size){
  nonegc_result = nonegc_free_ptr;
  nonegc_new_free = nonegc_result + size;
  nonegc_allocated_memory += size;
  if (nonegc_new_free >= nonegc_limit){
    //fputs("Requesting more memory\n", stderr);
    posix_memalign(&nonegc_alloc_ptr, 8, nonegc_memory_extension_bytes);
    nonegc_free_ptr = (int64_t)nonegc_alloc_ptr;
    if (nonegc_free_ptr == (int64_t)NULL){
      fputs("couldn't allocate any more memory\n", stderr);
      exit(ERROR_OUT_OF_MEMORY);
    }
    nonegc_limit = nonegc_free_ptr + nonegc_memory_extension_bytes;
    return nonegc_malloc(size);
  }
  nonegc_free_ptr = nonegc_new_free;
  return nonegc_result;
}

void nonegc_init(int64_t initial_heap_bytes){
  
  posix_memalign(&nonegc_alloc_ptr, 8, initial_heap_bytes);
  nonegc_free_ptr = (int64_t)nonegc_alloc_ptr;

  if (nonegc_free_ptr == (int64_t)NULL){
    fputs("couldn't allocate initial memory\n", stderr);
    exit(ERROR_OUT_OF_MEMORY);
  }

  nonegc_limit = nonegc_free_ptr + initial_heap_bytes;
  nonegc_allocated_memory = 0;

  long page_size = sysconf(_SC_PAGESIZE);
  long num_pages = 1024;
  
  if (page_size == 0) {
    nonegc_memory_extension_bytes = num_pages * 4096;
  } else {
    nonegc_memory_extension_bytes = num_pages * page_size;
  }

}


