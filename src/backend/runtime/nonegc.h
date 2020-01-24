#ifndef NONEGC
#define NONEGC
#include <stdint.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

void*   nonegc_alloc_ptr;
int64_t nonegc_free_ptr;
int64_t nonegc_limit;
int64_t nonegc_allocated_memory;
long    nonegc_memory_extension_bytes;
int64_t nonegc_result;
int64_t nonegc_new_free;

int64_t nonegc_malloc(int64_t size);
void nonegc_init(int64_t initial_heap_bytes);

#define ERROR_OUT_OF_MEMORY -2

#endif
