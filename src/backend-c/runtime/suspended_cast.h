#ifndef SUSPENDED_CAST_INCLUDED
#define SUSPENDED_CAST_INCLUDED

#include <inttypes.h>

typedef struct {
  int64_t address;
  int64_t type;
} suspended_cast;

int64_t suspended_cast_get_address(suspended_cast*);
int64_t suspended_cast_get_type(suspended_cast*);

#endif
