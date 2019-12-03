#include "suspended_cast.h"


int64_t suspended_cast_get_address(suspended_cast* sc) {
  return sc->address;
}

int64_t suspended_cast_get_type(suspended_cast* sc) {
  return sc->type;
}
