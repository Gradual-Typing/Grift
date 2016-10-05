

// Change for fn_app
int64_t u4_run_test(int64_t clos, int64_t i, int64_t acc) { 
  int64_t fn_clos = ((int64_t*)clos)[2];
  int64_t (*fn)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) fn_clos)[0];
  
  return fn(fn_clos, acc);
}

// Changes for ref-write-read

int64_t u0_write(int64_t clos, int64_t r, int64_t v) {
  ((int64_t*)r)[0] = v;
  return 0;
}

int64_t u1_read(int64_t clos, int64_t r) {
  return ((int64_t*)r)[0];
}
