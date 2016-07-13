
// Change for fn_app x 1
int64_t u4_run_test(int64_t clos, int64_t i, int64_t acc) { 
  int64_t id1_clos = ((int64_t*)clos)[2];
  int64_t (*id1)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) id1_clos)[0];
  return id1(id1_clos, (1 + acc));
}

int64_t u6_run_test(int64_t clos, int64_t i, int64_t acc) { 
  int64_t id1_clos = ((int64_t*)clos)[2];
  int64_t id2_clos = ((int64_t*)clos)[3];
  int64_t (*id1)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) id1_clos)[0];
  int64_t (*id2)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) id2_clos)[0];
  int64_t tmp1 = id1(id1_clos, (1 + acc));
  return id2(id2_clos, tmp1);
}

int64_t u8_run_test(int64_t clos, int64_t i, int64_t acc) { 
  int64_t id1_clos = ((int64_t*)clos)[2];
  int64_t id2_clos = ((int64_t*)clos)[3];
  int64_t id3_clos = ((int64_t*)clos)[4];
  int64_t (*id1)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) id1_clos)[0];
  int64_t (*id2)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) id2_clos)[0];
  int64_t (*id3)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) id3_clos)[0];
  int64_t tmp1 = id1(id1_clos, (1 + acc));
  int64_t tmp2 = id2(id2_clos, tmp1);
  return id3(id3_clos, tmp2);
}

int64_t u10_run_test(int64_t clos, int64_t i, int64_t acc) { 
  int64_t id1_clos = ((int64_t*)clos)[2];
  int64_t id2_clos = ((int64_t*)clos)[3];
  int64_t id3_clos = ((int64_t*)clos)[4];
  int64_t id4_clos = ((int64_t*)clos)[5];
  int64_t (*id1)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) id1_clos)[0];
  int64_t (*id2)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) id2_clos)[0];
  int64_t (*id3)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) id3_clos)[0];
  int64_t (*id4)(int64_t, int64_t) = 
    (int64_t(*)(int64_t, int64_t)) ((int64_t*) id4_clos)[0];
  int64_t tmp1 = id1(id1_clos, (1 + acc));
  int64_t tmp2 = id2(id2_clos, tmp1);
  int64_t tmp3 = id3(id3_clos, tmp2);
  return id4(id4_clos, tmp3);
}

int64_t u3_run_test(int64_t clos, int64_t i, int64_t acc) {
  int64_t* b1 = ((int64_t**) clos)[2];
  int64_t r1 = *b1;
  *b1 = (1 + acc);
  return r1;
}

int64_t u4_run_test(int64_t clos, int64_t i, int64_t acc) {
  int64_t* b1 = ((int64_t**) clos)[2];
  int64_t* b2 = ((int64_t**) clos)[3];
  int64_t r1 = *b1;
  int64_t r2 = *b2;
  *b2 = (1 + acc);
  *b1 = r2;
  return r1;
}

int64_t u5_run_test(int64_t clos, int64_t i, int64_t acc) {
  int64_t* b1 = ((int64_t**) clos)[2];
  int64_t* b2 = ((int64_t**) clos)[3];
  int64_t* b3 = ((int64_t**) clos)[4];
  int64_t r1 = *b1;
  int64_t r2 = *b2;
  int64_t r3 = *b3;
  *b3 = (1 + acc);
  *b2 = r3
  *b1 = r2;
  return r1;
}

int64_t u6_run_test(int64_t clos, int64_t i, int64_t acc) {
  int64_t* b1 = ((int64_t**) clos)[2];
  int64_t* b2 = ((int64_t**) clos)[3];
  int64_t* b3 = ((int64_t**) clos)[4];
  int64_t* b4 = ((int64_t**) clos)[5];
  int64_t r1 = *b1;
  int64_t r2 = *b2;
  int64_t r3 = *b3;
  int64_t r4 = *b4;
  *b4 = (1 + acc);
  *b3 = r4;
  *b2 = r3
  *b1 = r2;
  return r1;
}

