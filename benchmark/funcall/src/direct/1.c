#define ITERATIONS 1

int __attribute__((noinline)) fun (int n) {
  return n + 1;
}

int main () {
  int ret = 0;
  for(long i = ITERATIONS; i > 0; i--) ret = fun(ret);
  return ret;
}
   
