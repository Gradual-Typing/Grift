#define ITERATIONS 10000

int __attribute__((noinline)) fun (int n) {
  return n + 1;
}

int main () {
  int ret = 0;
  for(int i = ITERATIONS; i > 0; i--) ret = fun(ret);
  return ret;
}
