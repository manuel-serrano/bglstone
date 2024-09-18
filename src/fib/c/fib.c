#include <stdio.h>
#include <stdlib.h>

static long res = 0;

static long fib(long x) {
   if (x < 2) {
      return 1;
   } else {
      return fib(x - 1) + fib(x - 2);
   }
}

static void doit(int num) {
   while (num > 0) {
      res = fib(35);
      num--;
   }
}

int main(int argc, char *argv[]) {
   doit(900);
   fprintf(stderr, "%ld\n", res);
   return res != 14930352;
}
