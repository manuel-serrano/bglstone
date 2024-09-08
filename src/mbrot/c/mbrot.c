#include <stdio.h>
#include <stdlib.h>

static double _r = -1.0;
static double _i = -0.5;
static long _n = 75;
static double _step = 0.005;

static long count( double  cr, double  ci ) {
   long c = 0;
   double zr = cr, zi = ci;
   long max_count = 64;
   double radius_squared = 16.0;

   while (c != max_count) {
      double zr_squared = zr * zr;
      double zi_squared = zi * zi;

      if (radius_squared < (zr_squared + zi_squared))
	 return c;
      else {
	 ++c;
	 zi = (2.0 * (zr * zi)) + ci;
	 zr = (zr_squared - zi_squared) + cr;
      }
   }

   return c;
}

static long mbrot( double r, double i, long n, double step, int p ) {
   long y = n;
   double ci = i;

   while (0 < y) {
      long x = n;
      double cr = r;

      while (0 < x) {
        long c = count( cr, ci );

        if (p)
	   printf( "%c", (char)(c + 32) );
        --x;
        cr += step;
      }

      --y;
      ci += step;

      if (p)
	 puts( "" );
    }

    return y;
  }

static void run( long  num ) {
   long res = mbrot( _r, _i, _n, _step, (num == 1) );

   while (1 < num) {
      --num;
      res = mbrot( _r, _i, _n, _step, (num == 2) );
    }

   printf( "done: %d\n" + res );
}

int main( int argc, char *args[] ) {
   run( 10000 );
   return 0;
}
