///////////////////////////////////////////////////////////////////////////////
/// File:         fft.c
/// Description:  FFT benchmark from the Gabriel tests.
/// Author:       Harry Barrow
/// Created:      8-Apr-85
/// Modified:     6-May-85 09:29:22 (Bob Shaw)
///               11-Aug-87 (Will Clinger)
///               16-Nov-94 (Qobi)
///               31-Mar-98 (Qobi)
/// Language:     C
/// Status:       Public Domain
///////////////////////////////////////////////////////////////////////////////
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// FFT -- This is an FFT benchmark written by Harry Barrow.
// It tests a variety of floating point operations,
// including array references.

#define ARRAY_SIZE 1025

static double pi;

static double _re_[ARRAY_SIZE];

static double _im_[ARRAY_SIZE];

static void fft( double  areal[ARRAY_SIZE],
                 double  aimag[ARRAY_SIZE] )
{
  double *    const   ar= areal;
  double *    const   ai= aimag;
  int         i;
  int         j= 0;
  int         k= 0;
  int         m= 0;                    // compute m = log(n)
  int         l;
  int         n= ARRAY_SIZE-1;
  int         le= 0;
  int         le1= 0;
  int         ip= 0;
  int         nv2= n/2;
  double      ur= 0.0;
  double      ui= 0.0;
  double      wr= 0.0;
  double      wi= 0.0;
  double      tr= 0.0;
  double      ti= 0.0;

  i= 1;
  while (i < n)
  {
    ++m;
    i*= 2;
  }

  {
    int       i= m;
    int       p= 1;

    while (i != 0)
    {
      --i;
      p*= 2;
    }

    if (n != p)
      puts( "array size not a power of two." );
  }

  // interchange elements in bit-reversed order
  j= 1;
  i= 1;

  do
  {
    if (i < j)
    {
      tr= ar[j];
      ti= ai[j];
      ar[j]= ar[i];
      ai[j]= ai[i];
      ar[i]= tr;
      ai[i]= ti;
    }
    k= nv2;
    while (k < j)
    { 
      j-= k;
      k/= 2;
    }
    j+= k;
    ++i;
  } while (i < n);

  // loop thru stages (syntax converted from old MACLISP style \bs)
  for ( l= 1 ; l <= m ; ++l )
  {
    {
      int     i= l;
      int     p= 1;

      while (i != 0)
      {
        --i;
        p*= 2;
      }

      le= p;
    }

    le1= le / 2;
    ur= 1.0;
    ui= 0.0;
    wr= cos( pi / le1 );
    wi= sin( pi / le1 );

    // loop thru butterflies
    for ( j= 1 ; j <= le1 ; ++j )
    {
      // do a butterfly
      for ( i= j ; i <= n ; i+= le )
      {
        ip= i + le1;
        tr= (ar[ip] * ur) - (ai[ip] * ui);
        ti= (ar[ip] * ui) + (ai[ip] * ur);
        ar[ip]= ar[i] - tr;
        ai[ip]= ai[i] - ti;
        ar[i]+= tr;
        ai[i]+= ti;
        tr= (ur * wr) - (ui * wi);
        ti= (ur * wi) + (ui * wr);
        ur= tr;
        ui= ti;
      }
    }
  }
}

fft_bench() {
   int i = 0;

   for( i = 0; i < 10; i++ ) {
      fft( _re_, _im_ );
   }
}

// note: The MAKE-VECTOR is not done multiple times.
int main( const int argc, const char * const * const  argv )
{
  int num = 2000;
  int i;

  pi= atan2( 0, -1 );

  for ( i= 0 ; i < 1025 ; ++i )
    _re_[i]= _im_[0]= 0.0;

  if ( argc > 2 )
    num= atoi( argv[1] );

  for ( i= 0 ; i < num ; ++i )
     fft_bench();
     
  return 0;
}
