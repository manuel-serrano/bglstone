#include <stdio.h>
#include <stdlib.h>

static int         seed= 0;

static int _rand()
{
  seed= (((seed * 25173) + 17431) & 4095);
  return seed;
}

static void _qsort( const int    lo,
                    const int    hi,
                    int * const  a )
{
  if (lo < hi)
  {
    int            i= lo;
    int            j= hi;
    const int      pivot= a[hi];

    while (i < j)
    {
      while ((i < hi) && (a[i] <= pivot))
        ++i;

      while ((lo < j) && (pivot <= a[j]))
        --j;

      if (i < j)
      {
        const int  temp= a[i];

        a[i]= a[j];
        a[j]= temp;
      }
    }

    {
      const int    temp= a[i];

      a[i]= a[hi];
      a[hi]= temp;
    }

    _qsort( lo, i-1, a );
    _qsort( i+1, hi, a );
  }
}

static int cmp( const int  i,
                const int  j )
{
  return (i - j);
}

static void _qsort2( const int    lo,
                     const int    hi,
                     int * const  a )
{
  if (lo < hi)
  {
    int            i= lo;
    int            j= hi;
    const int      pivot= a[hi];

    while (i < j)
    {
      while ((i < hi) && (cmp( a[i], pivot ) <= 0))
        ++i;

      while ((j > lo) && (cmp( a[j ], pivot ) >= 0))
        --j;

      if (i < j)
      {
        const int  temp= a[i];

        a[i]= a[j];
        a[j]= temp;
      }
    }

    {
      const int    temp= a[i];

      a[i]= a[hi];
      a[hi]= temp;
    }

    _qsort2( lo, i-1, a );
    _qsort2( i+1, hi, a );
  }
}

static const char * test_sort( const int  which,
                               const int  size )
{
  int * const      a= malloc( size * sizeof( int ) );
  int * const      check= calloc( size, sizeof( int ) );

  {
    int            i;

    for ( i= 0 ; i < size ; ++i )
    {
      const int    n= _rand();

      a[i]= n;
      check[n]= (check[n] + 1);
    }
  }

  if (which)
    _qsort( 0, size-1, a );
  else
    _qsort2( 0, size-1, a );

  check[a[0]]= check[a[0]]-1;

  {
    int            i;

    for ( i= 1 ; i < size ; ++i )
      if (a[i-1] > a[i] )
        fprintf( stderr, "test-sort: illegal sort -- %d", i );
      else
        check[a[i]]= (check[a[i]]-1);
  }

  {
    int            i;

    for ( i= 0 ; i < 4096 ; ++i )
      if (check[i] != 0 )
        fprintf( stderr, "test-sort: illegal sort -- %d", i );
  }

  return "ok";
}

static void testit()
{
  printf( "%s\n", test_sort( 1, 1300000 ) );
  printf( "%s\n", test_sort( 0, 1300000 ) );
}

static void doit()
{
  testit();
  testit();
  testit();
  testit();
}

int main( const int                   argc,
          const char * const * const  argv )
{
  doit();
  return 0;
}
