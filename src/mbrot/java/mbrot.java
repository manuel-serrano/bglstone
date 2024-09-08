public class mbrot
{
  static double    _r= -1.0;
  static double    _i= -0.5;
  static int        _n= 75;
  static double    _step= 0.005;

  private static int count( double  cr,
                            double  ci )
  {
    int            c= 0;
    double         zr= cr, zi= ci;
    int            max_count= 64;
    double         radius_squared= 16.0;

    while (c != max_count)
    {
      double       zr_squared= zr * zr;
      double       zi_squared= zi * zi;

      if (radius_squared < (zr_squared + zi_squared))
        return c;
      else
      {
        ++c;
        zi= (2.0 * (zr * zi)) + ci;
        zr= (zr_squared - zi_squared) + cr;
      }
    }

    return c;
  }

  private static int mbrot( double   r,
                            double   i,
                            int      n,
                            double   step,
                            boolean  p )
  {
    int            y= n;
    double         ci= i;

    while (0 < y)
    {
      int          x= n;
      double       cr= r;

      while (0 < x)
      {
        int        c= count( cr, ci );

        if (p)
          System.out.print( (char)(c + 32) );
        --x;
        cr+= step;
      }

      --y;
      ci+= step;

      if (p)
        System.out.println();
    }

    return y;
  }

  private static void run( int  num )
  {
    int            res= mbrot( _r, _i, _n, _step, (num == 1) );

    while (1 < num)
    {
      --num;
      res= mbrot( _r, _i, _n, _step, (num == 2) );
    }

    System.out.println( "done: " + res );
  }

  private static void do_bench( int  num )
  {
    if (0 < num)
      run( num );
  }

  public static void main( String[]  args )
  {
/*     if (args.length == 3)                                           */
/*     {                                                               */
/*       System.out.println( "setting new values" );                   */
/*       _r = 1.1;                                                     */
/*       _i = -0.4;                                                    */
/*       _n = 50;                                                      */
/*       _step = 0.001;                                                */
/*     }                                                               */

    do_bench( 10000 );
  }
}
