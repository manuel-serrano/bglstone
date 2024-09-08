class Pair
{
  private Object   _car;
  private Object   _cdr;

  public Pair( Object  a,
               Object  d )
  {
    _car= a;
    _cdr= d;
  }

  public Object car()
  {
    return _car;
  }

  public Object cdr()
  {
    return _cdr;
  }

  public static Pair count( int  from,
                            int  to )
  {
    if (from > to)
      return null;
    else
      return new Pair( new Integer( from ), count( from + 1, to ) );
  }

  public static Pair append( Pair  l1,
                             Pair  l2 )
  {
    if (l1 == null)
      return l2;
    else
      return new Pair( l1.car(), append( (Pair)l1.cdr(), l2 ) );
  }

  public static int length( Pair l )
  {
    int       result= 0;

    while (l != null)
    {
      ++result;
      l= (Pair)l.cdr();
    }

    return result;
  }

  public static void display( Pair l )
  {
    System.out.print( "(" );

    while (l != null)
    {
      System.out.print( l.car() + " " );
      l= (Pair)l.cdr();
    }

    System.out.println( ")" );
  }
}

abstract class Lambda
{
  public abstract Object body( Object  o );

  public Pair mmap( Pair  l )
  {
    if (l == null)
      return null;
    else
      return new Pair( body( l.car() ), mmap( (Pair)l.cdr() ) );
  }

  public Pair filter( Pair  l )
  {
    if (l == null)
      return l;
    else if (((Boolean)body( l.car() )).booleanValue())
      return new Pair( l.car(), filter( (Pair)l.cdr() ) );
    else
      return filter( (Pair)l.cdr() );
  }

  public Pair concmap( Pair  l )
  {
    if (l == null)
      return null;
    else
      return Pair.append( (Pair)body( l.car() ),
                          concmap( (Pair)l.cdr() ) );
  }
}

class nsoln
{
  private static class ok extends Lambda
  {
    private static boolean safe( int   d,
                                 int   x,
                                 Pair  l )
    {
      if (l == null)
        return true;
      else
      {
        int        q= ((Integer)(l.car())).intValue();

        return (   (x != q)
                && (x != (q + d))
                && (x != (q - d))
                && safe( d + 1, x, (Pair)l.cdr() ));
      }
    }

    public Object body( Object  l )
    {
      if (l == null)
        return Boolean.TRUE;
      else
        return new Boolean( safe( 1,
                                  ((Integer)(((Pair)l).car())).intValue(),
                                  (Pair)((Pair)l).cdr()) );
    }
  }

  private static class TestCol extends Lambda
  {
    private static class Anonymous extends Lambda
    {
      private Object    b;

      public Anonymous( Object _b )
      {
        b= _b;
      }

      public Object body( Object  q )
      {
        return new Pair( q, b );
      }
    }

    private Pair        pos_l;
    private Lambda      ok;

    public TestCol( Pair    _pos_l,
                    Lambda  _ok )
    {
      pos_l= _pos_l;
      ok= _ok;
    }

    public Object body( Object  b )
    {
      return ok.filter( (new Anonymous( b )).mmap( pos_l ) );
    }
  }

  private Pair          pos_l;
  private TestCol       testcol;
  private int           nq;

  public nsoln( int  _nq )
  {
    nq= _nq;
    pos_l=  Pair.count( 1, nq );
    testcol= new TestCol( pos_l, new ok() );
  }

  private Pair gen( int  n )
  {
    if (n == 0)
      return new Pair( null, null );
    else
      return testcol.concmap( gen( n - 1 ) );
  }

  public int run()
  {
    return Pair.length( gen( nq ) );
  }
}

class nsoln_a
{
  private static class ok extends Lambda
  {
    private static boolean safe( int   x,
                                 int   d,
                                 Pair  l )
    {
      if (l == null)
        return true;
      else
      {
        int        q= ((Integer)(l.car())).intValue();

        return (   (x != q)
                && (x != (q + d))
                && (x != (q - d))
                && safe( x, d + 1, (Pair)l.cdr() ));
      }
    }

    public Object body( Object  l )
    {
      if (l == null)
        return Boolean.TRUE;
      else
        return new Boolean( safe( ((Integer)((Pair)l).car()).intValue(),
                                  1,
                                  (Pair)((Pair)l).cdr() ) );
    }
  }

  private static class TestCol extends Lambda
  {
    private static class Anonymous extends Lambda
    {
      private Object    b;

      public Anonymous( Object _b )
      {
        b= _b;
      }

      public Object body( Object  q )
      {
        return new Pair( q, b );
      }
    }

    private Lambda      ok;
    private int         nq;

    public TestCol( Lambda  _ok,
                    int     _nq )
    {
      ok= _ok;
      nq= _nq;
    }

    public Object body( Object  b )
    {
      return ok.filter( (new Anonymous( b )).mmap( Pair.count( 1, nq ) ) );
    }
  }

  private TestCol       testcol;
  private int           nq;

  public nsoln_a( int  _nq )
  {
    nq= _nq;
    testcol= new TestCol( new ok(), nq );
  }

  private Pair gen( int  n )
  {
    if (n == 0)
      return new Pair( null, null );
    else
      return testcol.concmap( gen( n - 1 ) );
  }

  public int run()
  {
    return Pair.length( gen( nq ) );
  }
}

class queens
{
  public static void main( String[]  args )
  {
    System.out.println( (new nsoln( 10 )).run() );
    System.out.println( (new nsoln_a( 10 )).run() );

    int       res= 0;

    for ( int i= 0 ; i < 60 ; ++i )
      res+= ((new nsoln( 10 )).run() - (new nsoln_a( 10 )).run());

    System.out.println( res );
  }
}
