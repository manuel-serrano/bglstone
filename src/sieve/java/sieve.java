class CONS
{
  private int _car;
  private CONS _cdr;

  public CONS( int a, CONS d )
  {
    _car = a;
    _cdr = d;
  }

  public int car()
  {
    return _car;
  }

  public CONS cdr()
  {
    return _cdr;
  }

  public static CONS interval( int min, int max )
  {
    if ( min > max )
      return null;
    else
      return new CONS( min, interval( min + 1, max ) );
  }

  public static CONS append( CONS l1, CONS l2 )
  {
    if ( l1 == null )
      return l2;
    else
      return new CONS( l1.car(), append( ( CONS )l1.cdr(), l2 ) );
  }

  public static int length( CONS l )
  {
    int result = 0;

    while ( l != null )
    {
      ++result;
      l = l.cdr();
    }

    return result;
  }

  public static void display( CONS l )
  {
    System.out.print( "(" );

    while ( l != null )
    {
      System.out.print( l.car() + " " );
      l = ( CONS )l.cdr();
    }

    System.out.println( ")" );
  }
}

abstract class LAMBDA
{
  public abstract boolean body( int o );

  public CONS filter( CONS l )
  {
    if ( l == null )
      return l;
    else
    {
      int a = l.car();
      CONS b = l.cdr();

      if ( body( a ) )
        return new CONS( a, filter( b ) );
      else
        return filter( b );
    }
  }
}

class anonymous
  extends LAMBDA
{
  private int n;

  public anonymous( int _n )
  {
    n = _n;
  }

  public boolean body( int m )
  {
    return ( m % n ) != 0;
  }
}

  class run
{
  private int max;

  public run( int _m )
  {
    max = _m;
  }

  CONS remove_multiples_of_n( int n, CONS l )
  {
    return new anonymous( n ).filter( l );
  }

  CONS filter_again( CONS l )
  {
    if ( l == null )
      return l;
    else
    {
      int n = l.car();
      CONS r = l.cdr();

      if ( ( n * n ) > max )
        return l;
      else
        return new CONS( n, filter_again( remove_multiples_of_n( n, r ) ) );
    }
  }

  public CONS sieve()
  {
    return filter_again( CONS.interval( 2, max ) );
  }
}

public class sieve
{
  public static void main( String[] args )
  {
    int g = 0;
    for ( int i = 0; i < 5000; i++ )
      g += CONS.length( new run( 3000 ).sieve() );
    System.out.println( "doit: " + g );
    CONS.display( new run( 5000 ).sieve() );
  }
}