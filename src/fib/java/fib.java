public class fib {
    static int fib( int x ) {
	if( x < 2 ) {
	    return 1;
	} else {
	    return fib( x - 1 ) + fib( x - 2 );
	}
    }

    public static void main( String[] args ) {
	int y = 0;

	for( int i = 0; i < 300; i++ ) {
	    y += fib( 30 );
	}

	System.out.println( fib( 30 ) + "  / " + (y/10) );
    }
}
