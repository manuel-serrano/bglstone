public class quicksort {
    static int seed = 0;
    static int rand() {
	seed = (((seed * 25173) + 17431) & 4095);
	return seed;
    }

    static void qsort( int lo, int hi, int[] a ) {
	if( lo < hi ) {
	    int i = lo;
	    int j = hi;
	    int pivot = a[ hi ];
	    while( i < j ) {
		while( (i < hi) && (a[ i ] <= pivot) ) {
		    i = i + 1;
		}
		while( (j > lo) && (a[ j ] >= pivot) ) {
		    j = j - 1;
		}
		if( i < j ) {
		    int temp = a[ i ];
		    a[ i ] = a[ j ];
		    a[ j ] = temp;
		}
	    }
	    int temp = a[ i ];
	    a[ i ] = a[ hi ];
	    a[ hi ] = temp;
	    qsort( lo, i - 1, a );
	    qsort( i + 1, hi, a );
	}
    }

    static int cmp( int i, int j ) {
	return i - j;
    }

    static void qsort2( int lo, int hi, int[] a ) {
	if( lo < hi ) {
	    int i = lo;
	    int j = hi;
	    int pivot = a[ hi ];
	    
	    while( i < j ) {
		while( (i < hi) && (cmp( a[ i ], pivot) <= 0) ) {
		    i = i + 1;
		}
		while( (j > lo) && (cmp( a[ j ], pivot) >= 0) ) {
		    j = j - 1;
		}
		if( i < j ) {
		    int temp = a[ i ];
		    a[ i ] = a[ j ];
		    a[ j ] = temp;
		}
	    }
	    int temp = a[ i ];
	    a[ i ] = a[ hi ];
	    a[ hi ] = temp;
	    qsort2( lo, i - 1, a );
	    qsort2( i + 1, hi, a );
	}
    }

    static String test_sort( boolean which, int size ) {
	int[] a = new int[ size ];
	int[] check = new int[ 4096 ];

	for( int i = 0; i < size; i++ ) {
	    a[ i ] = 0;
	}
	for( int i = 0; i < 4096; i++ ) {
	    check[ i ] = 0;
	}
	for( int i = 0; i < size; i++ ) {
	    int n = rand();
	    a[ i ] = n;
	    check[ n ] = check[ n ] + 1;
	}
	if( which ) {
	    qsort( 0, size - 1, a );
	} else {
	    qsort2( 0, size - 1, a );
	}
	check[ a[ 0 ] ] = check[ a[ 0 ] ] - 1;
	for( int i = 1; i < size; i++ ) {
	    if( a[ i - 1 ] > a[ i ] ) {
		System.err.println( "test-sort: illegal sort -- " + i );
	    } else {
		check[ a[ i ] ] = check[ a[ i ] ] - 1;
	    }
	}
	for( int i = 0; i < 4096; i++ ) {
	    if( check[ i ] != 0 ) {
		System.err.println( "test-sort: illegal sort -- " + i );
	    }
	}
	return "ok";
    }

    static void testit() {
	System.out.println( test_sort( true, 1300000 ) );
	System.out.println( test_sort( false, 1300000 ) );
    }

    static void doit() {
	testit();
	testit();
	testit();
	testit();
    }

    public static void main( String[] args ) {
	doit();
    }
}
