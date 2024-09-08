typedef int[] tab;

typedef struct {
   int x;
   int y;
} point;

typedef struct {
   int x;
   point pt;
} foo;

typedef point[] tab2;

tab2 glob;

int get_hash( point p ) {
   return 0;
}

point make_point( int x, int y ) {
   point p;

   return p;
}

int bar( point p ) {
   int i;
   int x;
   int y;
   int norm;

   x = p.x;
   y = p.y;

   i = get_hash( p );

   glob[ i ] = p;

   norm = x + y;
   return norm + x + y + i;
}
typedef struct {
   string key;
   tree   left;
   tree   right;
} tree;

string pp( tree tree ) {
  string output;
  void write( string s ) {
    output = string_append( output, s );
  }
  void show( int n, tree t ) {
    void indent( string s ) {
       int i;
       i = 1;
       while( i < n ) {
         write( " " );
         output = string_append( output, s );
       }
    }
    if( t == nil ) 
       indent( "." );
    else {
       indent( t.key );
       show( n + 1, t.left );
       show( n + 1, t.left );
    }
  }

  output = "";
  show( 0, tree );
  return output;
}

string string_append( string s, string t ) {
   return s;
}

int foo( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;
   
   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo1( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;
   
   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo2( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo3( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo4( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo5( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo6( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo7( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo8( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo9( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

string pp2( tree tree ) {
  string output;
  void write( string s ) {
    output = string_append( output, s );
  }
  void show( int n, tree t ) {
    void indent( string s ) {
       int i;
       i = 1;
       while( i < n ) {
         write( " " );
         output = string_append( output, s );
       }
    }
    if( t == nil ) 
       indent( "." );
    else {
       indent( t.key );
       show( n + 1, t.left );
       show( n + 1, t.left );
    }
  }

  output = "";
  show( 0, tree );
  return output;
}

int foo10( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo11( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo12( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo13( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo14( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo15( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo16( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo17( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo18( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo19( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo20( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo21( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo22( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo23( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo24( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo25( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo26( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo27( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo28( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo29( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

string ppp( tree tree ) {
  string output;
  void write( string s ) {
    output = string_append( output, s );
  }
  void show( int n, tree t ) {
    void indent( string s ) {
       int i;
       i = 1;
       while( i < n ) {
         write( " " );
         output = string_append( output, s );
       }
    }
    if( t == nil ) 
       indent( "." );
    else {
       indent( t.key );
       show( n + 1, t.left );
       show( n + 1, t.left );
       show( n + 1, t.left );
       show( n + 1, t.left );
       show( n + 1, t.left );
    }
  }

  output = "";
  show( 0, tree );
  return output;
}

int foo30( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo31( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo32( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo33( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo34( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo35( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

string pp4( tree tree ) {
  string output;
  void write( string s ) {
    output = string_append( output, s );
  }
  void show( int n, tree t ) {
    void indent( string s ) {
       int i;
       i = 1;
       while( i < n ) {
         write( " " );
         output = string_append( output, s );
       }
    }
    if( t == nil ) 
       indent( "." );
    else {
       indent( t.key );
    }
  }

  output = "";
  show( 0, tree );
  return output;
}

int foo36( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo37( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo38( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo39( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo409( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo41( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo42( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo43( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo4545( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo45( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo46( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo47( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo48( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo49( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo50( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo51( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo52( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo53( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo534( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo54( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo55( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo66( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo77( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo777( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo88( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo99( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo312( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   return res;
}

int foo312b( tab tab1, tab tab2, int len1, int len2 ) {
   int res;
   int i;

   res = 0;
   i = 0;

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 j = j + 1;
      }

      tab1[ i ] = aux;
      res = res + aux;
      i = i + 1;
   }

   while( i < len1 ) {
      int aux;
      int j;

      aux = 0;
      j = 0;

      while( j < len2 ) {
	 aux = tab1[ i ] + tab2[ j ];
	 aux = tab1[ i + j ] + tab2[ j -i ];
	 j = j + 1;
      }

      tab1[ i ] = aux + tab1[ i - 1 ];
      tab1[ i ] = aux + tab1[ i - 1 ];
      tab1[ i ] = aux + tab1[ i - 1 ];
      tab1[ i ] = aux + tab1[ i - 1 ];
      res = res + aux;
      i = i + 1;
   }

   return res;
}

tab make_tab() {
   tab res;

   return res;
}

{
   tab t1;
   tab t2;

   t1 = make_tab();
   t2 = make_tab();
   
   foo( t1, t2, 10, 10 );
}
