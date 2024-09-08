#include <stdlib.h>
#include <stdio.h>

static int              nombre_de_coups= 0;
static const int        nombre_de_pierres= 28;
static const int        une_pierre= 1;
static const int        une_case_vide= 0;
static int *            jeu;

static void baguen( const int  n );

static void error( const char * const  f,
                   const char * const  m,
                   const int           a )
{
  printf( "%d error %s : %s : %d", nombre_de_coups, f, m, a );
  exit( -1 );
}

static void init_jeu()
{
  jeu= calloc( nombre_de_pierres, sizeof( int ) );
  nombre_de_coups= 0;
  {
    int            i;

    for ( i= 0 ; i < nombre_de_pierres ; ++i )
      jeu[i]= une_pierre;
  }
}

static void enleve_la_pierre( const int  n )
{
	if (jeu[n-1] == une_pierre)
    jeu[n-1] = une_case_vide;
  else
    error( "bague", "cannot remove a stone from an empty slot", n );
}

static void pose_la_pierre( const int  n )
{
  if (jeu[n-1] == une_case_vide)
	  jeu[n-1]= une_pierre;
	  else
  error( "bague", "cannot lay a stone on a non empty slot", n );
}

static int ok( int        b,
               int        i,
               const int  n )
{
  while (i <= (n-3))
  {
	  b&= (jeu[i] == une_case_vide);
	  ++i;
  }
  return b;
}

static int autorise_mouvement( const int  n )
{
  switch (n)
  {
    case 1:   return 1;
    case 2:   return (jeu[0] == une_pierre);
    default:  return (   (jeu[n - 2] == une_pierre)
                      && ok( 1, 0, n ));
  }
}

static void enleve_pierre( const int  n )
{
  ++nombre_de_coups;
  if (autorise_mouvement( n ))
    enleve_la_pierre( n );
  else
    error( "bague", "forbidden action", n );
}

static void pose_pierre( const int  n )
{
  ++nombre_de_coups;
  if (autorise_mouvement( n ))
    pose_la_pierre( n );
  else
    error( "bague", "forbidden action", n );
}

static void repose( const int  n )
{
  switch (n)
  {
    case 1:   pose_pierre( 1 );
              return;
    case 2:   pose_pierre( 1 );
              pose_pierre( 2 );
              return;
    default:  repose( n-1 );
              baguen( n-2 );
              pose_pierre( n );
	            repose( n-2 );
	            return;
	}
}

static void baguen( const int  n )
{
  switch (n)
  {
	  case 1:   enleve_pierre( 1 );
	            return;
	  case 2:   enleve_pierre( 2 );
              enleve_pierre( 1 );
              return;
    default:  baguen( n-2 );
              enleve_pierre( n );
              repose( n-2 );
	            baguen( n-1 );
	            return;
  }
}

int main( const int                   argc,
          const char * const * const  argv )
{
  init_jeu();
  baguen( nombre_de_pierres );

  {
    const int      result= (nombre_de_coups == 178956970);

    printf( "%d", result );
    return (result ? 0 : 1);
  }
}
