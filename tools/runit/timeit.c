/*=====================================================================*/
/*    serrano/prgm/project/bglstone/tools/runit/timeit.c               */
/*    -------------------------------------------------------------    */
/*    Author      :  Manuel Serrano                                    */
/*    Creation    :  Tue Jan  2 13:54:15 2001                          */
/*    Last change :  Sat Feb 14 06:19:10 2004 (serrano)                */
/*    Copyright   :  2001-04 Manuel Serrano                            */
/*    -------------------------------------------------------------    */
/*    Measure how long last an execution.                              */
/*=====================================================================*/
#include <bigloo.h>
#include <sys/times.h>
#include <sys/time.h>
#include <unistd.h>

/*---------------------------------------------------------------------*/
/*    static double                                                    */
/*    timeit_cmd_once ...                                              */
/*---------------------------------------------------------------------*/
static double
timeit_cmd_once( char *cmd, long clktck ) {
   struct tms tmsstart, tmsend;
   clock_t start, end;
   long sum;

   if( clktck <= 0 )
      return -1.;

   if( (start = times( &tmsstart )) == -1 )
      return -2.;

   if( system( cmd ) != 0 ) {
      return -3.;
   }
   
   if( (end = times( &tmsend) ) == -1 )
      return -4.;

   sum = ((tmsend.tms_cutime - tmsstart.tms_cutime) +
	  (tmsend.tms_cstime - tmsstart.tms_cstime));
   
   return ((double)sum / (double)clktck);
}
   
/*---------------------------------------------------------------------*/
/*    static double                                                    */
/*    clockit_cmd_once ...                                             */
/*---------------------------------------------------------------------*/
static double
clockit_cmd_once( char *cmd ) {
   struct timeval start, stop;

   gettimeofday( &start, 0 );

   if( system( cmd ) != 0 ) {
      return -3.;
   }

   gettimeofday( &stop, 0 );

   return (double)(stop.tv_sec - start.tv_sec)
      + ((double)(stop.tv_usec - start.tv_usec) / 1000000.);
}
   
/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    timeit_cmd ...                                                   */
/*---------------------------------------------------------------------*/
double
timeit_cmd( char *cmd, int repetition, int verbose ) {
   long clktck = sysconf( _SC_CLK_TCK );
   
   if( clktck < 0 )
      return -1;
   else {
      double min = timeit_cmd_once( cmd, clktck );
      if( verbose >= 2 ) {
	 fprintf( stderr, "           run %d:  %g\n", repetition, min );
      } else {
	 if( verbose >= 1 )
	    fprintf( stderr, ".%d", repetition );
      }
	 
      repetition--;

      if( min < .0 )
	 return -1.;
   
      while( repetition ) {
	 double new = timeit_cmd_once( cmd, clktck );
	 if( new < .0 )
	    return new;

	 if( verbose >= 2 ) {
	    fprintf( stderr, "           run %d:  %g\n", repetition, new );
	 } else {
	    if( verbose >= 1 )
	       fprintf( stderr, ".%d", repetition );
	 }
	 
	 if( new < min ) min = new;
	 repetition--;
      }

      return min;
   }
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    clockit_cmd ...                                                  */
/*---------------------------------------------------------------------*/
double
clockit_cmd( char *cmd, int repetition, int verbose ) {
   double min = clockit_cmd_once( cmd );
   if( verbose >= 2 ) {
      fprintf( stderr, "           run %d:  %g\n", repetition, min );
   } else {
      if( verbose >= 1 )
	 fprintf( stderr, ".%d", repetition );
   }
	 
   repetition--;

   if( min < .0 )
      return -1.;
   
   while( repetition ) {
      double new = clockit_cmd_once( cmd );
      if( new < .0 )
	 return new;

      if( verbose >= 2 ) {
	 fprintf( stderr, "           run %d:  %g\n", repetition, new );
      } else {
	 if( verbose >= 1 )
	    fprintf( stderr, ".%d", repetition );
      }
	 
      if( new < min ) min = new;
      repetition--;
   }

   return min;
}

/*---------------------------------------------------------------------*/
/*    static double                                                    */
/*    timeit_thunk_once ...                                            */
/*---------------------------------------------------------------------*/
static double
timeit_thunk_once( obj_t fun, long clktck ) {
   struct tms tmsstart, tmsend;
   clock_t start, end;
   double sum;
   long arity = PROCEDURE_ARITY( fun );
   
   if( clktck <= 0 )
      return -1.;

   if( (start = times( &tmsstart )) == -1 )
      return -2.;

   if( arity < 0 ) {
      PROCEDURE_VA_ENTRY( fun )( fun );
   } else {
      PROCEDURE_ENTRY( fun )( fun );
   }
      
   if( (end = times( &tmsend) ) == -1 )
      return -4.;

   sum = ((tmsend.tms_cutime - tmsstart.tms_cutime) +
	  (tmsend.tms_cstime - tmsstart.tms_cstime));
   
   return (sum / (double)clktck);
}
   
/*---------------------------------------------------------------------*/
/*    static double                                                    */
/*    clockit_thunk_once ...                                           */
/*---------------------------------------------------------------------*/
static double
clockit_thunk_once( obj_t fun ) {
   struct timeval start, stop;
   long arity = PROCEDURE_ARITY( fun );

   gettimeofday( &start, 0 );

   if( arity < 0 ) {
      PROCEDURE_VA_ENTRY( fun )( fun );
   } else {
      PROCEDURE_ENTRY( fun )( fun );
   }
      
   gettimeofday( &stop, 0 );

   return (double)(stop.tv_sec - start.tv_sec)
      + ((double)(stop.tv_usec - start.tv_usec) / 1000000.);
}
   
/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    timeit_thunk ...                                                 */
/*---------------------------------------------------------------------*/
double
timeit_thunk( obj_t fun, int repetition, int verbose ) {
   long clktck = sysconf( _SC_CLK_TCK );
   
   if( clktck < 0 )
      return -1.;
   else {
      double min = timeit_thunk_once( fun, clktck );
      if( verbose >= 2 ) {
	 fprintf( stderr, "           run %d:  %g\n", repetition, min );
      } else {
	 if( verbose >= 1 )
	    fprintf( stderr, ".%d", repetition );
      }
	 
      repetition--;

      if( min < .0 )
	 return -1.;
   
      while( repetition ) {
	 double new = timeit_thunk_once( fun, clktck );
	 if( new < .0 )
	    return new;
	 
	 if( verbose >= 2 ) {
	    fprintf( stderr, "           run %d:  %g\n", repetition, new );
	 } else {
	    if( verbose >= 1 )
	       fprintf( stderr, ".%d", repetition );
	 }
	 
	 if( new < min ) min = new;
	 repetition--;
      }

      return min;
   }
}

/*---------------------------------------------------------------------*/
/*    double                                                           */
/*    clockit_thunk ...                                                */
/*---------------------------------------------------------------------*/
double
clockit_thunk( obj_t fun, int repetition, int verbose ) {
   double min = clockit_thunk_once( fun );
   if( verbose >= 2 ) {
      fprintf( stderr, "           run %d:  %g\n", repetition, min );
   } else {
      if( verbose >= 1 )
	 fprintf( stderr, ".%d", repetition );
   }
	 
   repetition--;

   if( min < .0 )
      return -1.;
   
   while( repetition ) {
      double new = clockit_thunk_once( fun );
      if( new < .0 )
	 return new;
	 
      if( verbose >= 2 ) {
	 fprintf( stderr, "           run %d:  %g\n", repetition, new );
      } else {
	 if( verbose >= 1 )
	    fprintf( stderr, ".%d", repetition );
      }
	 
      if( new < min ) min = new;
      repetition--;
   }

   return min;
}

