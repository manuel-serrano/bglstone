#*=====================================================================*/
#*    .../prgm/project/bglstone/tools/gnuplothistogram/Makefile        */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Aug  5 12:08:21 2001                          */
#*    Last change :  Wed Sep 11 11:54:38 2024 (serrano)                */
#*    Copyright   :  2001-24 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile to compile barchart                                 */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Parameters and variables                                         */
#*---------------------------------------------------------------------*/
TOOL=$(TOOLSBIN)/gnuplothistogram
OBJECTS=gnuplothistogram.o

#*---------------------------------------------------------------------*/
#*    gnuplothistogram                                                 */
#*---------------------------------------------------------------------*/
$(TOOL): $(OBJECTS)	
	$(BGLTOOLS) $(BGLTOOLSFLAGS) $(OBJECTS) -o $(TOOL)

#*---------------------------------------------------------------------*/
#*    clean                                                            */
#*---------------------------------------------------------------------*/
clean:
	/bin/rm -f $(OBJECTS)
	/bin/rm -f $(TOOL)

#*---------------------------------------------------------------------*/
#*    SUFFIXES                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:
.SUFFIXES: .scm .o

#*---------------------------------------------------------------------*/
#*    .scm.o                                                           */
#*---------------------------------------------------------------------*/
.scm.o:
	$(BGLTOOLS) $(EFLAGS) $(BGLTOOLSFLAGS) $*.scm -o $*.o -c

#*---------------------------------------------------------------------*/
#*    Dependences                                                      */
#*---------------------------------------------------------------------*/
gnuplothistogram.o: gnuplothistogram-template.plot
