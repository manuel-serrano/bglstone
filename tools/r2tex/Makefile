#*=====================================================================*/
#*    serrano/prgm/project/bglstone/tools/r2tex/Makefile               */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Aug  5 12:08:21 2001                          */
#*    Last change :  Sun Aug 12 14:16:49 2001 (serrano)                */
#*    Copyright   :  2001 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile to compile r2tex                                    */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Parameters and variables                                         */
#*---------------------------------------------------------------------*/
TOOL=$(TOOLSBIN)/r2tex
OBJECTS=r2tex.o

#*---------------------------------------------------------------------*/
#*    r2tex                                                            */
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
	$(BGLTOOLS) $(BGLTOOLSFLAGS) $*.scm -o $*.o -c

