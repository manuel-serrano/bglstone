#*=====================================================================*/
#*    serrano/prgm/project/bglstone/tools/bchart/Makefile              */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Aug  5 12:08:21 2001                          */
#*    Last change :  Fri Aug 17 09:42:11 2001 (serrano)                */
#*    Copyright   :  2001 Manuel Serrano                               */
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
TOOL=$(TOOLSBIN)/bchart
OBJECTS=bchart.o

#*---------------------------------------------------------------------*/
#*    bchart                                                           */
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

#*---------------------------------------------------------------------*/
#*    Dependences                                                      */
#*---------------------------------------------------------------------*/
bchart.scm: bchart.ps
