#*=====================================================================*/
#*    serrano/prgm/project/bglstone/tools/r2b/Makefile                 */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Aug  5 12:08:21 2001                          */
#*    Last change :  Sun Aug  5 14:50:58 2001 (serrano)                */
#*    Copyright   :  2001 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile to compile r2b                                      */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Configuration                                                    */
#*---------------------------------------------------------------------*/
include ../../Makefile.config

#*---------------------------------------------------------------------*/
#*    Parameters and variables                                         */
#*---------------------------------------------------------------------*/
TOOL=$(TOOLSBIN)/r2b
OBJECTS=r2b.o

#*---------------------------------------------------------------------*/
#*    r2b                                                              */
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

