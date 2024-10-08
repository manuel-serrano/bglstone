#*=====================================================================*/
#*    serrano/diffusion/article/bjvm/bench/makes/Makefile.java         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 17 14:34:35 2001                          */
#*    Last change :  Sun Jan 21 17:39:23 2001 (serrano)                */
#*    Copyright   :  2001 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The standard Makefile to compile java benchmarks.                */
#*=====================================================================*/
#*---------------------------------------------------------------------*/
#*    The compile and flags                                            */
#*---------------------------------------------------------------------*/
JAVA=javac
JFLAGS=-O

#*---------------------------------------------------------------------*/
#*    Basename and objects                                             */
#*---------------------------------------------------------------------*/
BENCH=
OBJECTS=$(BENCH).class

compile: java.bat

#*---------------------------------------------------------------------*/
#*    Jvm entry                                                        */
#*---------------------------------------------------------------------*/
java.bat: $(OBJECTS)
	echo "#!/bin/sh" > $@
	echo "java -noverify $(BENCH)" >> $@
	chmod a+rx $@

#*---------------------------------------------------------------------*/
#*    clean:                                                           */
#*---------------------------------------------------------------------*/
clean:
	/bin/rm -f $(OBJECTS)
	/bin/rm -f java.bat
	/bin/rm -f *.class
	cleanup

#*---------------------------------------------------------------------*/
#*    Implicit rules                                                   */
#*---------------------------------------------------------------------*/
%.class: %.java
	$(JAVA) $(JFLAGS) $*.java

