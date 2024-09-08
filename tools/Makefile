#*=====================================================================*/
#*    serrano/prgm/project/bglstone/tools/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Aug  5 12:06:20 2001                          */
#*    Last change :  Tue Aug 21 15:30:05 2001 (serrano)                */
#*    Copyright   :  2001 Manuel Serrano                               */
#*    -------------------------------------------------------------    */
#*    The Makefile that is in charge of the bglstone tools             */
#*=====================================================================*/

TOOLS	= r2b barchart bchart b2b r2tex runit statinfo

all:
	for p in $(TOOLS); do \
	   (cd $$p; $(MAKE)); \
        done

clean:
	for p in $(TOOLS); do \
	   (cd $$p; $(MAKE) clean); \
        done

