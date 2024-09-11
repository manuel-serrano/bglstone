#*=====================================================================*/
#*    serrano/prgm/project/bglstone/tools/Makefile                     */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Sun Aug  5 12:06:20 2001                          */
#*    Last change :  Wed Sep 11 08:36:42 2024 (serrano)                */
#*    Copyright   :  2001-24 Manuel Serrano                            */
#*    -------------------------------------------------------------    */
#*    The Makefile that is in charge of the bglstone tools             */
#*=====================================================================*/

TOOLS	= r2b barchart bchart b2b r2tex runit statinfo gnuplothistogram

all:
	for p in $(TOOLS); do \
	   (cd $$p; $(MAKE)); \
        done

clean:
	for p in $(TOOLS); do \
	   (cd $$p; $(MAKE) clean); \
        done

