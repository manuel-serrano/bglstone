#*=====================================================================*/
#*    serrano/prgm/project/bglstone/src/bigloo/bigloo/Makefile         */
#*    -------------------------------------------------------------    */
#*    Author      :  Manuel Serrano                                    */
#*    Creation    :  Wed Jan 14 08:29:03 1998                          */
#*    Last change :  Mon Mar 15 16:18:51 2004 (serrano)                */
#*    Copyright   :  1998-2004 Manuel Serrano, see LICENSE file        */
#*    -------------------------------------------------------------    */
#*    The Makefile to build the compiler part of Bigloo (bootstrap).   */
#*    This Makefile has to be processed by Gnu make.                   */
#*=====================================================================*/

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/
include ../Makefile.config

#*---------------------------------------------------------------------*/
#*    Compilers, Tools and Destinations                                */
#*---------------------------------------------------------------------*/
# the executable used to compile (used to bootstrap)
BIGLOO     	= $(BOOTBINDIR)/bigloo
# the produced executable
BIGLOO_DEST	= $(BOOTBINDIR)/bigloo
# the .afile
AFILE           = .afile
# the .jfile
JFILE           = .jfile
# the shell to be used
SHELL           = /bin/sh
# another produced executable
NEW_SUFFIX	= new
# another target.
DEFAULT		= plain
# These variables control how we are booting (from Scheme or C)
SCMOBJSUFFIX	= %.o
SCMCLASSSUFFIX	= %.class
DOTNETOBJSUFFIX	= %.obj
COBJSUFFIX	= 
CSRCSUFFIX	= 

#*---------------------------------------------------------------------*/
#*    Bigloo compilation flags                                         */
#*---------------------------------------------------------------------*/
LIBDIRFLAGS	= -lib-dir $(FILDIR)
# Commmon flags
COMMONFLAGS	= -q \
                  -afile $(AFILE) \
                  -fsharing \
                  -fno-reflection \
                  -cc $(CC) \
                  $(LIBDIRFLAGS) \
                  -unsafev \
                  -no-hello
# Optimized _and_ debugged compilation
OGBFLAGS        = $(COMMONFLAGS) -Wall -g -cg -O2 +rm
# Bootstrap flag
BFLAGS		= $(OGBFLAGS)       
EFLAGS		=
# Distribution flag
BDISTFLAGS	= $(COMMONFLAGS) -O2 -rm -unsafe -s
# Distribution Jvm flag
BJVMFLAGS	= $(BDISTFLAGS) -lib-dir $(BOOTLIBDIR)
# Distribution Dotnet flag
BDOTNETFLAGS	= $(BDISTFLAGS) -lib-dir $(BOOTLIBDIR)
# Link flag
LDBOOTFLAGS     = -static-bigloo -ldopt $(CSTRIPFLAGS)
# C compiler flag
CADDFLAGS	= -I$(BOOTLIBDIR)

#*---------------------------------------------------------------------*/
#*    Afile, Jfile and Bdepend tools                                   */
#*---------------------------------------------------------------------*/
AFILE_EXE	= $(BOOTBINDIR)/afile
JFILE_EXE	= $(BOOTBINDIR)/jfile
BTAGS_EXE	= $(BOOTBINDIR)/btags
DEPEND_EXE	= $(BOOTBINDIR)/bdepend

#*---------------------------------------------------------------------*/
#*    License                                                          */
#*---------------------------------------------------------------------*/
LICENSE		= ../LICENSE

#*---------------------------------------------------------------------*/
#*    The compiler source files ...                                    */
#*---------------------------------------------------------------------*/
#*--- init ------------------------------------------------------------*/
INIT_FILE_NOMAIN= parse-args setrc extend
INIT_FILE	= $(INIT_FILE_NOMAIN) main

INIT_OBJ_NOMAIN	= $(INIT_FILE_NOMAIN:%=Init/%.o)
INIT_OBJ	= $(INIT_FILE:%=Init/%.o)

#*--- tools -----------------------------------------------------------*/
TOOLS_FILE	= args location progn trace speek error shape misc date \
                  dsssl license

TOOLS_OBJ	= $(TOOLS_FILE:%=Tools/%.o)

#*--- engine ----------------------------------------------------------*/
ENGINE_FILE	= compiler engine param interp link signals pass \
                  configure

ENGINE_OBJ	= $(ENGINE_FILE:%=Engine/%.o)

#*--- heap ------------------------------------------------------------*/
HEAP_FILE	= restore make

HEAP_OBJ	= $(HEAP_FILE:%=Heap/%.o)

#*--- write -----------------------------------------------------------*/
WRITE_FILE	= version expanded scheme ast

WRITE_OBJ	= $(WRITE_FILE:%=Write/%.o)

#*--- read ------------------------------------------------------------*/
READ_FILE	= access include src inline load reader jvm

READ_OBJ	= $(READ_FILE:%=Read/%.o)

#*--- module ----------------------------------------------------------*/
MODULE_FILE	= foreign impuse main prototype with class include \
                  module statexp eval load pragma type library checksum \
                  option alibrary java

MODULE_OBJ	= $(MODULE_FILE:%=Module/%.o)

#*--- ast -------------------------------------------------------------*/
AST_FILE	= build venv glo-decl node unit dump glo-def pragma \
                  var find-gdefs ident remove local sexp labels let \
                  exit substitute alphatize occur app apply lvtype \
                  hrtype shrinkify check-init private check-sharing

AST_OBJ		= $(AST_FILE:%=Ast/%.o)

#*--- cc --------------------------------------------------------------*/
CC_FILE		= exec indent cc ld

CC_OBJ		= $(CC_FILE:%=Cc/%.o)

#*--- foreign ---------------------------------------------------------*/
FOREIGN_FILE	= access cenum cptr ctype calias cfun cstruct copaque \
                  library jtype
 
FOREIGN_OBJ	= $(FOREIGN_FILE:%=Foreign/%.o)

#*--- Type ------------------------------------------------------------*/
TYPE_FILE	= cache tenv type coercion tools typeof pptype

TYPE_OBJ	= $(TYPE_FILE:%=Type/%.o)

#*--- expand ----------------------------------------------------------*/
EXPAND_FILE	= eps assert case define expander garith iarith farith \
                  if initial lambda let map struct exit object \
                  srfi-0

EXPAND_OBJ	= $(EXPAND_FILE:%=Expand/%.o)

#*--- inline ----------------------------------------------------------*/
INLINE_FILE	= walk inline app size simple recursion variant loop


INLINE_OBJ	= $(INLINE_FILE:%=Inline/%.o)

#*--- coerce ----------------------------------------------------------*/
COERCE_FILE	= walk pproto coerce app funcall apply convert

COERCE_OBJ	= $(COERCE_FILE:%=Coerce/%.o)

#*--- globalize -------------------------------------------------------*/
GLOBALIZE_FILE	= walk ginfo escape gn node free kapture integration \
                  new-body loc2glo gloclo clocto globalize

GLOBALIZE_OBJ	= $(GLOBALIZE_FILE:%=Globalize/%.o)

#*--- cnst ------------------------------------------------------------*/
CNST_FILE	= walk cache alloc node initialize

CNST_OBJ	= $(CNST_FILE:%=Cnst/%.o)

#*--- cfa -------------------------------------------------------------*/
CFA_FILE	= app cinfo iterate set approx collect loose show walk \
                  setup vector cfa funcall procedure struct type \
                  closure tvector box specialize cinfo2 cinfo3 \
                  arithmetic ltype

CFA_OBJ		= $(CFA_FILE:%=Cfa/%.o)

#*--- Integrate -------------------------------------------------------*/
INTEGRATE_FILE	= walk definition iinfo a kk u g ctn cto kaptured \
                  let-fun loc2glo node free

INTEGRATE_OBJ	= $(INTEGRATE_FILE:%=Integrate/%.o)

#*--- Tailc -------------------------------------------------------*/
TAILC_FILE	= walk

TAILC_OBJ	= $(TAILC_FILE:%=Tailc/%.o)

#*--- Cgen ------------------------------------------------------------*/
CGEN_FILE	= walk cop emit-cop cgen capp

CGEN_OBJ	= $(CGEN_FILE:%=Cgen/%.o)

#*--- Jas ------------------------------------------------------------*/
JAS_FILE        = lib classfile opcode peep wide labels stack produce as

JAS_OBJ		= $(JAS_FILE:%=Jas/%.o)

#*--- Jvm ------------------------------------------------------------*/
JVM_FILE	= extern env instr inline peephole lisp prelude compress \
                  init constr closure \
                  compiler tail effect predicate expr \
                  compile jdump jld vmemq

JVM_OBJ		= $(JVM_FILE:%=Jvm/%.o)

#*--- DotNet-----------------------------------------------------------*/
DOTNET_FILE	= extern env instr inline peephole lisp prelude compress \
                  init constr closure \
                  compiler tail effect predicate expr \
                  compile as dotnet vmemq dnld

DOTNET_OBJ	= $(DOTNET_FILE:%=DotNet/%.o)

#*--- BackEnd ---------------------------------------------------------*/
BACKEND_FILE    = backend walk c jvm dotnet lib cplib \
                  c-main c-proto c-emit c-init

BACKEND_OBJ	= $(BACKEND_FILE:%=BackEnd/%.o)

#*--- SawMill ---------------------------------------------------------*/
SAW_FILE        = lib defs rtl node2rtl collapse remove InlineReturn \
		blockorder gotos cast expr woodcutter \
		procedures elsewhere

SAW_OBJ		= $(SAW_FILE:%=SawMill/%.o)

#*--- SawC ------------------------------------------------------------*/
SAW_C_FILE      = compile code

SAW_C_OBJ	= $(SAW_C_FILE:%=SawC/%.o)

#*--- SawMsil ---------------------------------------------------------*/
SAW_MSIL_FILE	= compile type code inline maxstack funcall names out

SAW_MSIL_OBJ	= $(SAW_MSIL_FILE:%=SawMsil/%.o)

#*--- SawJvm -----------------------------------------------------------*/
SAW_JVM_FILE	= compile type code inline funcall names out

SAW_JVM_OBJ	= $(SAW_JVM_FILE:%=SawJvm/%.o)

#*--- Hgen ------------------------------------------------------------*/
HGEN_FILE	= walk

HGEN_OBJ	= $(HGEN_FILE:%=Hgen/%.o)

#*--- tvector ---------------------------------------------------------*/
TVECTOR_FILE	= tvector access cnst

TVECTOR_OBJ	= $(TVECTOR_FILE:%=Tvector/%.o)

#*--- trace -----------------------------------------------------------*/
TRACE_FILE	= walk

TRACE_OBJ	= $(TRACE_FILE:%=Trace/%.o)

#*--- fail ------------------------------------------------------------*/
FAIL_FILE	= walk

FAIL_OBJ	= $(FAIL_FILE:%=Fail/%.o)

#*--- callcc ----------------------------------------------------------*/
CALLCC_FILE	= walk

CALLCC_OBJ	= $(CALLCC_FILE:%=Callcc/%.o)

#*--- reduce ----------------------------------------------------------*/
REDUCE_FILE	= walk cond typec copy cse same 1occ sbeta

REDUCE_OBJ	= $(REDUCE_FILE:%=Reduce/%.o)

#*--- effect ----------------------------------------------------------*/
EFFECT_FILE	= effect walk cgraph spread feffect

EFFECT_OBJ	= $(EFFECT_FILE:%=Effect/%.o)

#*--- object ----------------------------------------------------------*/
OBJECT_FILE	= class generic method getter \
                  plain-access wide-access java-access \
                  struct slots tools coercion predicate creator nil

OBJECT_OBJ	= $(OBJECT_FILE:%=Object/%.o)

#*--- bdb -------------------------------------------------------------*/
BDB_FILE	= setting spread-obj initialize walk emit

BDB_OBJ		= $(BDB_FILE:%=Bdb/%.o)

#*--- prof -------------------------------------------------------------*/
PROF_FILE	= unit emit

PROF_OBJ	= $(PROF_FILE:%=Prof/%.o)

#*--- user ------------------------------------------------------------*/
USER_FILE	= user

USER_OBJ	= $(USER_FILE:%=User/%.o)

#*---------------------------------------------------------------------*/
#*    The object files                                                 */
#*---------------------------------------------------------------------*/
O_OBJECTS_NOINIT= $(ENGINE_OBJ) $(WRITE_OBJ) \
                  $(READ_OBJ) $(AST_OBJ) $(CC_OBJ) $(FOREIGN_OBJ) \
                  $(HEAP_OBJ) $(MODULE_OBJ) $(TYPE_OBJ) $(EXPAND_OBJ) \
                  $(INLINE_OBJ) $(BIVALUE_OBJ) $(USER_OBJ) $(HGEN_OBJ) \
                  $(COERCE_OBJ) $(GLOBALIZE_OBJ) $(CNST_OBJ) $(CFA_OBJ) \
		  $(INTEGRATE_OBJ) $(TAILC_OBJ) $(CGEN_OBJ) $(TVECTOR_OBJ) \
                  $(TSTRUCT_OBJ) $(TRACE_OBJ) $(CALLCC_OBJ) $(EVAL_OBJ) \
                  $(LIFEXT_OBJ) $(FAIL_OBJ) $(REDUCE_OBJ) $(EFFECT_OBJ) \
                  $(OBJECT_OBJ) $(TOOLS_OBJ) $(BDB_OBJ) $(PROF_OBJ) \
                  $(JAS_OBJ) $(JVM_OBJ) $(DOTNET_OBJ) $(BACKEND_OBJ) \
                  $(SAW_OBJ) $(SAW_C_OBJ) $(SAW_JVM_OBJ) $(SAW_MSIL_OBJ)
O_OBJECTS       = $(O_OBJECTS_NOINIT) $(INIT_OBJ)
CLASS_OBJECTS	= $(O_OBJECTS:%.o=%.class)
DOTNET_OBJECTS	= $(O_OBJECTS:%.o=%.obj)

# This entry is used by QNC, for building bigloo++. Mainly Christian
# defines is own main entry point. This list does not contains the
# init/main.o file
O_OBJECTS_NOMAIN= $(O_OBJECTS_NOINIT) $(INIT_OBJ_NOMAIN)

C_OBJECTS	= $(O_OBJECTS:%.o=%.c)

SOURCE_FILES	= $(O_OBJECTS:%.o=%.scm)

POPULATION	= $(SOURCE_FILES) \
		  Ast/node.sch \
		  Ast/unit.sch \
		  Engine/pass.sch \
		  Expand/expander.sch \
		  Tools/location.sch \
		  Tools/fprint.sch \
		  Tools/verbose.sch \
		  Tools/trace.sch \
		  Tvector/tvector.sch \
		  Type/coercer.sch \
                  Makefile

#*---------------------------------------------------------------------*/
#*    Suffixes                                                         */
#*---------------------------------------------------------------------*/
.SUFFIXES:

#*---------------------------------------------------------------------*/
#*    Bootstrap (one round) the compiler                               */
#*---------------------------------------------------------------------*/
bigloo:
	@ $(MAKE) bigloo.$(DEFAULT)

#*---------------------------------------------------------------------*/
#*    jvm                                                              */
#*---------------------------------------------------------------------*/
jvm: $(CLASS_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) -jvm -jvm-jar -jvm-java $(JAVA) \
             -jvm-shell $(JSHELL) \
             -jvm-classpath $(CLASSPATH) -jvm-jarpath $(JARPATH) \
             $(BDISTFLAGS) -o $(BIGLOO_DEST).jvm $(CLASS_OBJECTS)
	@ echo "$(BIGLOO_DEST).jvm done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    dotnet                                                           */
#*---------------------------------------------------------------------*/
dotnet: $(DOTNET_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) -dotnet $(BDISTFLAGS) -o $(BIGLOO_DEST)-dotnet$(SCRIPTEXTENSION) $(DOTNET_OBJECTS)
	@ echo "$(BIGLOO_DEST)-dotnet done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    distrib                                                          */
#*    -------------------------------------------------------------    */
#*    We prepare the compiler for a distribution                       */
#*---------------------------------------------------------------------*/
distrib: 
	@ if [ `pwd` = $(BOOTDIR)/comptime ]; then \
             echo "*** ERROR:Illegal dir to make a distrib `pwd`"; \
             exit 1; \
          fi
	@ $(MAKE) afile
	@ $(MAKE) jfile
	@ $(MAKE) dist-updates
	@ $(MAKE) dist-bigloo.exe CSRCSUFFIX="%.c"

#*---------------------------------------------------------------------*/
#*    bigloo.plain and bigloo.new                                      */
#*    -------------------------------------------------------------    */
#*    Because some OS does not support executable to be changed while  */
#*    running we cannot directly override the file used to compile.    */
#*    That is, if we are compiling Bigloo with a file called           */
#*    bin/bigloo we cannot directly produce a new version called       */
#*    bin/bigloo. Instead, we generate a file called bin/bigloo.plain  */
#*    and then, we rename this file into bin/bigloo.                   */
#*    -------------------------------------------------------------    */
#*    On a bootstrap, we _always_ change the compiler date. That's     */
#*    why, Tools/date.scm is erased and rebuild.                       */
#*---------------------------------------------------------------------*/
.PHONY: plain new newnew

plain: bigloo.plain
new: bigloo.new
so: bigloo.so

bigloo.plain: Tools/license.scm
	@- $(RM) -f Tools/date.scm
	@ $(MAKE) Tools/date.scm
	@ $(MAKE) $(BIGLOO_DEST)

bigloo.new:
	@ $(MAKE) $(BIGLOO_DEST).$(NEW_SUFFIX)

renew:
	@ $(RM) -f $(BIGLOO_DEST).$(NEW_SUFFIX)
	@ $(MAKE) new

bigloo.so:
	@ $(MAKE) $(BIGLOO_DEST).so

#*---------------------------------------------------------------------*/
#*    bigloo                                                           */
#*    -------------------------------------------------------------    */
#*    For bigloo.dest, we have to use a temporary target and then      */
#*    rename into the final target. Otherwise we may have trouble      */
#*    during the linkg if BIGLOO and BIGLOO_DEST are the same          */
#*    file.                                                            */
#*---------------------------------------------------------------------*/
$(BIGLOO_DEST): $(O_OBJECTS)
	@ echo "Linking..."
	$(BIGLOO) $(BFLAGS) $(EFLAGS) $(LDBOOTFLAGS) \
                  -copt "$(EXTRA_LD_OPT)" \
                  -o $(BIGLOO_DEST).aux \
                  $(O_OBJECTS)
	@ mv $(BIGLOO_DEST).aux $(BIGLOO_DEST)$(EXE_SUFFIX)
	@ echo "$(BIGLOO_DEST)$(EXE_SUFFIX) done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    bigloo.new                                                       */
#*---------------------------------------------------------------------*/
$(BIGLOO_DEST).$(NEW_SUFFIX): $(O_OBJECTS)
	@ echo "Linking..."
	@ $(BIGLOO) $(BFLAGS) $(EFLAGS) $(LDBOOTFLAGS) \
                    -o $(BIGLOO_DEST).$(NEW_SUFFIX) \
          $(O_OBJECTS)
	@ echo "$(BIGLOO_DEST).$(NEW_SUFFIX) done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    bigloo.so                                                        */
#*---------------------------------------------------------------------*/
$(BIGLOO_DEST).so: $(O_OBJECTS)
	@ echo "Linking..."
	@ $(BIGLOO) $(BFLAGS) $(EFLAGS) -o $(BIGLOO_DEST).so $(O_OBJECTS) \
                    -ldopt $(CSTRIPFLAGS)
	@ echo "$(BIGLOO_DEST).so done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    mainnewo (for Bigloo++ regeneration)                             */
#*---------------------------------------------------------------------*/
mainnewo: $(O_LESS_MAIN_OBJECTS_OBJECTS)
	@ $(BIGLOO) $(BFLAGS) $(EFLAGS) -o $(BIGLOO_DEST) $(ADDITIONAL_BFLAGS) \
	    $(INIT_MAIN_SCM) $(O_LESS_MAIN_OBJECTS) $(ADDITIONAL_LIBRARIES)

#*---------------------------------------------------------------------*/
#*    dist-bigloo.exe                                                  */
#*---------------------------------------------------------------------*/
dist-bigloo.exe: $(C_OBJECTS)
	@ echo "Comptime (distribution) done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    Tools/date.scm                                                   */
#*    -------------------------------------------------------------    */
#*    We produce a legal Scheme file, that implements a function,      */
#*    returning the date of the compiler bootstrap.                    */
#*---------------------------------------------------------------------*/
Tools/date.scm:
	@- $(RM) -f $@
	@ echo ";; Automatically generated file (don't edit)" > $@
	@ echo "(module tools_date (export (bigloo-date)))" >> $@
	@ echo "" >> $@
	@ echo "(define (bigloo-date) " \" `date` \"")" >> $@

#*---------------------------------------------------------------------*/
#*    Tools/license.scm                                                */
#*    -------------------------------------------------------------    */
#*    We produce the license module from the official Bigloo license.  */
#*---------------------------------------------------------------------*/
Tools/license.scm: $(LICENSE)
	@- $(RM) -f $@
	@ echo ";; Automatically generated file (don't edit)" > $@
	@ echo "(module tools_license (export (bigloo-license)))" >> $@
	@ echo "" >> $@
	@ echo "(define (bigloo-license)" >> $@
	@ echo " \" `cat ../LICENSE` \")" >> $@

#*---------------------------------------------------------------------*/
#*    afile                                                            */
#*    -------------------------------------------------------------    */
#*    This entry point produce the .afile file, the .etags file for    */
#*    emacs and update the Makefile for dependences.                   */
#*---------------------------------------------------------------------*/
.PHONY: afile jfile ude
ude: afile jfile
afile: Tools/date.scm Tools/license.scm
	@ echo "   - " $(AFILE)
	@ $(AFILE_EXE) -o $(AFILE) $(SOURCE_FILES)
	@ echo "   - " $(JFILE)
	@ $(MAKE) jfile
	@ echo "   - dep"
	@ $(MAKE) dep
	@ echo "   - .etags"
	@ $(MAKE) tags

#*---------------------------------------------------------------------*/
#*    jfile                                                            */
#*---------------------------------------------------------------------*/
jfile:
	@ $(JFILE_EXE) -o $(JFILE) $(SOURCE_FILES)

#*---------------------------------------------------------------------*/
#*    tags                                                             */
#*    -------------------------------------------------------------    */
#*    The emacs .etags file                                            */
#*---------------------------------------------------------------------*/
.PHONY: tags
tags:
	@ $(BTAGS_EXE) --define-var doc-define -o .etags $(SOURCE_FILES)

#*---------------------------------------------------------------------*/
#*    qnctags                                                          */
#*---------------------------------------------------------------------*/
.PHONY: qnctags
qnctags:
	@ etags `find . -name '*.scm' -print -o -name '*.[ch]' -print`

#*---------------------------------------------------------------------*/
#*    dep                                                              */
#*---------------------------------------------------------------------*/
.PHONY: dep
dep: Tools/date.scm Tools/license.scm
	@(num=`grep -n '^#bdepend start' Makefile | awk -F: '{ print $$1}' -`;\
          head -`expr $$num - 1` Makefile > /tmp/Makefile.aux)
	@ $(DEPEND_EXE) -fno-mco $(SOURCE_FILES) >> /tmp/Makefile.aux
	@ mv /tmp/Makefile.aux Makefile

#*---------------------------------------------------------------------*/
#*    dist-updates                                                     */
#*    -------------------------------------------------------------    */
#*    When compiling a distribution we have to make some updates into  */
#*    the source files. Mostly we have to remove debug informations.   */
#*---------------------------------------------------------------------*/
dist-updates:
	echo "updating Tools/trace.sc[hm]"
	cat Tools/trace.sch | sed -e s/"#t"/"#f"/g > Tools/trace.schu; \
         $(RM) Tools/trace.sch; mv Tools/trace.schu Tools/trace.sch
	cat Tools/trace.scm | sed -e s/"#t"/"#f"/g > Tools/trace.scmu; \
         $(RM) Tools/trace.scm; mv Tools/trace.scmu Tools/trace.scm

#*---------------------------------------------------------------------*/
#*    boot                                                             */
#*    -------------------------------------------------------------    */
#*    This is the regular entry point for a standard Bigloo boot       */
#*    procedure (from the C files).                                    */
#*---------------------------------------------------------------------*/
.PHONY: boot doboot

boot: 
	$(MAKE) doboot SCMOBJSUFFIX="" COBJSUFFIX=%.o

doboot: $(O_OBJECTS)
	if [ "$(SHRD_COMP)" = "yes" -a -f $(BOOTLIBDIR)/lib$(LIBRARYNAME)_s$(VERSION)$(SHAREDSUFFIX) ]; then \
           gc=-lbigloogc-$(RELEASE); \
           if [ "$(GCCUSTOM)" = "no" ]; then \
	     gc=-lgc; \
           fi; \
	   $(CC) $(EXTRA_LD_OPT) $(CFLAGS) -o $(BIGLOO_DEST) $(O_OBJECTS) -L $(BOOTLIBDIR) -lbigloo_s-$(RELEASE) $$gc $(EXTRALIBS); \
	else \
           gc=$(BOOTLIBDIR)/libbigloogc-$(RELEASE).a; \
           if [ "$(GCCUSTOM)" = "no" ]; then \
              gc=-lgc; \
           fi; \
	   $(CC) $(EXTRA_LD_OPT) $(CFLAGS) -o $(BIGLOO_DEST) $(O_OBJECTS) $(BOOTLIBDIR)/libbigloo_s-$(RELEASE).a $$gc $(EXTRALIBS); \
	fi
	-$(STRIP) $(BIGLOO_DEST)$(EXE_SUFFIX)
	@ echo "$(BIGLOO_DEST) bigloo done..."

#*---------------------------------------------------------------------*/
#*    bigboot                                                          */
#*    -------------------------------------------------------------    */
#*    This is the regular entry point for a standard Bigloo boot       */
#*    procedure from the Scheme files, using an already (compatible)   */
#*    installed compiler.                                              */
#*---------------------------------------------------------------------*/
.PHONY: bigboot

bigboot: 
	$(MAKE) SCMOBJSUFFIX="%.o" LDBOOTFLAGS="-lib-version $(RELEASE) -static-bigloo -ldopt $(CSTRIPFLAGS)"
	-$(STRIP) $(BIGLOO_DEST)$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*    install                                                          */
#*---------------------------------------------------------------------*/
.PHONY: install uninstall

install:
	if [ $(BINDIR) != $(BOOTBINDIR) ]; then \
	   if [ ! -d $(DESTDIR)$(BINDIR) ]; then \
             mkdir -p $(DESTDIR)$(BINDIR); chmod a+rx $(DESTDIR)$(BINDIR) || exit 1; \
           fi; \
	   cp $(BOOTBINDIR)/bigloo$(EXE_SUFFIX) $(DESTDIR)$(BINDIR)/bigloo$(RELEASE)$(EXE_SUFFIX) && \
	   chmod $(BMASK) $(DESTDIR)$(BINDIR)/bigloo$(RELEASE)$(EXE_SUFFIX) && \
           (cd $(DESTDIR)$(BINDIR) && \
            $(RM) -f bigloo && \
            $(RM) -f bigloo$(EXE_SUFFIX) && \
            ln bigloo$(RELEASE)$(EXE_SUFFIX) bigloo$(EXE_SUFFIX)) || exit 1; \
        fi

uninstall:
	- $(RM) -f $(DESTDIR)$(BINDIR)/bigloo$(RELEASE)$(EXE_SUFFIX)
	- $(RM) -f $(DESTDIR)$(BINDIR)/bigloo$(EXE_SUFFIX)

#*---------------------------------------------------------------------*/
#*     clean ...                                                       */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean-common:
	@- $(RM) -f $(AFILE)
	@- $(RM) -f $(JFILE)
	@- $(RM) -f .etags
	@ find . \( -name '*[~%]'                   \
                       -o -name '.??*[~%]'          \
                       -o -name '#*#'               \
                       -o -name '?*#'               \
                       -o -name \*core \)           \
                     -type f -exec $(RM) {} \;   

clean-c:
	@- $(RM) -f $(O_OBJECTS)

clean-jvm:
	@- $(RM) -f $(CLASS_OBJECTS)
	@- $(RM) -f $(BIGLOO_DEST).jvm
	@- $(RM) -f $(BIGLOO_DEST).jar

clean-dotnet:
	@- $(RM) -f $(DOTNET_OBJECTS)
	@- $(RM) -f $(BIGLOO_DEST)-dotnet$(SCRIPTEXTENSION)
	@- $(RM) -f $(BIGLOO_DEST)-dotnet$(EXE_SUFFIX)

clean: clean-common clean-c clean-jvm clean-dotnet
	@ echo "cleanup done..."
	@ echo "-------------------------------"

cleanall: clean
	@ if [ `pwd` = $(HOME)/prgm/project/bigloo/comptime ]; then \
             echo "*** ERROR:Illegal dir to make a cleanall `pwd`"; \
             exit 1; \
          fi
	@- $(RM) -f $(C_OBJECTS)
	@- $(RM) -f Tools/date.scm Tools/license.scm
	@- $(RM) -f $(BIGLOO_DEST)
	@- $(RM) -f $(AFILE_EXE) 
	@- $(RM) -f $(DEPEND_EXE)

distclean: 
	@- $(RM) -f $(O_OBJECTS)
	@- $(RM) -f $(BIGLOO_DEST)

#*---------------------------------------------------------------------*/
#*     touchall ...                                                    */
#*---------------------------------------------------------------------*/
.PHONY: touchall
touchall:
	@ touch $(SOURCE_FILES)
	@ echo "touchall done..."
	@ echo "-------------------------------"

#*---------------------------------------------------------------------*/
#*    getsources                                                       */
#*---------------------------------------------------------------------*/
getsources:
	@ echo $(SOURCE_FILES)

#*---------------------------------------------------------------------*/
#*     .scm --> .o                                                     */
#*---------------------------------------------------------------------*/
$(SCMOBJSUFFIX): %.scm
	$(BIGLOO) $(BFLAGS) $(EFLAGS) -c $*.scm -o $*.o

#*---------------------------------------------------------------------*/
#*     .scm --> .class                                                 */
#*---------------------------------------------------------------------*/
$(SCMCLASSSUFFIX): %.scm
	$(BIGLOO) -jvm $(BJVMFLAGS) $(EFLAGS) -c $*.scm -o $*.class

#*---------------------------------------------------------------------*/
#*     .scm --> .obj                                                   */
#*---------------------------------------------------------------------*/
$(DOTNETOBJSUFFIX): %.scm
	$(BIGLOO) -dotnet $(BDOTNETFLAGS) $(EFLAGS) -c $*.scm -o $*.obj

#*---------------------------------------------------------------------*/
#*     .c --> .o                                                       */
#*---------------------------------------------------------------------*/
$(COBJSUFFIX): %.c
	$(CC) $(CFLAGS) $(CADDFLAGS) $(EFLAGS) $*.c -c && mv $(@F) $*.o

#*---------------------------------------------------------------------*/
#*     .scm --> .c                                                     */
#*---------------------------------------------------------------------*/
$(CSRCSUFFIX): %.scm
	$(BIGLOO) $(BDISTFLAGS) $(EFLAGS) -indent $*.scm

#*---------------------------------------------------------------------*/
#*    pop                                                              */
#*    -------------------------------------------------------------    */
#*    This entry is used by the bigloo/Makefile (the main Bigloo       */
#*    makefile) to get the list of file that populate a revision.      */
#*---------------------------------------------------------------------*/
.PHONY: pop
pop: Tools/date.scm Tools/license.scm
	@ echo $(POPULATION:%=comptime/%)

#*---------------------------------------------------------------------*/
#*    Dependences                                                      */
#*---------------------------------------------------------------------*/
Engine/param.o: $(BOOTLIBDIR)/bigloo_config.h ../configure

#bdepend start (don't edit)
#*---------------------------------------------------------------------*/
#*    Dependencies ...                                                 */
#*---------------------------------------------------------------------*/
Globalize/node.o Globalize/node.class: Tools/trace.sch 
Object/getter.o Object/getter.class: Tools/trace.sch 
Reduce/typec.o Reduce/typec.class: Tools/trace.sch 
Tools/progn.o Tools/progn.class: Tools/location.sch 
Ast/build.o Ast/build.class: Engine/pass.sch Ast/node.sch 
Foreign/calias.o Foreign/calias.class: Tools/trace.sch 
Ast/check-sharing.o Ast/check-sharing.class: Ast/unit.sch                \
      Engine/pass.sch 
Coerce/app.o Coerce/app.class: Tools/trace.sch 
Foreign/cptr.o Foreign/cptr.class: Tools/trace.sch 
Cc/exec.o Cc/exec.class: Tools/trace.sch 
BackEnd/jvm.o BackEnd/jvm.class: Engine/pass.sch 
Globalize/ginfo.o Globalize/ginfo.class: Ast/node.sch 
Integrate/definition.o Integrate/definition.class: Tools/trace.sch       \
      Tools/verbose.sch 
Module/eval.o Module/eval.class: Ast/unit.sch Ast/node.sch 
Ast/remove.o Ast/remove.class: Ast/node.sch 
Globalize/globalize.o Globalize/globalize.class: Tools/trace.sch         \
      Tools/verbose.sch 
Integrate/walk.o Integrate/walk.class: Engine/pass.sch 
Object/predicate.o Object/predicate.class: Tools/trace.sch 
Object/java-access.o Object/java-access.class: Tools/trace.sch 
Bdb/spread-obj.o Bdb/spread-obj.class: Engine/pass.sch Ast/node.sch 
Expand/assert.o Expand/assert.class: Tools/location.sch 
Coerce/convert.o Coerce/convert.class: Tools/trace.sch                   \
      Tools/location.sch Type/coercer.sch 
DotNet/init.o DotNet/init.class: Tvector/tvector.sch 
Cfa/show.o Cfa/show.class: Tools/trace.sch 
Expand/case.o Expand/case.class: Tools/trace.sch 
Effect/walk.o Effect/walk.class: Engine/pass.sch Tools/trace.sch 
Module/alibrary.o Module/alibrary.class: Ast/unit.sch 
Read/load.o Read/load.class: Tools/trace.sch 
Ast/labels.o Ast/labels.class: Ast/node.sch Tools/trace.sch              \
      Tools/location.sch 
Cnst/node.o Cnst/node.class: Tools/trace.sch Tvector/tvector.sch 
Ast/glo-decl.o Ast/glo-decl.class: Tools/trace.sch 
Ast/unit.o Ast/unit.class: Ast/unit.sch Ast/node.sch Tools/trace.sch 
Module/java.o Module/java.class: Ast/unit.sch Tools/trace.sch 
Effect/cgraph.o Effect/cgraph.class: Ast/node.sch Tools/trace.sch 
BackEnd/cplib.o BackEnd/cplib.class: Tvector/tvector.sch 
Cfa/procedure.o Cfa/procedure.class: Tools/trace.sch 
DotNet/dotnet.o DotNet/dotnet.class: Ast/unit.sch Engine/pass.sch 
Globalize/integration.o Globalize/integration.class: Tools/trace.sch 
Integrate/g.o Integrate/g.class: Tools/trace.sch 
Integrate/let-fun.o Integrate/let-fun.class: Tools/trace.sch 
Inline/recursion.o Inline/recursion.class: Tools/trace.sch               \
      Tools/verbose.sch 
SawC/compile.o SawC/compile.class: Engine/pass.sch Ast/unit.sch          \
      Tools/trace.sch 
Ast/sexp.o Ast/sexp.class: Tools/trace.sch Tools/location.sch 
Ast/pragma.o Ast/pragma.class: Ast/node.sch 
Tvector/cnst.o Tvector/cnst.class: Tvector/tvector.sch 
Heap/restore.o Heap/restore.class: Engine/pass.sch 
Ast/let.o Ast/let.class: Ast/node.sch Tools/trace.sch Tools/location.sch 
Ast/occur.o Ast/occur.class: Ast/node.sch 
Ast/substitute.o Ast/substitute.class: Tools/trace.sch 
Inline/variant.o Inline/variant.class: Tools/trace.sch 
Integrate/loc2glo.o Integrate/loc2glo.class: Tools/trace.sch 
Reduce/copy.o Reduce/copy.class: Tools/trace.sch 
Expand/exit.o Expand/exit.class: Expand/expander.sch Tools/trace.sch 
Inline/size.o Inline/size.class: Ast/node.sch 
Object/coercion.o Object/coercion.class: Tools/trace.sch 
Expand/if.o Expand/if.class: Tools/trace.sch 
Inline/loop.o Inline/loop.class: Tools/trace.sch 
Module/statexp.o Module/statexp.class: Ast/unit.sch 
Ast/find-gdefs.o Ast/find-gdefs.class: Tools/trace.sch Ast/node.sch 
Type/tenv.o Type/tenv.class: Tools/trace.sch Type/coercer.sch 
Type/typeof.o Type/typeof.class: Tvector/tvector.sch 
SawMsil/compile.o SawMsil/compile.class: Tvector/tvector.sch 
Tools/shape.o Tools/shape.class: Ast/node.sch Tvector/tvector.sch 
Cfa/box.o Cfa/box.class: Tools/trace.sch 
Module/module.o Module/module.class: Ast/unit.sch Engine/pass.sch 
Reduce/cond.o Reduce/cond.class: Tools/trace.sch 
Globalize/clocto.o Globalize/clocto.class: Tools/trace.sch 
Foreign/jtype.o Foreign/jtype.class: Tools/trace.sch 
Ast/check-init.o Ast/check-init.class: Ast/unit.sch 
Cfa/funcall.o Cfa/funcall.class: Tools/trace.sch 
Integrate/node.o Integrate/node.class: Tools/trace.sch 
Reduce/sbeta.o Reduce/sbeta.class: Tools/trace.sch 
Integrate/ctn.o Integrate/ctn.class: Tools/trace.sch 
Coerce/coerce.o Coerce/coerce.class: Tools/trace.sch 
Reduce/1occ.o Reduce/1occ.class: Tools/trace.sch 
Write/expanded.o Write/expanded.class: Ast/unit.sch 
Object/slots.o Object/slots.class: Tools/trace.sch 
Effect/spread.o Effect/spread.class: Ast/node.sch 
Cfa/loose.o Cfa/loose.class: Tools/trace.sch 
Cfa/cfa.o Cfa/cfa.class: Tools/trace.sch 
BackEnd/c-proto.o BackEnd/c-proto.class: Tvector/tvector.sch             \
      Tools/fprint.sch 
Cfa/setup.o Cfa/setup.class: Tools/trace.sch 
Globalize/new-body.o Globalize/new-body.class: Tools/trace.sch 
Cnst/walk.o Cnst/walk.class: Engine/pass.sch 
Object/wide-access.o Object/wide-access.class: Tools/trace.sch 
Cgen/capp.o Cgen/capp.class: Tools/trace.sch 
Cfa/vector.o Cfa/vector.class: Tools/trace.sch 
Ast/exit.o Ast/exit.class: Ast/node.sch Tools/trace.sch 
Coerce/funcall.o Coerce/funcall.class: Tools/trace.sch                   \
      Tools/location.sch 
Prof/emit.o Prof/emit.class: Ast/node.sch Ast/unit.sch                   \
      Tools/location.sch 
Ast/app.o Ast/app.class: Ast/node.sch Tools/trace.sch 
Cfa/tvector.o Cfa/tvector.class: Tools/trace.sch Ast/unit.sch            \
      Tvector/tvector.sch 
Prof/unit.o Prof/unit.class: Engine/pass.sch Ast/unit.sch 
Expand/eps.o Expand/eps.class: Expand/expander.sch Tools/trace.sch       \
      Engine/pass.sch Ast/unit.sch 
Coerce/apply.o Coerce/apply.class: Tools/trace.sch Tools/location.sch 
Callcc/walk.o Callcc/walk.class: Engine/pass.sch Ast/node.sch 
Inline/walk.o Inline/walk.class: Engine/pass.sch Tools/trace.sch         \
      Ast/node.sch 
Heap/make.o Heap/make.class: Engine/pass.sch 
Tailc/walk.o Tailc/walk.class: Engine/pass.sch 
Jvm/extern.o Jvm/extern.class: Tools/location.sch Ast/unit.sch           \
      Tvector/tvector.sch 
Object/creator.o Object/creator.class: Tools/trace.sch 
Cfa/iterate.o Cfa/iterate.class: Tools/trace.sch 
Engine/compiler.o Engine/compiler.class: Ast/unit.sch Engine/pass.sch 
Expand/lambda.o Expand/lambda.class: Tools/trace.sch 
Hgen/walk.o Hgen/walk.class: Engine/pass.sch 
Module/library.o Module/library.class: Ast/unit.sch 
BackEnd/c.o BackEnd/c.class: Engine/pass.sch Ast/unit.sch                \
      Tools/trace.sch 
Jvm/jdump.o Jvm/jdump.class: Ast/unit.sch Engine/pass.sch 
DotNet/extern.o DotNet/extern.class: Tools/location.sch Ast/unit.sch     \
      Tvector/tvector.sch 
Globalize/free.o Globalize/free.class: Tools/trace.sch 
Cfa/specialize.o Cfa/specialize.class: Tools/trace.sch 
SawMill/node2rtl.o SawMill/node2rtl.class: Tools/location.sch 
User/user.o User/user.class: Engine/pass.sch Ast/unit.sch 
Inline/simple.o Inline/simple.class: Tools/trace.sch Tools/verbose.sch 
Integrate/u.o Integrate/u.class: Tools/trace.sch 
Integrate/cto.o Integrate/cto.class: Tools/trace.sch 
Reduce/walk.o Reduce/walk.class: Engine/pass.sch 
Jvm/init.o Jvm/init.class: Tvector/tvector.sch 
Reduce/same.o Reduce/same.class: Tools/trace.sch 
Cnst/alloc.o Cnst/alloc.class: Tvector/tvector.sch 
Init/parse-args.o Init/parse-args.class: Tools/trace.sch 
Inline/app.o Inline/app.class: Tools/trace.sch 
Expand/expander.o Expand/expander.class: Expand/expander.sch 
Cfa/walk.o Cfa/walk.class: Engine/pass.sch 
Ast/apply.o Ast/apply.class: Ast/node.sch 
Globalize/gn.o Globalize/gn.class: Tools/trace.sch 
Cnst/initialize.o Cnst/initialize.class: Tools/trace.sch Ast/unit.sch 
Cgen/cgen.o Cgen/cgen.class: Tools/trace.sch Tools/location.sch 
Tools/error.o Tools/error.class: Tools/location.sch Ast/node.sch 
Cfa/app.o Cfa/app.class: Tools/trace.sch 
DotNet/vmemq.o DotNet/vmemq.class: Ast/node.sch Tools/location.sch 
Fail/walk.o Fail/walk.class: Engine/pass.sch Tools/location.sch          \
      Ast/node.sch 
Globalize/loc2glo.o Globalize/loc2glo.class: Tools/trace.sch 
Trace/walk.o Trace/walk.class: Engine/pass.sch Ast/node.sch              \
      Tools/location.sch 
Inline/inline.o Inline/inline.class: Ast/node.sch Tools/trace.sch 
Bdb/emit.o Bdb/emit.class: Ast/node.sch Ast/unit.sch Tools/location.sch 
Effect/feffect.o Effect/feffect.class: Tools/trace.sch 
Module/foreign.o Module/foreign.class: Ast/unit.sch Tools/trace.sch 
Globalize/walk.o Globalize/walk.class: Engine/pass.sch Ast/node.sch 
Cfa/collect.o Cfa/collect.class: Tools/trace.sch 
Cfa/closure.o Cfa/closure.class: Tools/trace.sch 
Ast/alphatize.o Ast/alphatize.class: Ast/node.sch Tools/location.sch 
Type/coercion.o Type/coercion.class: Type/coercer.sch Tools/trace.sch 
Integrate/a.o Integrate/a.class: Tools/trace.sch 
Cgen/walk.o Cgen/walk.class: Engine/pass.sch Ast/unit.sch                \
      Tools/trace.sch 
Foreign/ctype.o Foreign/ctype.class: Tools/trace.sch 
Integrate/kk.o Integrate/kk.class: Tools/trace.sch 
Bdb/walk.o Bdb/walk.class: Engine/pass.sch 
Cfa/struct.o Cfa/struct.class: Tools/trace.sch 
Ast/dump.o Ast/dump.class: Tvector/tvector.sch Ast/node.sch 
Expand/initial.o Expand/initial.class: Tools/location.sch 
BackEnd/dotnet.o BackEnd/dotnet.class: Engine/pass.sch 
Expand/define.o Expand/define.class: Expand/expander.sch Tools/trace.sch 
Cgen/emit-cop.o Cgen/emit-cop.class: Tools/location.sch Tools/fprint.sch \
      Tools/trace.sch 
Module/class.o Module/class.class: Ast/unit.sch Tools/trace.sch 
Cfa/arithmetic.o Cfa/arithmetic.class: Tools/trace.sch 
Globalize/kapture.o Globalize/kapture.class: Tools/trace.sch 
Jvm/vmemq.o Jvm/vmemq.class: Ast/node.sch Tools/location.sch 
Object/nil.o Object/nil.class: Tools/trace.sch 
Module/option.o Module/option.class: Ast/unit.sch 
Tools/location.o Tools/location.class: Tools/location.sch 
Module/include.o Module/include.class: Ast/unit.sch 
Read/inline.o Read/inline.class: Tools/trace.sch Ast/unit.sch 
Reduce/cse.o Reduce/cse.class: Tools/trace.sch 
Foreign/cstruct.o Foreign/cstruct.class: Tools/trace.sch 
Object/plain-access.o Object/plain-access.class: Tools/trace.sch 
Bdb/initialize.o Bdb/initialize.class: Tools/trace.sch Ast/unit.sch 
Globalize/gloclo.o Globalize/gloclo.class: Tools/trace.sch 
SawJvm/compile.o SawJvm/compile.class: Tvector/tvector.sch 
Module/type.o Module/type.class: Ast/unit.sch Tools/trace.sch 
Module/impuse.o Module/impuse.class: Ast/unit.sch Tools/trace.sch 
SawC/code.o SawC/code.class: Tools/location.sch 
Effect/effect.o Effect/effect.class: Ast/node.sch 
Write/ast.o Write/ast.class: Engine/pass.sch Ast/node.sch 
Integrate/free.o Integrate/free.class: Tools/trace.sch 
Coerce/pproto.o Coerce/pproto.class: Tools/verbose.sch 
Integrate/kaptured.o Integrate/kaptured.class: Tools/trace.sch 
Ast/glo-def.o Ast/glo-def.class: Tools/trace.sch 
Coerce/walk.o Coerce/walk.class: Engine/pass.sch 

#bdepend stop
