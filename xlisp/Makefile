##############################################################################
##############################################################################
##
##                       Configuration Parameters
##
##############################################################################
##############################################################################
#
# XLSLIB -- directory for xlisp executable, startup, example, and help files
# BINDIR -- directory for xlispstat shell script
#
# For the remaining parameters, if your machine has a subdirectory in the 
# machines ditectory, check the values given in the README file of your
# machine's directory. Otherwise, try using the generic machine type. If
# you bring xlispstat up on a mathine not listed in the machines directory,
# please let me (luke@stat.umn.edu) know what you used for these variables,
# so I can add your machine type to future releases.
#
# GRAPHSYS -- X11WINDOWS or GNUPLOT
# X11INCDIR_FLAG -- empty or -Idir for X11 include files
# X11LIBDIR_FLAG -- empry or -Ldir for X11 libraries
#
# UCFLAGS -- C compiler flags, such as -O
# ULDFLAGS -- additional flags for linking,,such as -x
#
# EXTRALIBS -- any additional libraries, such as -ldl on some systems
# EXTRAOBJS -- any additional object files
#
# FOREIGN_FILE -- one of the dynamic loading files *-foreign.h
#
# CC -- usually cc, the C compiler to use
# LDCC -- usually also cc, the commant to link the objects

prefix = /home/jtan/Apps/xlisp
exec_prefix = ${prefix}

XLSLIB=${prefix}/lib/xlispstat
BINDIR=${exec_prefix}/bin

GRAPHSYS = X11WINDOWS
X11INCDIR_FLAG = 
X11LIBDIR_FLAG = 

UCFLAGS = -fPIC -O
ULDFLAGS =  -export-dynamic

EXTRALIBS = -lR -ldl
EXTRAOBJS = 

FOREIGN_FILE = sysvr4-foreign.h

CC = cc
LDCC = $(CC)

SHELL = /bin/sh

###############################################################################
###############################################################################
###                                                                         ###
###                      DO NOT EDIT BELOW THIS LINE                        ###
###                                                                         ###
###############################################################################
###############################################################################

.SUFFIXES: .lsp .fsl

.lsp.fsl:
	echo "(compile-file \"$<\") (exit)" | ./xlisp

CFLAGS = -D${GRAPHSYS} ${UCFLAGS} ${X11INCDIR_FLAG}

LIBS = ${EXTRALIBS} -lm

LDFLAGS = ${ULDFLAGS}

OSOBJS = xsdynload.o xssystem.o unixprim.o

XLISPOBJS = xlarray.o xlbfun.o xlbignum.o xlcont.o xldbug.o xldmem.o \
	xleval.o xlfio.o xlftab.o xlglob.o xlimage.o xlinit.o xlio.o \
	xlisp.o xljump.o xllist.o xlpp.o xlprin.o \
	xlrand.o xlread.o xlseq.o xlstr.o xlsubr.o xlsym.o xlsys.o \
	xlstruct.o xlmath2.o xlmath3.o xltvec.o xlbcode.o xlbcutil.o \
	xlshlib.o xlwrap.o xlmodule.o dummycod.o

STATOBJS = basics.o common.o compound.o dists.o ddists.o hrdwrobs.o mats1.o \
	mats2.o math.o obinit.o objects.o optimize.o sortdata.o \
	statinit.o stats.o utils.o utils2.o gamln.o splines.o kernel.o \
	lowess.o cfft.o betab.o gammab.o nor.o ppnd.o studentb.o bivnor.o \
	linalg.o cholesky.o ludecomp.o qrdecomp.o svdecomp.o \
	makerot.o eigen.o minimize.o blas.o

GRAFOBJS = xsiview2.o xsivint.o xsnewplt.o xsgraph.o xsiview3.o xsivwin.o \
	xsiview.o xsivwin2.o menus.o dialogs.o iviewdat.o iviewscl.o iview.o \
	iviewint.o xshist.o xsnames.o xssctmat.o xsspin.o xssctplt.o \
	windows.o stmem.o graphics.o myplot.o term.o postscript.o

BASICOBJS = ${OSOBJS} ${XLISPOBJS} ${STATOBJS} ${GRAFOBJS}

OBJS = ${BASICOBJS} ${EXTRAOBJS}

GNUPLOTOBJS = unixstuff.o dummygraph.o

X11WINDOWSOBJS = X11BSDstuff.o X11graph.o X11menus.o X11dialogs.o \
	X11buttons.o X11text.o X11toggle.o X11choice.o X11slider.o \
	X11listitem.o X11resizebr.o X11scroll.o

FSLFILES = init.fsl common.fsl common2.fsl common3.fsl help.fsl objects.fsl \
	linalg.fsl stats.fsl dialogs.fsl graphics.fsl graph2.fsl graph3.fsl \
	regress.fsl menus.fsl loadfsl.fsl conditns.fsl pathname.fsl \
	shlib.fsl cmpload.fsl

AUTOFSLFILES = oneway.fsl nonlin.fsl maximize.fsl bayes.fsl \
	stepper.fsl glim.fsl

CMPFSLFILES = compiler/backquot.fsl compiler/cmpmacro.fsl \
	compiler/convert.fsl compiler/cells.fsl compiler/simplify.fsl \
	compiler/lift.fsl compiler/gencode.fsl compiler/peephole.fsl \
	compiler/assemble.fsl compiler/cmpfront.fsl

xlispstat: setup.shell xlisp.wks
	./setup.shell xlispstat ${XLSLIB}
	chmod a+x xlispstat

libxlisp.so: ${OBJS} ${X11WINDOWSOBJS}
	$(LDCC) -shared -o $@  ${OBJS} ${X11WINDOWSOBJS} ${X11LIBDIR_FLAG} -lX11 ${LIBS}


xlisp.wks: xlisp ${CMPFSLFILES} ${FSLFILES} ${AUTOFSLFILES}
	rm -f xlisp.wks
	echo '(save-workspace "xlisp") (exit)' | ./xlisp
	cp ${AUTOFSLFILES} Autoload

xlisp: ${GRAPHSYS}xlisp
	-ln ${GRAPHSYS}xlisp xlisp

GNUPLOTxlisp: ${OBJS} ${GNUPLOTOBJS}
	rm -f xlisp xlisp.wks
	${LDCC} ${LDFLAGS} -o GNUPLOTxlisp ${OBJS} ${GNUPLOTOBJS} ${LIBS}

X11WINDOWSxlisp: ${OBJS} ${X11WINDOWSOBJS}
	rm -f xlisp xlisp.wks
	${LDCC} ${LDFLAGS} -o X11WINDOWSxlisp ${OBJS} ${X11WINDOWSOBJS} \
	        ${X11LIBDIR_FLAG} -lX11 ${LIBS}

install: installexecs installlsp

installexecs: xlispstat xlisp libxlisp.so installdirs
	-cp xlispstat ${BINDIR}/xlispstat
	-cp xlisp ${XLSLIB}
	-cp libxlisp.so ${XLSLIB}

installlsp: xlisp.wks xlisp.hlp ${AUTOFSLFILES} ${CMPFSLFILES} installdirs
	-cp xlisp.wks ${XLSLIB}
	-cp xlisp.hlp ${XLSLIB}
	-cp Autoload/_autoidx.lsp Autoload/*.fsl ${XLSLIB}/Autoload
	-cp Data/*.lsp ${XLSLIB}/Data
	-cp Examples/*.lsp ${XLSLIB}/Examples
#	-cp ${CMPFSLFILES} ${XLSLIB}/compiler

installdirs:
	-mkdir -p ${BINDIR}
	-mkdir -p ${XLSLIB}
	-mkdir ${XLSLIB}/Data
	-mkdir ${XLSLIB}/Examples
	-mkdir ${XLSLIB}/Autoload
#	-mkdir ${XLSLIB}/compiler

clean:
	rm -f core *.o *~ *.fsl compiler/*.fsl Autoload/*.fsl foreign.h

cleanall: clean
	rm -f xlisp xlisp.wks xlispstat
	rm -f GNUPLOTxlisp X11WINDOWSxlisp


##############################################################################
##############################################################################
# Dependencies

XLISP_INCLUDES = xlisp.h xldmem.h xlftab.h xlglob.h xlconfig.h
STAT_INCLUDES = xlstat.h iview.h osdefs.h

${OBJS}: ${XLISP_INCLUDES}
${STATOBJS}: ${STAT_INCLUDES}
${GRAFOBJS}: ${STAT_INCLUDES} xlgraph.h

xsdynload.o: foreign.h

foreign.h: ${FOREIGN_FILE} Makefile
	cp ${FOREIGN_FILE} foreign.h

xlftab.o: osptrs.h
xlbcode.o: xlmodule.h xlbcode.h
xlbcutil.o: xlmodule.h xlbcode.h
xlmodule.o: xlmodule.h

xlimage.o statinit.o: version.h

${GNUPLOTOBJS}: ${XLISP_INCLUDES} ${STAT_INCLUDES} xlgraph.h

unixstuff.o: version.h

graphics.o: gnuplot.h
myplot.o: gnuplot.h
term.o: gnuplot.h

${X11WINDOWSOBJS}: ${XLISP_INCLUDES} ${STAT_INCLUDES} xlgraph.h dialogs.h 

X11graph.o: StX11options.h
X11BSDstuff.o: version.h
