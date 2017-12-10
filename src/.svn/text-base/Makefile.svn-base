#------------------------------------------------------------------#
#                                                                  #
#                  FULL_MONTE executable Makefile                  #
#                     Renee Poutissou (TRIUMF)                     #
#                          1-may-1998                              #
#                                                                  #
#  usage examples:                                                 #
#                                                                  #
#  % make                                                          #
#  % make dsbatch                                                    # 
#------------------------------------------------------------------#
# 09-feb-1998 Renee - Fix the make file : add IFLAGS, add rules    #
# 10-feb-1998 Renee - adapt for Linux                              #
#------------------------------------------------------------------#
# this can be changed here, or defined on the command line.
# (Say "make OUTPUT=progname", for example.)
#
DSROOT=.
DSBIN=.

# KJR - comment out hard-coded paths
#CERN_LIBRARY= /usr/lib64/cernlib/2006-g77/lib
#X11= /usr/lib64

# KJR - seems to be unused
#F771= /usr/local/lib

# KJR - I think the paths to here should be be included in some better
# automatic way.  Also, in the rpms, hbook is in the same location
# so doesn't need a separate entry
#GINCLUDE = /cern/new/src/
#HINCLUDE = /cern/new/src/packlib/hbook/

GINCLUDE = /usr/include/cernlib/2006

# KJR - remove HINCLUDE
#IFLAGS= -I../inc -I$(GINCLUDE) -I$(HINCLUDE)
IFLAGS= -I../inc -I$(GINCLUDE)


OUTPUT= $(DSBIN)/dsbatch
#------------------------------linux needs ------------------------------#
# KJR : Use cernlib to automatically generate cernlib switches

#F77= g77 -m64 -g -Dg77 -Dunix -fno-second-underscore -fno-automatic -static -c -lcernlib -lpacklib

F77 = g77
FFLAGS = -m32 -g -Dg77 -Dunix -fno-second-underscore -fno-automatic -c
CFLAGS = -m32

#------------------------------------------------------------------------#

# this is defined so that you can specify extra object files
# on the command line.  (Say "make OBJECTS=file.o", for example.)

OBJECTS=

#------------------------------------------------------------------#
# if you have your own version of a standard library, add its
# directory to the top of this list

# KJR: Not needed , using cernlib util
#LIBDIRS= \
#   -L$(CERN_LIBRARY) -L$(X11) 

#------------------------------------------------------------------#
# add your object files and personal libraries to the end of this list

OBJS_BATCH = \
    gumain.o

OBJS_INTER = \
    gxint321.o 

OBJS = \
	beaminit.o \
	ghidet.o \
	ghipmt.o \
	ggscnt.o \
       gudigi.o \
       guhadr.o \
       gukine.o \
       gukine_full.o \
       gukine_full_up.o \
       gukine_full_down.o \
       gukine_full_right.o \
       gukine_full_left.o \
       gukine_full_hole.o \
       gukine_gbox.o \
       gukine_mitray.o \
       guout.o  \
       guphad.o \
	gureact.o \
       gustep.o \
	gustep_trgt.o \
	gustep_mitray.o \
	gustep_gbox.o \
       gutrak.o \
       gutrev.o \
       uginit.o \
       uvinit.o \
       ugffgo.o \
       uhinit.o \
       ugmate.o \
       ugmate_trgt.o \
       ugstmed.o \
       ugstmed_trgt.o \
       ureact.o \
       uglast.o \
       ugeom.o \
       ugeom_gbox.o \
	ugeom_trgt_large.o \
	ugeom_trgt_small.o \
       ugeom_trgt_small_up.o \
       ugeom_trgt_small_down.o \
       ugeom_trgt_small_right.o \
       ugeom_trgt_small_left.o \
       ugeom_trgt_small_hole.o \
       ugeom_mitray.o \
	ugeom_defin.o \
       guswim.o \
       grkuta.o \
       guefld.o \
       mitray_setup.o \
       mitray_field.o \
       mitray_dipole.o \
       mitray_edipol.o \
       mitray_solnd.o \
       mitray_poles.o \
       mitray_zone.o \
       mitray_sasp.o \
       neighborhood.o \
       gthion.o \
       geant_mod.o \
       sig.o \
       chmass.o \
       gdeca2.o \
       angdist.o \
       gdecay.o \
       interac.o \
       hack.o \
       hrndm1.o 
#	alphapos.o
#
#------------------------------------------------------------------#
# add or delete the support libraries as you require them

# KJR :
#LIBS = \
#   -lgeant321 \
#   -lmathlib \
#   -lkernlib \
#   -lpacklib  

LIBS = $(shell cernlib-g77 geant321 mathlib kernlib packlib)

# KJR :
# GLIBS = \
#    -lgeant321 \
#    -lpawlib \
#    -lgraflib \
#    -lgrafX11 \
#    -lpacklib \
#    -lmathlib \
#    -lkernlib \
#    -lblas \
#    -lnsl 

GLIBS = $(shell cernlib-g77 geant321 pawlib graflib grafX11 mathlib kernlib packlib blas nsl)


#------------------------------------------------------------------#
# batch geant, suitable for long jobs.
# Say "make" or "make dsbatch"

# for linux
dsbatch : $(OBJS_BATCH) $(OBJS) $(OBJECTS)
#	g77 -O2 -o $(OUTPUT) $(LIBDIRS) $(OBJS_BATCH) $(OBJS) $(LIBS) $(GLIBS) \
#		-lcrypt -ldl -lXp -lXext -lXt -lX11
	g77 -m32 -O2 -o $(OUTPUT) $(OBJS_BATCH) $(OBJS) $(GLIBS) \
		-lXp -lXext

#------------------------------------------------------------------#
# interactive geant
# Say "make dsinter"
# This only works on ibm00 or lin01 if dsinter is linked on lin01
# Nothing to do with Xsetlocale, which is unnecessary now, or library order.
# Funny it works on twist which also uses openmotif but not ibm.make
dsinter : $(OBJS_INTER) $(OBJS) $(OBJECTS)
#	g77 -O2 -g -o $(DSBIN)/dsinter $(LIBDIRS) $(OBJS_INTER) $(OBJS) $(GLIBS) \
#		-lcrypt -ldl -lXp -lXext -lXt -lX11
	g77 -m32 -O2 -g -o $(DSBIN)/dsinter $(OBJS_INTER) $(OBJS) $(GLIBS) \
		-lXp -lXext -lXt -lX11

#	g77  -o $(DSBIN)/dsinter $(LIBDIRS) $(OBJS_INTER) $(OBJS) $(GLIBS) -lcrypt -lXm -lXt -lX11


#------------------------------------------------------------------#
# standard rules and definitions just as found in
# include $(TRIUMF_ROOT)/make.std
#------------------------------------------------------------------#

# clear default suffix list

.SUFFIXES: .f .c .o

# define targets

library : $(OBJS)
	ar ts $(LIB)

clean :
	@rm -f $(OUTPUT) $(LIB) *.o

# define rules
include $(DSROOT)/make.std


