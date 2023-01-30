# if JACOBI=1 use provided eigen-(vectors,values) routine
# jacobi.f90
export JACOBI=1
# the directory containing the X11 libraries
X11LIBDIR  = -L/usr/X11R6/lib
# the X11 libraries
X11LIB = -lXaw -lXt -lXmu -lXi -lXext -lX11 -lm
# the fortran 90 compiler
FC=ifort
FFLAGS= -O
# the directory containing the fortran 90 libraries for OpenGL, including GLUT and GLU
F90GLUTLIBDIR = -L/home/paulo/WORK/lib
# the f90 libraries
F90GLUTLIB = -lf90glut -lf90GLU -lf90GL -L/usr/local/nvidia/lib -lglut -lGLU -lGL 
# modules path
MODDIR = -I/home/paulo/WORK/include/GL
# mathematical libs if not using jacobi.f90
#MATH_MODULES = -I../include/LAPACK95
MATH_LIBS=-llapack95 -llapack -lf77blas -lcblas -latlas
# Linker
LOADER=ifort
# Linking flags
LDFLAGS=-static-libcxa

##############################################################################################
#       It wont be necessary to alter anything below this point (hopefully)                  #
##############################################################################################
.SUFFIXES=.f90 .f90.ps .f90.pdf .ps .pdf
%.o:%.f90
	$(FC) $(FFLAGS) $(MODDIR) $<  -c
%.f90.ps: %.f90 
	./f90tops < $< > $@
%.f90.pdf: %.f90.ps 
	ps2pdf $<
all:glSyMM 

f90tops: f90tops.f90
	ifort f90tops.f90 -static-libcxa -o f90tops

glSyMM.f90.ps: glSyMM.f90
tube.f90.ps: tube.f90
view_modifier.f90.ps: view_modifier.f90

ps: f90tops glSyMM.f90.ps tube.f90.ps view_modifier.f90.ps

glSyMM.f90.pdf: glSyMM.f90.ps
tube.f90.pdf: tube.f90.ps
view_modifier.f90.pdf: view_modifier.f90.ps

pdf:f90tops  glSyMM.f90.pdf tube.f90.pdf view_modifier.f90.pdf

glSyMM.o: glSyMM.f90
view_modifier.o: view_modifier.f90
tube.o: tube.f90


ifeq (1,${JACOBI})
eigens-jacobi.o: eigens-jacobi.f90
objects = view_modifier.o tube.o eigens-jacobi.o glSyMM.o 
MATH_LIBS=
else
eigens-lapack95.o: eigens-lapack95.f90
objects = view_modifier.o tube.o eigens-lapack95.o glSyMM.o 
endif

glSyMM: $(objects) 
	$(LOADER) $^ $(MODDIR) $(X11LIBDIR) $(F90GLUTLIBDIR) $(F90GLUTLIB) $(X11LIB) $(MATH_LIBS) $(LDFLAGS) -o $@

.PHONY: clean clean_docs clean_ps clean_pdf docs
clean: clean_docs
	rm -f *~ *.o glSyMM *.mod core work.pc* f90tops forttemp*
clean_docs: clean_ps clean_pdf
	rm -f glSyMM-calcs.dvi glSyMM-calcs.ps glSyMM-calcs.pdf glSyMM-calcs.aux glSyMM-calcs.log 
clean_ps:
	rm -f glSyMM.f90.ps tube.f90.ps view_modifier.f90.ps
clean_pdf:
	rm -f glSyMM.f90.pdf tube.f90.pdf view_modifier.f90.pdf
docs: glSyMM-calcs.pdf glSyMM-calcs.OUT pdf

glSyMM-calcs.pdf: glSyMM-calcs.ps
	ps2pdf glSyMM-calcs.ps
glSyMM-calcs.ps: glSyMM-calcs.dvi
	dvips glSyMM-calcs -o
glSyMM-calcs.dvi: glSyMM-calcs.tex
	latex glSyMM-calcs.tex

glSyMM-calcs.OUT: glSyMM-calcs.max
	maxima -l cmucl  -b glSyMM-calcs.max > glSyMM-calcs.OUT
VERSION:=0.99.0
HOJE:=$(shell date '+%d%b%Y')
TARBALL:=glSyMM-$(VERSION).tar.bz2
dist: clean
	cd ..;rm -f $(TARBALL); tar cvfj $(TARBALL) glSyMM-$(VERSION)
rpm: dist
	rpmbuild -ta ../$(TARBALL)
zip: dist
	cp ../$(TARBALL) /mnt/zip
