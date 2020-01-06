#
#  Makefile for dblas, dppfa, dppin
#  These are solve routines used in a modified form by sked. 
#
#
LIB=../matrix/
ARC=matrix.a
OBJECTS = \
indx4.o dasum.o daxpy.o ddot.o dscal.o dppin.o dppfa.o dppdi.o dppco.o


#FC:=$(FORT)
#CC:=$(CC)

$(ARC): $(OBJECTS)
	touch $(ARC)
	rm $(ARC)
	$(ARCHIV) $(ARC) $(OBJECTS)

