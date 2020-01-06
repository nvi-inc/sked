# Makefile for lnfch for linux
#
ARC = lnfch.a
#
OBJECTS=\
pchar-linux.o jchar-linux.o char2hol.o hol2char.o cjchar.o \
ib2as.o ias2b.o ir2as.o das2b_new.o  \
ichmv.o ichmv_ch.o ichcm_ch.o  ichcm.o ifill.o ifill_ch.o  \
gtfld.o  gtrsp.o iflch.o mcoma.o iscnc.o iscns.o iscn_ch.o

#FC:=$(FORT)
#CC:=$(CC)

$(ARC): $(OBJECTS)
	touch $(ARC)
	rm $(ARC)
	$(ARCHIV) $(ARC) $(OBJECTS)

