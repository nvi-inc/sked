#########################################################################
#                                                                       #
#                           curses for sked                             #
#                                                                       #
# Makefile for sked curses library.                                     #
# NRV updated 951101                                                    #
#                                                                       #
#########################################################################
#
#
LIB=../curses/
ARC=curses.a
#
OBJECTS=\
addstr_f.o start_mn.o save_term.o


#FC:=$(FORT)
#CC:=$(CC)

$(ARC): $(OBJECTS)
	touch $(ARC)
	rm $(ARC)
	$(ARCHIV) $(ARC) $(OBJECTS)
