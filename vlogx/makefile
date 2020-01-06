#####################################################################
#                                                                   #
#                       makefile for VLOGX                          #
#                         NRV 940413                                #
#                                                                   #
#####################################################################

PROGRAM = vlogx

#
# Set objects
#
VXOBS=\
vlogx.o parse_log1.o upper.o getfiles.o getydhms.o read_flag.o \
read_drg.o read_cal.o parse_cal_line.o

CFLAGS= -c -g 

LDFLAGS=  -lc  -lm 
$(PROGRAM): $(VXOBS) 
	rm -f ./core vlogx
	@echo Linking $(PROGRAM)...
	cc -o $(PROGRAM) \
		$(VXOBS) \
		$(LDFLAGS) ;
	@echo done.

.c.o:
	cd $(<D); gcc $(CFLAGS) $<

