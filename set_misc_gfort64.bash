# Set compilers and libraries for linking sked 
# History
# 
# Set the compilers and linkers:

export FC="gfortran -fno-range-check -fno-underscoring -g -D READ_LINE -fdefault-integer-8 -finteger-4-integer-8"
export FC="gfortran -fno-range-check -fno-underscoring -g -D READ_LINE -fdefault-integer-8 -finteger-4-integer-8"
export LINK="gfortran"
#export FC="/opt/intel/fc/bin/ifort -I skdrincl -c -fpp -nus -static -g -D READ_LINE"
#export LINK="/opt/intel/fc/bin/ifort -D READ_LINE"
export CC="gcc -c -g -I/usr/include"
#
#  Following are for linux. Uncomment if appropriate.
export SKED_HEAD="sked_lnx.o"
# This is for 64 bit version of sked. 
#export VEX_LIB="../vex/vex64.a"
#Don't need separate versions. just need to remake vex.a
export VEX_LIB="../vex/vex.a"
export ARCHIV="ar -sqc"
#
#if are using READ_LINE, then use read_cmdline_new.o
export READ_CMDLINE="read_cmdline.o"
#export READ_CMDLINE read_cmdline_new.o 
#
# PATHS to various libraries
#
#->export ATLAS_LIB /opt/lib/libatlas.a  
#->export BLAS_LIB  /opt/lib/libf77blas.a 
#->export CURSES_LIB /usr/lib/libncurses.a
export CURSES_LIB="-lncurses"
export FLEX_LIB="-lfl"
export READLINE_LIB="-lreadline"
export READLINE_LIB=""
#
# If you want to link to the mysql, include the following (or something similar)
#
export MYSQL_LIB="/lib64/libmysqlclient.so.18"
export MYSQL_INT="mysql_int.o"
#
# If you don't want to link uncomment the following
#
#export MYSQL_LIB="" 
#export MYSQL_INT="mysql_stub.o"

