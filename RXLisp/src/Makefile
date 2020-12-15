ifdef ADMIN
 include Install/GNUmakefile.admin
endif

include Makevars

# include $(R_HOME)/etc/Makeconf

ifndef R
 R=R
endif

ifndef SHLIB_EXT
 SHLIB_EXT=.so
endif

SRC=RXLisp.c Converters.c
OBJS=$(SRC:%.c=%.o)

all: RXLisp.so 

RXLisp.so: RXLisp.c Converters.c
	(PKG_LIBS="-L. -l$(CONVERTER_LIB_NAME) -L${XLISP_SRC_DIR} -lxlisp" ; export PKG_LIBS ; $(R) CMD SHLIB -o $@ RXLisp.c Converters.c)

clean:
	-rm *.o
	-rm RXLisp.so 
