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

SRC=RXLisp.c
OBJS=$(SRC:%.c=%.o)

CONVERTER_LIB_NAME=RXLispConverters
CONVERTER_LIB=lib$(CONVERTER_LIB_NAME)$(SHLIB_EXT)

all: RXLisp.so RinXLisp.so

RXLisp.so: RXLisp.c $(CONVERTER_LIB)
	(PKG_LIBS="-L. -l$(CONVERTER_LIB_NAME) -L${XLISP_SRC_DIR} -lxlisp" ; export PKG_LIBS ; $(R) CMD SHLIB -o $@ RXLisp.c)

RinXLisp.so: RinXLisp.c $(CONVERTER_LIB)
	(PKG_LIBS="-L. -l$(CONVERTER_LIB_NAME)" ; export PKG_LIBS ; $(R) CMD SHLIB -o $@ RinXLisp.c )

$(CONVERTER_LIB): Converters.c
	$(R) CMD SHLIB -o $@ Converters.o

clean:
	-rm *.o
	-rm RXlisp.so RinXLisp.so $(CONVERTER_LIB)
