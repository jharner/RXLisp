#ifndef RXLISP_CONVERTERS_H
#define RXLISP_CONVERTERS_H

LVAL convertToXLisp(USER_OBJECT_ robj);
USER_OBJECT_ convertToR(LVAL xobj);
void RXLisp_initConverters(void);
USER_OBJECT_ makeXLispReference(LVAL xobj, const char * const className);

#endif
