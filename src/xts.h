/*
Header file for using internal C-level facilities
provided by xts.

This is not 100% designed for end users, so
any user comments and bug reports are very
welcomed.

Copyright Jeffrey A. Ryan 2008

This source is distributed with the same license
as the full xts software, GPL3.
*/

#ifndef _XTS
#define _XTS


/*
INTERNAL SYMBOLS
*/
#define  xts_IndexSymbol        install("index")
#define  xts_ClassSymbol        install(".CLASS")
#define  xts_IndexFormatSymbol  install(".indexFORMAT")
#define  xts_IndexClassSymbol   install(".indexCLASS")


/*
DATA TOOLS
*/
#define  xts_ATTRIB(x)          coerceVector(do_xtsAttributes(x),LISTSXP)

/*
FUNCTIONS
*/
SEXP do_xtsAttributes(SEXP x);              // xtsAttributes
void copy_xtsAttributes(SEXP x, SEXP y);    

#endif /* _XTS */
