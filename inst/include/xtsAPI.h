/*
Header file for using internal C-level facilities
provided by xts.

This is not 100% designed for end users, so
any user comments and bug reports are very
welcomed.

Copyright 2008 - 2013 Jeffrey A. Ryan   
Copyright        2013 Dirk Eddelbuettel        

This source is distributed with the same license
as the full xts software, GPL (>= 2).
*/

#ifndef _XTS_API_H
#define _XTS_API_H

#include <xts.h>		// also includes R.h, Rinternals.h, Rdefines.h

#include <Rconfig.h>
#include <R_ext/Rdynload.h>

#ifdef HAVE_VISIBILITY_ATTRIBUTE
  # define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
  # define attribute_hidden
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
  To aid those looking for answers on interfacing compiled code from
  another package.

  This is simply constructing a function pointer for use.

  static RETURNTYPE(*fun)(ARG1,ARG2) = NULL     where ARGS are the types accepted, comma seperated
    fun = ( RETURNTYPE(*)(ARG1,ARG2)) R_GetCCallable("PACKAGENAME", "FUNCTIONNAME")

*/
int attribute_hidden xtsIs(SEXP x)
{
  static int(*fun)(SEXP) = NULL;
  if(fun == NULL)
    fun = (int(*)(SEXP)) R_GetCCallable("xts","isXts");
  return fun(x);
}

SEXP attribute_hidden xtsIsOrdered(SEXP x, SEXP increasing, SEXP strictly)
{
  static SEXP(*fun)(SEXP,SEXP,SEXP) = NULL;
  if(fun == NULL)
    fun = (SEXP(*)(SEXP,SEXP,SEXP)) R_GetCCallable("xts","do_is_ordered");
  return fun(x, increasing, strictly);
}

SEXP attribute_hidden xtsNaCheck(SEXP x, SEXP check)
{
  static SEXP(*fun)(SEXP,SEXP) = NULL;
  if(fun == NULL)
    fun = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("xts","naCheck");
  return fun(x, check);
}

SEXP attribute_hidden xtsTry(SEXP x) {
    static SEXP(*fun)(SEXP) = NULL;
    if (fun == NULL) 
	fun = (SEXP(*)(SEXP)) R_GetCCallable("xts","tryXts");
    return fun(x);
}
    
SEXP attribute_hidden xtsRbind(SEXP x, SEXP y, SEXP dup) {
    static SEXP(*fun)(SEXP, SEXP, SEXP) = NULL;
    if (fun == NULL) 
	fun = (SEXP(*)(SEXP,SEXP,SEXP)) R_GetCCallable("xts","do_rbind_xts");
    return fun(x, y, dup);
}

SEXP attribute_hidden xtsCoredata(SEXP x) {
    static SEXP(*fun)(SEXP) = NULL;
    if (fun == NULL) 
	fun = (SEXP(*)(SEXP)) R_GetCCallable("xts","coredata_xts");
    return fun(x);
}

SEXP attribute_hidden xtsLag(SEXP x, SEXP k, SEXP pad) {
    static SEXP(*fun)(SEXP,SEXP,SEXP) = NULL;
    if (fun == NULL) 
	fun = (SEXP(*)(SEXP,SEXP,SEXP)) R_GetCCallable("xts","lagXts");
    return fun(x, k, pad);
}

#ifdef __cplusplus
}
#endif

#endif /* !_XTS_API_H */
