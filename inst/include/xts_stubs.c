#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif

/*
  To aid those looking for answers on interfacing compiled code
  from another package.

  This is simply constructing a function pointer for use.

  static RETURNTYPE(*fun)(ARG1,ARG2) = NULL     where ARGS are the types accepted, comma seperated
    fun = ( RETURNTYPE(*)(ARG1,ARG2)) R_GetCCallable("PACKAGENAME", "FUNCTIONNAME")

*/
int attribute_hidden isXts(SEXP x)
{
  static int(*fun)(SEXP) = NULL;
  if(fun == NULL)
    fun = (int(*)(SEXP)) R_GetCCallable("xts","isXts");
  return fun(x);
}

SEXP attribute_hidden do_is_ordered(SEXP x, SEXP increasing, SEXP strictly)
{
  static SEXP(*fun)(SEXP,SEXP,SEXP) = NULL;
  if(fun == NULL)
    fun = (SEXP(*)(SEXP,SEXP,SEXP)) R_GetCCallable("xts","do_is_ordered");
  return fun(x, increasing, strictly);
}

SEXP attribute_hidden naCheck(SEXP x, SEXP check)
{
  static SEXP(*fun)(SEXP,SEXP) = NULL;
  if(fun == NULL)
    fun = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("xts","naCheck");
  return fun(x, check);
}

SEXP attribute_hidden tryXts(SEXP x) {
    static SEXP(*fun)(SEXP) = NULL;
    if (fun == NULL) 
	fun = (SEXP(*)(SEXP)) R_GetCCallable("xts","tryXts");
    return fun(x);
}
    
SEXP attribute_hidden do_rbind_xts(SEXP x, SEXP y, SEXP dup) {
    static SEXP(*fun)(SEXP, SEXP, SEXP) = NULL;
    if (fun == NULL) 
	fun = (SEXP(*)(SEXP,SEXP,SEXP)) R_GetCCallable("xts","do_rbind_xts");
    return fun(x, y, dup);
}

SEXP attribute_hidden coredata_xts(SEXP x) {
    static SEXP(*fun)(SEXP) = NULL;
    if (fun == NULL) 
	fun = (SEXP(*)(SEXP)) R_GetCCallable("xts","coredata_xts");
    return fun(x);
}

SEXP attribute_hidden lagXts(SEXP x, SEXP k, SEXP pad) {
    static SEXP(*fun)(SEXP,SEXP,SEXP) = NULL;
    if (fun == NULL) 
	fun = (SEXP(*)(SEXP,SEXP,SEXP)) R_GetCCallable("xts","lagXts");
    return fun(x, k, pad);
}
