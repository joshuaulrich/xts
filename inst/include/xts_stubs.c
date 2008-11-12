#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif

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
