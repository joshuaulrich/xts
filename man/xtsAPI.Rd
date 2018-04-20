\name{xtsAPI}
\alias{xtsAPI}
\title{ xts C API Documentation }
\description{
This help file is to help in development of xts, as well
as provide some clarity and insight into its purpose and implementation.

By Jeffrey A. Ryan, Dirk Eddelbuettel, and Joshua M. Ulrich
Last modified: 2018-05-02
Version: 0.10-3 and above

At present the \pkg{xts} API has publicly available
interfaces to the following functions (as defined in \code{xtsAPI.h}):

\preformatted{
Callable from other R packages:
  SEXP xtsIsOrdered(SEXP x, SEXP increasing, SEXP strictly)
  SEXP xtsNaCheck(SEXP x, SEXP check)
  SEXP xtsTry(SEXP x)
  SEXP xtsRbind(SEXP x, SEXP y, SEXP dup)
  SEXP xtsCoredata(SEXP x)
  SEXP xtsLag(SEXP x, SEXP k, SEXP pad)

Internal use functions:
  int isXts(SEXP x)
  void copy_xtsAttributes(SEXP x, SEXP y)
  void copy_xtsCoreAttributes(SEXP x, SEXP y)

Internal use macros:
  xts_ATTRIB(x)
  xts_COREATTRIB(x)
  GET_xtsIndex(x)
  SET_xtsIndex(x,value)
  GET_xtsIndexFormat(x)
  SET_xtsIndexFormat(x,value)
  GET_xtsCLASS(x)
  SET_xtsCLASS(x,value)

Internal use SYMBOLS:
  xts_IndexSymbol
  xts_ClassSymbol
  xts_IndexFormatSymbol

Callable from R:
  SEXP mergeXts(SEXP args)
  SEXP rbindXts(SEXP args)
  SEXP tryXts(SEXP x)
}
}
\examples{
\dontrun{
# some example code to look at

file.show(system.file('api_example/README', package="xts"))
file.show(system.file('api_example/src/checkOrder.c', package="xts"))
}
}
\author{ Jeffrey A. Ryan }
\keyword{ utilities }
