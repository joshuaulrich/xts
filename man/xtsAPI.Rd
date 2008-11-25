\name{xtsAPI}
\alias{xtsAPI}
\title{ xts C API Documentation }
\description{
This help file is to help in development of xts, as well
as provide some clarity and insight into its purpose and implementation.

Last modified: 2008-10-22 by Jeffrey A. Ryan
Version: 0.5-0 and above

At present the \pkg{xts} API has publically available
interfaces to the following:

See xts.h and the code in /src for details.

\preformatted{
Internal use functions:
  int isXts(SEXP x)
  void copy_xtsAttributes(SEXP x, SEXP y)
  void copy_xtsCoreAttributes(SEXP x, SEXP y)

Internal use macros:
  xts_ATTRIB(x)
  xts_COREATTRIB(x)
  GET_xtsIndex(x)
  SET_xtsIndex(x,value)
  GET_xtsIndexClass(x)
  SET_xtsIndexClass(x,value)
  GET_xtsIndexFormat(x)
  SET_xtsIndexFormat(x,value)
  GET_xtsCLASS(x)
  SET_xtsCLASS(x,value)

Internal use SYMBOLS:
  xts_IndexSymbol
  xts_ClassSymbol
  xts_IndexFormatSymbol
  xts_IndexClassSymbol

Callable from R:
  SEXP mergeXts(SEXP args)
  SEXP rbindXts(SEXP args)
  SEXP tryXts(SEXP x)
}
}
\author{ Jeffrey A. Ryan }
\keyword{ utilities }
