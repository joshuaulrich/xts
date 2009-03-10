#include <R.h>
#include <Rinternals.h>

#define rawTo(X, MODE) \
        PROTECT(t = s = allocList(4)); \
        SET_TYPEOF(s, LANGSXP); \
        SETCAR(t, install("readBin")); t = CDR(t); \
        SETCAR(t, X); t = CDR(t); \
        PROTECT(what = allocVector(MODE, 0)); \
        SETCAR(t, what);  t = CDR(t); \
        PROTECT(n = allocVector(INTSXP,1)); \
        INTEGER(n)[0] = length(index); \
        SETCAR(t, n); \
        PROTECT(X = eval(s, R_GlobalEnv)); \
        
        
       
SEXP readXts (SEXP x, SEXP indexCLASS)
{
  SEXP index;
  SEXP s, t, n, what;
  int i, j, r=0;
  int lenx = length(x);

  PROTECT(index = allocVector(RAWSXP, (int)(length(x)/3)));
  
  unsigned char *rawx=NULL, *rawindex=NULL;
  rawindex = RAW(index);
  rawx   = RAW(x);

  for(i=0; i < lenx; i=i+12) {
    for(j=0; j < 4; j++, r++) {
      rawindex[r] = rawx[i+j];
    }
  }
  rawTo(index, INTSXP);

  SEXP data;
  PROTECT(data = allocVector(RAWSXP, (int)((length(x)/3)*2)));
  unsigned char *rawdata=NULL;
  rawdata = RAW(data);

  r = 0;
  for(i=4; i < lenx; i=i+12) {
    for(j=0; j < 8; j++, r++) {
      rawdata[r] = rawx[i+j];
    }
  }
  rawTo(data, REALSXP);

  setAttrib(data, install("index"), index);
  SEXP klass;
  PROTECT(klass = allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, mkChar("xts"));
  SET_STRING_ELT(klass, 1, mkChar("zoo"));
  setAttrib(data, install("class"), klass);
  setAttrib(data, install(".indexCLASS"), indexCLASS);
  UNPROTECT(11);
  return(data);
}
