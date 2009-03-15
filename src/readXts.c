#include <R.h>
#include <Rinternals.h>

/*
  xts objects are to able to be stored in a native format
  to facilitate loading and on-disk retrieval ops that
  mimic traditional DB operations.

  The current (Version 1) on disk structure
  is a header block, followed by a C struct.

  VERSION: 4 byte integer
  STRUCTURE_LEN: 4 byte integer
  STRUCTURE_TYPES: (currently using R mapping e.g. 13 = INTEGER)
  NROWS: 4 byte integer (long?)
  STRUCTs:

*/

#define RAWTO(X, MODE) \
        PROTECT(t = s = allocList(4)); \
        SET_TYPEOF(s, LANGSXP); \
        SETCAR(t, install("readBin")); t = CDR(t); \
        SETCAR(t, X); t = CDR(t); \
        PROTECT(what = allocVector(MODE, 0)); \
        SETCAR(t, what);  t = CDR(t); \
        UNPROTECT(1); \
        PROTECT(n = allocVector(INTSXP,1)); \
        INTEGER(n)[0] = length(X); \
        SETCAR(t, n); \
        UNPROTECT(1); \
        PROTECT(X = eval(s, R_GlobalEnv)); \
       
SEXP readXts (SEXP x, SEXP indextype, SEXP datatype, SEXP indexCLASS)
{
  SEXP index;
  SEXP s, t, n, what;
  int i, j, r=0;
  int lenx = length(x), incr;
  int index_mode, data_mode;
  int index_size, data_size;

  switch(TYPEOF(indextype)) {
    case INTSXP:
      index_size = sizeof(INTEGER(indextype));
      break;
    case REALSXP:
      index_size = sizeof(REAL(indextype));
      break;
    default:
      error("unsupported index type");
  }
  switch(TYPEOF(datatype)) {
    case INTSXP:
      data_size = sizeof(INTEGER(datatype)[0]);
      break;
    case REALSXP:
      data_size = sizeof(REAL(datatype)[0]);
      break;
    default:
      error("unsupported data type");
  }

  incr = (index_size + data_size);

/*Rprintf("lenx: %d, index_size: %d, data_size: %d, incr: %d\n", lenx, index_size, data_size, incr);*/
  PROTECT(index = allocVector(RAWSXP, (int)(lenx/incr)*index_size));
/*Rprintf("length(index): %d\n", length(index));*/

  SEXP data;
  PROTECT(data = allocVector(RAWSXP, (int)(lenx/incr)*data_size));

/*
  for(i in length of datatype list)
    identify size of current data type i
    alloc tmp var for bytes
    make pointer
    data assign to raw_data
    non-data (index/ID) assign to list variable
    next datatype
*/
  
  unsigned char *raw_x=NULL, *raw_index=NULL;
  raw_index = RAW(index);
  raw_x   = RAW(x);
  unsigned char *raw_data=NULL;
  raw_data = RAW(data);

  /* copy index */
  for(i=0; i < lenx; i=i+incr) {
    for(j=0; j < index_size; j++, r++) {
      raw_index[r] = raw_x[i+j];
    }
  }
  RAWTO(index, TYPEOF(indextype));
  /* copy data */
  r = 0;
  for(i=index_size; i < lenx; i=i+incr) {
    for(j=0; j < data_size; j++, r++) {
      raw_data[r] = raw_x[i+j];
    }
  }

  RAWTO(data, TYPEOF(datatype));

  setAttrib(data, install("index"), index);
  SEXP klass;
  PROTECT(klass = allocVector(STRSXP, 2));
  SET_STRING_ELT(klass, 0, mkChar("xts"));
  SET_STRING_ELT(klass, 1, mkChar("zoo"));
  setAttrib(data, install("class"), klass);
  setAttrib(data, install(".indexCLASS"), indexCLASS);
  UNPROTECT(7);
  return(data);
}
