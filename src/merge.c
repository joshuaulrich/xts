/*
#   xts: eXtensible time-series 
#
#   Copyright (C) 2008  Jeffrey A. Ryan jeff.a.ryan @ gmail.com
#
#   Contributions from Joshua M. Ulrich
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/


#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "xts.h"

/* 

  This is a merge_join algorithm used to
  allow two xts objects to be merged as one
  along a common index efficiently and fast

  The code is branched for REAL and INTEGER indexed values
  which allows for efficient memory usage and minimal
  testing/coercion

  
  Copyright Jeffrey A. Ryan 2008

*/
/* do_merge_xts {{{ */
SEXP do_merge_xts (SEXP x, SEXP y,
                   SEXP all,
                   SEXP fill,
                   SEXP retclass,
                   SEXP colnames, 
                   SEXP suffixes,
                   SEXP retside,
                   SEXP env,
                   int coerce)
{
  int nrx, ncx, nry, ncy, len;
  int left_join, right_join;
  int i = 0, j = 0, xp = 1, yp = 1; /* x and y positions in index */
  int mode;
  int ij_original, ij_result;
  int p = 0;
  SEXP xindex, yindex, index, result, attr, len_xindex;
  SEXP s, t, unique;

  int *int_result=NULL, *int_x=NULL, *int_y=NULL, int_fill=0;
  int *int_index=NULL, *int_xindex=NULL, *int_yindex=NULL;
  double *real_result=NULL, *real_x=NULL, *real_y=NULL;
  double *real_index=NULL, *real_xindex=NULL, *real_yindex=NULL;

  /* we do not check that 'x' is an xts object.  Dispatch and mergeXts
    (should) make this unecessary.  So we just get the index value 

    This assumption seems to be invalid when dispatched from cbind.xts
    So we need to check that the objects are not NULL, or at least
    treat NULL objects as zero-width with an index that matches the non-null
   
    2009/01/07: calling merge(NA,x) or merge(1,1,xts) causes a segfault;
                calling merge(1,x) causes the xts-info (none!) from the 1st arg
                to be used, resulting in a classless object. [fixed - jar]
  */
  if( isNull(x) || isNull(y) ) {
    if(!isNull(x)) return(x);
    return(y);
  }

  PROTECT( xindex = getAttrib(x, install("index")) );

  /* convert to xts object if needed */
  if( !isXts(y) ) {
    PROTECT(s = t = allocList(4)); p++;
    SET_TYPEOF(s, LANGSXP);
    SETCAR(t, install("try.xts")); t = CDR(t);
    SETCAR(t, y); t = CDR(t);
    PROTECT( len_xindex = allocVector(INTSXP, 1)); p++;
    INTEGER(len_xindex)[0] = length(xindex);
    SETCAR(t, len_xindex);
    SET_TAG(t, install("length.out")); t = CDR(t);
    SETCAR(t, install(".merge.xts.scalar"));
    SET_TAG(t, install("error"));
    PROTECT(y = eval(s, env)); p++;
  } /* end conversion process */

  mode = TYPEOF(x);

  if( isXts(y) ) {
    PROTECT( yindex = getAttrib(y, xts_IndexSymbol) );
  } else {
    PROTECT( yindex = getAttrib(x, xts_IndexSymbol) );
  }

  if( TYPEOF(retside) != LGLSXP )
    error("retside must be a logical value of TRUE or FALSE");

  nrx = nrows(x);
  ncx = ncols(x);
  /* if object is zero-width */
  if( LENGTH(x)==0 || INTEGER(retside)[0]==0 ) {
    nrx = nrows(xindex);
    ncx = 0;
  }
  
  nry = nrows(y);
  ncy = ncols(y);
  /* if object is zero-width */
  if( LENGTH(y)==0 || INTEGER(retside)[1]==0) {
    nry = nrows(yindex);
    ncy = 0;
  }

  len = nrx + nry;

  /* at present we are failing the call if the indexing is of
     mixed type.  This should probably instead simply coerce
     to REAL so as not to lose any information (at the expense
     of conversion cost and memory), and issue a warning. */
  if( TYPEOF(xindex) != TYPEOF(yindex) )
  {
    PROTECT(xindex = coerceVector(xindex, REALSXP)); p++;
    PROTECT(yindex = coerceVector(yindex, REALSXP)); p++;
  }

  if( TYPEOF(all) != LGLSXP )
    error("all must be a logical value of TRUE or FALSE");

  left_join = INTEGER(all)[ 0 ];
  right_join = INTEGER(all)[ 1 ];

  /* determine num_rows of final merged xts object
     
     this seems to only cost 1/1000 of a sec per
     1e6 observations.  Acceptable 'waste' given
     that now we can properly allocate space
     for our results

     We also check the index type and use the appropriate macros
   */
  
  if( TYPEOF(xindex) == REALSXP ) { 
  real_xindex = REAL(xindex);
  real_yindex = REAL(yindex);
  while( (xp + yp) <= (len + 1) ) {
    if( xp > nrx ) {
      yp++;
      if(right_join) i++;
    } else
    if( yp > nry ) {
      xp++;
      if(left_join) i++;
    } else
    if( real_xindex[ xp-1 ] == real_yindex[ yp-1 ] ) {
      /* INNER JOIN  --- only result if all=FALSE */
      yp++;
      xp++;
      i++;
    } else
    if( real_xindex[ xp-1 ] < real_yindex[ yp-1 ] ) {
      /* LEFT JOIN */
      xp++;
      if(left_join) i++;
    } else
    if( real_xindex[ xp-1 ] > real_yindex[ yp-1 ] ) {
      /* RIGHT JOIN */
      yp++;
      if(right_join) i++;
    } else
    if(ISNA(real_xindex[ xp-1 ]) || ISNA(real_yindex[ yp-1 ])) {
Rprintf("%f, %f\n",real_xindex[xp-1],real_yindex[yp-1]);
      error("'NA' not allowed in 'index'");
    }
  } 
  } else
  if( TYPEOF(xindex) == INTSXP ) {
  int_xindex = INTEGER(xindex);
  int_yindex = INTEGER(yindex);
  while( (xp + yp) <= (len + 1) ) {
    if( xp > nrx ) {
      yp++;
      if(right_join) i++;
    } else
    if( yp > nry ) {
      xp++;
      if(left_join) i++;
    } else
    if( int_xindex[ xp-1 ] == int_yindex[ yp-1 ] ) {
      yp++;
      xp++;
      i++;
    } else
    if( int_xindex[ xp-1 ] < int_yindex[ yp-1 ] ) {
      xp++;
      if(left_join) i++;
    } else
    if( int_xindex[ xp-1 ] > int_yindex[ yp-1 ] ) {
      yp++;
      if(right_join) i++;
    } else
    if(real_xindex[ xp-1 ]==NA_INTEGER ||
       real_yindex[ yp-1 ]==NA_INTEGER) {
       error("'NA' not allowed in 'index'");
    }
  } 
  }

  if(i == 0) {
    /* if no rows match, return an empty xts object, similar in style to zoo */
    PROTECT( result = allocVector(TYPEOF(x), 0) ); p++;
    PROTECT( index  = allocVector(TYPEOF(xindex), 0) ); p++;
    SET_xtsIndex(result, index);
    if(LOGICAL(retclass)[0])
      setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
    UNPROTECT(2 + p);
    return result;
  }

  int num_rows = i;
  xp = 1; yp = 1;

  PROTECT( index  = allocVector(TYPEOF(xindex), num_rows) );
  /* coercion/matching of TYPE for x and y needs to be checked,
     either here or in the calling R code.  I suspect here is
     more useful if other function can call the C code as well. 
     If objects are not the same type, convert to REALSXP. */
  if( coerce || TYPEOF(x) != TYPEOF(y) ) {
    PROTECT( x = coerceVector(x, REALSXP) ); p++;
    PROTECT( y = coerceVector(y, REALSXP) ); p++;
  }
  PROTECT( result = allocVector(TYPEOF(x), (ncx + ncy) * num_rows) );

  if( TYPEOF(fill) != TYPEOF(x) ) {
    PROTECT( fill = coerceVector(fill, TYPEOF(x)) ); p++;
  } 

  mode = TYPEOF(x);

  /* use pointers instead of function calls */
  switch(TYPEOF(x)) {
    case INTSXP:
        int_x = INTEGER(x);
        int_y = INTEGER(y);
        int_fill = INTEGER(fill)[0];
        int_result = INTEGER(result);
        break;
    case REALSXP:
        real_x = REAL(x);
        real_y = REAL(y);
        /*real_fill = REAL(fill)[0];*/
        real_result = REAL(result);
        break;
    default:
        break;
  }

  switch(TYPEOF(xindex)) {
    case INTSXP:
        int_index = INTEGER(index);
        break;
    case REALSXP:
        real_index = REAL(index);
        break;
    default:
        break;
  }

  /* There are two type of supported index types, each branched from here */
  if( TYPEOF(xindex) == REALSXP ) {

  /* REAL INDEXING */
  for(i = 0; i < num_rows; i++) {
    /* If we are past the last row in x, assign NA to merged data 
       and copy the y column values to the second side of result
    */
    if( xp > nrx ) {
      if(right_join) {
        real_index[ i ] = real_yindex[ yp-1 ];
        for(j = 0; j < ncx; j++) { /* x-values */
          ij_result = i + j * num_rows;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(fill)[ 0 ];
              break;
            case INTSXP:
              /*INTEGER(result)[ ij_result ] = INTEGER(fill)[ 0 ];*/
              int_result[ ij_result ] = int_fill;
              break;
            case REALSXP:
              REAL(result)[ ij_result ] = REAL(fill)[ 0 ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ].r = REAL(fill)[ 0 ];
              COMPLEX(result)[ ij_result ].i = REAL(fill)[ 0 ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(fill, 0));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
        for(j = 0; j < ncy; j++) { /* y-values */
          ij_result = i + (j+ncx) * num_rows;
          ij_original = (yp-1) + j * nry;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(y)[ ij_original ];
              break;
            case INTSXP:
              int_result[ ij_result ] = int_y[ ij_original ];
              break;
            case REALSXP:
              real_result[ ij_result ] = real_y[ ij_original ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ] = COMPLEX(y)[ ij_original ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(y, ij_original));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
      }
      yp++;
      if(!right_join) i--;  /* if all=FALSE, we must decrement i for each non-match */
    } else

    /* past the last row of y */
    if( yp > nry ) {
      if(left_join) {

        /* record new index value */
        real_index[ i ] = real_xindex[ xp-1 ];

        /* copy values from x and y to result */
        for(j = 0; j < ncx; j++) { /* x-values */
          ij_result = i + j * num_rows;
          ij_original = (xp-1) + j * nrx; 
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(x)[ ij_original ];
              break;
            case INTSXP:
              int_result[ ij_result ] = int_x[ ij_original ];
              break;
            case REALSXP:
              real_result[ ij_result ] = real_x[ ij_original ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ] = COMPLEX(x)[ ij_original ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(x, ij_original));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }

        /* we are out of y-values, so fill merged result with NAs */
        for(j = 0; j < ncy; j++) { /* y-values */
          ij_result = i + (j+ncx) * num_rows;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(fill)[ 0 ];
              break;
            case INTSXP:
              /*INTEGER(result)[ ij_result ] = INTEGER(fill)[ 0 ];*/
              int_result[ ij_result ] = int_fill;
              break;
            case REALSXP:
              REAL(result)[ ij_result ] = REAL(fill)[ 0 ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ].r = REAL(fill)[ 0 ];
              COMPLEX(result)[ ij_result ].i = REAL(fill)[ 0 ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(fill, 0));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
      }
      xp++;
      if(!left_join) i--;
    } else

    /* matching index values copy all column values from x and y to results */
    if( real_xindex[ xp-1 ] == real_yindex[ yp-1 ] ) {
      real_index[ i ] = real_xindex[ xp-1 ];
      /* copy x-values to result */
      for(j = 0; j < ncx; j++) { /* x-values */
        ij_result = i + j * num_rows;
        ij_original = (xp-1) + j * nrx;
        switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(x)[ ij_original ];
              break;
            case INTSXP:
              int_result[ ij_result ] = int_x[ ij_original ];
              break;
            case REALSXP:
              real_result[ ij_result ] = real_x[ ij_original ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ] = COMPLEX(x)[ ij_original ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(x, ij_original));
              break;
            default:
              error("unsupported data type");
              break;
          }
      }

      /* copy y-values to result */
      for(j = 0; j < ncy; j++) { /* y-values */
        ij_result = i + (j+ncx) * num_rows;
        ij_original = (yp-1) + j * nry;
        switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(y)[ ij_original ];
              break;
            case INTSXP:
              int_result[ ij_result ] = int_y[ ij_original ];
              break;
            case REALSXP:
              real_result[ ij_result ] = real_y[ ij_original ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ] = COMPLEX(y)[ ij_original ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(y, ij_original));
              break;
            default:
              error("unsupported data type");
              break;
          }
      }
      xp++;
      yp++;
    } else

    if( real_xindex[ xp-1 ] < real_yindex[ yp-1 ] ) {
      if(left_join) {
        real_index[ i ] = real_xindex[ xp-1 ];
        for(j = 0; j < ncx; j++) { /* x-values */
          ij_result = i + j * num_rows;
          ij_original = (xp-1) + j * nrx;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(x)[ ij_original ];
              break;
            case INTSXP:
              int_result[ ij_result ] = int_x[ ij_original ];
              break;
            case REALSXP:
              real_result[ ij_result ] = real_x[ ij_original ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ] = COMPLEX(x)[ ij_original ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(x, ij_original));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
        for(j = 0; j < ncy; j++) { /* y-values */
          ij_result = i + (j+ncx) * num_rows;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(fill)[ 0 ]; 
              break;
            case INTSXP:
              /*INTEGER(result)[ ij_result ] = INTEGER(fill)[ 0 ]; */
              int_result[ ij_result ] = int_fill;
              break;
            case REALSXP:
              REAL(result)[ ij_result ] = REAL(fill)[ 0 ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ].r = REAL(fill)[ 0 ];
              COMPLEX(result)[ ij_result ].i = REAL(fill)[ 0 ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(fill, 0));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
      }
      xp++;
      if(!left_join) i--;
    } else

    if( real_xindex[ xp-1 ] > real_yindex[ yp-1 ] ) {
      if(right_join) {
        real_index[ i ] = real_yindex[ yp-1 ];
        for(j = 0; j < ncx; j++) { /* x-values */
          ij_result = i + j * num_rows;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(fill)[ 0 ];
              break;
            case INTSXP:
              /*INTEGER(result)[ ij_result ] = INTEGER(fill)[ 0 ];*/
              int_result[ ij_result ] = int_fill;
              break;
            case REALSXP:
              REAL(result)[ ij_result ] = REAL(fill)[ 0 ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ].r = REAL(fill)[ 0 ];
              COMPLEX(result)[ ij_result ].i = REAL(fill)[ 0 ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(fill, 0));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
        for(j = 0; j < ncy; j++) { /* y-values */
          ij_result = i + (j+ncx) * num_rows;
          ij_original = (yp-1) + j * nry;
          switch( mode ) {
              case LGLSXP:
                LOGICAL(result)[ ij_result ] = LOGICAL(y)[ ij_original ];
                break;
              case INTSXP:
                int_result[ ij_result ] = int_y[ ij_original ];
                break;
              case REALSXP:
                real_result[ ij_result ] = real_y[ ij_original ];
                break;
              case CPLXSXP:
                COMPLEX(result)[ ij_result ] = COMPLEX(y)[ ij_original ];
                break;
              case STRSXP:
                SET_STRING_ELT(result, ij_result, STRING_ELT(y, ij_original));
                break;
              default:
                error("unsupported data type");
                break;
          }
        }
      }
      yp++;
      if(!right_join) i--;
    }
  }

  } else
  if( TYPEOF(xindex) == INTSXP ) {
  for(i = 0; i < num_rows; i++) {
    /* If we are past the last row in x, assign NA to merged data 
       and copy the y column values to the second side of result
    */
    if( xp > nrx ) {
      if(right_join) {
        int_index[ i ] = int_yindex[ yp-1 ];
        for(j = 0; j < ncx; j++) { /* x-values */
          ij_result = i + j * num_rows;
          switch( mode ) {
            case LGLSXP:
            case INTSXP:
              /*INTEGER(result)[ ij_result ] = INTEGER(fill)[ 0 ];*/
              int_result[ ij_result ] = int_fill;
              break;
            case REALSXP:
              REAL(result)[ ij_result ] = REAL(fill)[ 0 ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ].r = REAL(fill)[ 0 ];
              COMPLEX(result)[ ij_result ].i = REAL(fill)[ 0 ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(fill, 0));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
        for(j = 0; j < ncy; j++) { /* y-values */
          ij_result = i + (j+ncx) * num_rows;
          ij_original = (yp-1) + j * nry;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(y)[ ij_original ];
              break;
            case INTSXP:
              int_result[ ij_result ] = int_y[ ij_original ];
              break;
            case REALSXP:
              real_result[ ij_result ] = real_y[ ij_original ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ] = COMPLEX(y)[ ij_original ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(y, ij_original));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
      }
      yp++;
      if(!right_join) i--;  /* if all=FALSE, we must decrement i for each non-match */
    } else

    /* past the last row of y */
    if( yp > nry ) {
      if(left_join) {

        /* record new index value */
        int_index[ i ] = int_xindex[ xp-1 ];

        /* copy values from x and y to result */
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          ij_original = (xp-1) + j * nrx; //num_rows;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(x)[ ij_original ];
              break;
            case INTSXP:
              int_result[ ij_result ] = int_x[ ij_original];
              //INTEGER(result)[ ij_result ] = INTEGER(x)[ ij_original ];
              break;
            case REALSXP:
              //REAL(result)[ ij_result ] = REAL(x)[ ij_original ];
              real_result[ ij_result ] = real_x[ ij_original ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ] = COMPLEX(x)[ ij_original ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(x, ij_original));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }

        /* we are out of y-values, so fill merged result with NAs */
        for(j = 0; j < ncy; j++) { // y-values
          ij_result = i + (j+ncx) * num_rows;
          //REAL(result)[ ij_result ] = NA_REAL;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(fill)[ 0 ]; //NA_INTEGER;
              break;
            case INTSXP:
              int_result[ ij_result ] = int_fill;
              break;
            case REALSXP:
              REAL(result)[ ij_result ] = REAL(fill)[ 0 ]; //NA_REAL;
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ].r = REAL(fill)[ 0 ]; //NA_REAL;
              COMPLEX(result)[ ij_result ].i = REAL(fill)[ 0 ]; //NA_REAL;
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(fill, 0)); //NA_STRING);
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
      }
      xp++;
      if(!left_join) i--;
    } else

    /* matching index values copy all column values from x and y to results */
    //if( INTEGER(xindex)[ xp-1 ] == INTEGER(yindex)[ yp-1 ] ) {
    if( int_xindex[ xp-1 ] == int_yindex[ yp-1 ] ) {

      /* copy index FIXME this needs to handle INTEGER efficiently as well*/
      //INTEGER(index)[ i ] = INTEGER(xindex)[ xp-1 ]; 
      int_index[ i ] = int_xindex[ xp-1 ];

      /* copy x-values to result */
      for(j = 0; j < ncx; j++) { // x-values
        ij_result = i + j * num_rows;
        ij_original = (xp-1) + j * nrx; //num_rows;
        //REAL(result)[ ij_result ] = REAL(x)[ ij_original ];
        switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(x)[ ij_original ];
              break;
            case INTSXP:
              int_result[ ij_result ] = int_x[ ij_original ];
              //INTEGER(result)[ ij_result ] = INTEGER(x)[ ij_original ];
              break;
            case REALSXP:
              //REAL(result)[ ij_result ] = REAL(x)[ ij_original ];
              real_result[ ij_result ] = real_x[ ij_original ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ] = COMPLEX(x)[ ij_original ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(x, ij_original));
              break;
            default:
              error("unsupported data type");
              break;
          }
      }

      /* copy y-values to result */
      for(j = 0; j < ncy; j++) { // y-values
        ij_result = i + (j+ncx) * num_rows;
        ij_original = (yp-1) + j * nry; //num_rows;
        //REAL(result)[ ij_result ] = REAL(y)[ ij_original ];
        switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(y)[ ij_original ];
              break;
            case INTSXP:
              int_result[ ij_result ] = int_y[ ij_original ];
              //INTEGER(result)[ ij_result ] = INTEGER(y)[ ij_original ];
              break;
            case REALSXP:
              //REAL(result)[ ij_result ] = REAL(y)[ ij_original ];
              real_result[ ij_result ] = real_y[ ij_original ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ] = COMPLEX(y)[ ij_original ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(y, ij_original));
              break;
            default:
              error("unsupported data type");
              break;
          }
      }
      xp++;
      yp++;
    } else

    //if( INTEGER(xindex)[ xp-1 ]  < INTEGER(yindex)[ yp-1 ] ) {
    if( int_xindex[ xp-1 ] < int_yindex[ yp-1 ] ) {
      if(left_join) {
        //copyIndex(index, xindex, i, xp-1);
        //INTEGER(index)[ i ] = INTEGER(xindex)[ xp-1 ]; 
        int_index[ i ] = int_xindex[ xp-1 ];
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          ij_original = (xp-1) + j * nrx; //num_rows;
          //REAL(result)[ ij_result ] = REAL(x)[ ij_original ];
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(x)[ ij_original ];
              break;
            case INTSXP:
              //INTEGER(result)[ ij_result ] = INTEGER(x)[ ij_original ];
              int_result[ ij_result ] = int_x[ ij_original ];
              break;
            case REALSXP:
              //REAL(result)[ ij_result ] = REAL(x)[ ij_original ];
              real_result[ ij_result ] = real_x[ ij_original ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ] = COMPLEX(x)[ ij_original ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(x, ij_original));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
        for(j = 0; j < ncy; j++) { /* y-values */
          ij_result = i + (j+ncx) * num_rows;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(fill)[ 0 ];
              break;
            case INTSXP:
              int_result[ ij_result ] = int_fill;
              break;
            case REALSXP:
              REAL(result)[ ij_result ] = REAL(fill)[ 0 ];
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ].r = REAL(fill)[ 0 ];
              COMPLEX(result)[ ij_result ].i = REAL(fill)[ 0 ];
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(fill, 0));
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
      }
      xp++;
      if(!left_join) i--;
    } else

    //if( INTEGER(xindex)[ xp-1 ]  > INTEGER(yindex)[ yp-1 ] ) {
    if( int_xindex[ xp-1 ] > int_yindex[ yp-1 ] ) {
      if(right_join) {
        //INTEGER(index)[ i ] = INTEGER(yindex)[ yp-1 ]; 
        int_index[ i ] = int_yindex[ yp-1 ];
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          //REAL(result)[ ij_result ] = NA_REAL;
          switch( mode ) {
            case LGLSXP:
              LOGICAL(result)[ ij_result ] = LOGICAL(fill)[ 0 ]; //NA_INTEGER;
            case INTSXP:
              int_result[ ij_result ] = int_fill;
              break;
            case REALSXP:
              REAL(result)[ ij_result ] = REAL(fill)[ 0 ]; //NA_REAL;
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ].r = REAL(fill)[ 0 ]; //NA_REAL;
              COMPLEX(result)[ ij_result ].i = REAL(fill)[ 0 ]; //NA_REAL;
              break;
            case STRSXP:
              SET_STRING_ELT(result, ij_result, STRING_ELT(fill, 0)); //NA_STRING);
              break;
            default:
              error("unsupported data type");
              break;
          }
        }
        for(j = 0; j < ncy; j++) { // y-values
          ij_result = i + (j+ncx) * num_rows;
          ij_original = (yp-1) + j * nry; //num_rows;
          //REAL(result)[ ij_result ] = REAL(y)[ ij_original ];
          switch( mode ) {
              case LGLSXP:
                LOGICAL(result)[ ij_result ] = LOGICAL(y)[ ij_original ];
                break;
              case INTSXP:
                //INTEGER(result)[ ij_result ] = INTEGER(y)[ ij_original ];
                int_result[ ij_result ] = int_y[ ij_original ];
                break;
              case REALSXP:
                //REAL(result)[ ij_result ] = REAL(y)[ ij_original ];
                real_result[ ij_result ] = real_y[ ij_original ];
                break;
              case CPLXSXP:
                COMPLEX(result)[ ij_result ] = COMPLEX(y)[ ij_original ];
                break;
              case STRSXP:
                SET_STRING_ELT(result, ij_result, STRING_ELT(y, ij_original));
                break;
              default:
                error("unsupported data type");
                break;
          }
        }
      }
      yp++;
      if(!right_join) i--;
    }
  }
  }

  /* following logic to allow for 
     dimensionless xts objects (unsupported)
     to be used in Ops.xts calls
     This maps to how zoo behaves */
  if(LOGICAL(retside)[0] &&
     !LOGICAL(retside)[1] && 
     isNull(getAttrib(x,R_DimSymbol))) {
     /* retside=c(T,F) AND is.null(dim(x)) */ 
     setAttrib(result, R_DimSymbol, R_NilValue);
  } else 
  if(LOGICAL(retside)[1] &&
     !LOGICAL(retside)[0] && 
     isNull(getAttrib(y,R_DimSymbol))) {
     /* retside=c(F,T) AND is.null(dim(y)) */ 
     setAttrib(result, R_DimSymbol, R_NilValue);
  } else /* set Dim and DimNames */
  if(num_rows >= 0 && (ncx + ncy) >= 0) {
    /* DIM */
    PROTECT(attr = allocVector(INTSXP, 2));
    INTEGER(attr)[0] = num_rows;
    INTEGER(attr)[1] = ncx + ncy;
    setAttrib(result, R_DimSymbol, attr);
    UNPROTECT(1);
    /* DIMNAMES */
    if(!isNull(colnames)) { // only set DimNamesSymbol if passed colnames is not NULL
      SEXP dimnames, dimnames_x, dimnames_y, newcolnames;
      PROTECT(dimnames = allocVector(VECSXP, 2));
      PROTECT(dimnames_x = getAttrib(x, R_DimNamesSymbol)); p++;
      PROTECT(dimnames_y = getAttrib(y, R_DimNamesSymbol)); p++;
      PROTECT(newcolnames = allocVector(STRSXP, ncx+ncy));
      for(i = 0; i < (ncx + ncy); i++) {
        if( i < ncx ) {
          if(!isNull(dimnames_x) && !isNull(VECTOR_ELT(dimnames_x,1))) {
            SET_STRING_ELT(newcolnames, i, STRING_ELT(VECTOR_ELT(dimnames_x,1),i));
          } else {
            SET_STRING_ELT(newcolnames, i, STRING_ELT(colnames, i));
          }
        } else { // i >= ncx; 
          if(!isNull(dimnames_y) && !isNull(VECTOR_ELT(dimnames_y,1))) {
            SET_STRING_ELT(newcolnames, i, STRING_ELT(VECTOR_ELT(dimnames_y,1),i-ncx));
          } else {
            SET_STRING_ELT(newcolnames, i, STRING_ELT(colnames, i));
          }
        }
      }
      SET_VECTOR_ELT(dimnames, 0, R_NilValue);  // ROWNAMES are NULL

      PROTECT(s = t = allocList(3)); p++;
      SET_TYPEOF(s, LANGSXP);
      SETCAR(t, install("make.names")); t = CDR(t);
      SETCAR(t, newcolnames); t = CDR(t);
      PROTECT(unique = allocVector(LGLSXP, 1)); p++;  LOGICAL(unique)[0] = 1;
      SETCAR(t, unique);  SET_TAG(t, install("unique"));
      SET_VECTOR_ELT(dimnames, 1, eval(s, env));
 
      //SET_VECTOR_ELT(dimnames, 1, newcolnames); // COLNAMES are passed in
      setAttrib(result, R_DimNamesSymbol, dimnames);
      UNPROTECT(2);
    }
  } else {
    // only used for zero-width results! xts always has dimension
    setAttrib(result, R_DimSymbol, R_NilValue);
  }

  setAttrib(result, xts_IndexSymbol, index);
  if(LOGICAL(retclass)[0])
    setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
  setAttrib(result, xts_IndexClassSymbol, getAttrib(x, xts_IndexClassSymbol));
  setAttrib(result, xts_IndexTZSymbol, getAttrib(x, xts_IndexTZSymbol));
  setAttrib(result, xts_IndexFormatSymbol, getAttrib(x, xts_IndexFormatSymbol));
  setAttrib(result, xts_ClassSymbol, getAttrib(x, xts_ClassSymbol));
  copy_xtsAttributes(x, result);

  UNPROTECT(4 + p);
  return result;  
} //}}}

//SEXP mergeXts (SEXP all, SEXP fill, SEXP retclass, SEXP colnames, SEXP retside, SEXP env, SEXP args)
/* called via .External("mergeXts", ...) */
SEXP mergeXts (SEXP args) // mergeXts {{{
{
  SEXP _x, _y, xtmp, result, _INDEX;
  /* colnames should be renamed as suffixes, as colnames need to be added at the C level */
  SEXP all, fill, retc, retclass, symnames,
       suffixes, rets, retside, env, tzone;
  int nr, nc, ncs=0;
  int index_len;
  int i, n=0, P=0;

  SEXP argstart;

  args = CDR(args);
  PROTECT(all = CAR(args)); P++;
  args = CDR(args);
  PROTECT(fill = CAR(args)); P++;
  args = CDR(args);
  PROTECT(retclass = CAR(args)); P++;
  args = CDR(args);
  PROTECT(symnames = CAR(args)); P++;
  args = CDR(args);
  PROTECT(suffixes = CAR(args)); P++;
  args = CDR(args);
  PROTECT(retside = CAR(args)); P++;
  args = CDR(args);
  PROTECT(env = CAR(args)); P++;
  args = CDR(args);
  PROTECT(tzone = CAR(args)); P++;
  args = CDR(args);
  // args should now correspond to the ... objects we are looking to merge 
  argstart = args; // use this to rewind list...

  n = 0;
  int type_of;
  int coerce_to_double=0;
  if(args != R_NilValue) type_of = TYPEOF(CAR(args));
  while(args != R_NilValue) {
    if( length(CAR(args)) > 0 )
      ncs += ncols(CAR(args));
    if(TYPEOF(CAR(args)) != type_of)
      coerce_to_double = 1;  /* need to convert all objects if one needs to be converted */
    args = CDR(args);
    n++;
  }


  /* build an index to be used in all subsequent calls */
  args = argstart;

  PROTECT(_x = CAR(args)); P++;
  args = CDR(args);

  int leading_non_xts = 0;
  while( !isXts(_x) ) {
    if( args == R_NilValue ) error("no xts object to merge");
    leading_non_xts = 1;
    /*warning("leading non-xts objects may have been dropped");*/
    PROTECT(_x = CAR(args)); P++;
    args = CDR(args);
  }
  /* test for NULLs that may be present from cbind dispatch */
  if(!leading_non_xts) { /* leading non-xts in 2 case scenario was igoring non-xts value */
    if(n < 3 && (args == R_NilValue || (isNull(CAR(args)) && length(args) == 1))) {/* no y arg or y==NULL */
      UNPROTECT(P);
      return(_x);
    }
  }

  if( args != R_NilValue) {
    PROTECT(_y = CAR(args)); P++;
    args = CDR(args);
  } else {
    PROTECT(_y = duplicate(_x)); P++;
  }

  if(n > 2 || leading_non_xts) { /*args != R_NilValue) {*/
    /* generalized n-case optimization
       currently if n>2 this is faster and more memory efficient
       than recursively building a merged object, object by object. */

    PROTECT(retc = allocVector(LGLSXP, 1)); P++;
    LOGICAL(retc)[0] = 1; /* return class == TRUE */
    PROTECT(rets = allocVector(LGLSXP, 2)); P++;
    LOGICAL(rets)[0] = 0; /* don't return left */
    LOGICAL(rets)[1] = 0; /* don't return right */
  
    if( isNull(_y) ) {
      PROTECT(_y = duplicate(_x)); P++;
    }

    PROTECT(_INDEX = do_merge_xts(_x,
                                  _y, 
                                  all,
                                  fill,
                                  retc,
                                  R_NilValue,
                                  R_NilValue, 
                                  rets, 
                                  env,
                                  coerce_to_double)); P++;

    /* merge all objects into one zero-width common index */
    while(args != R_NilValue) { 
      if( !isNull(CAR(args)) ) {
        PROTECT(_INDEX = do_merge_xts(_INDEX,
                                      CAR(args),
                                      all,
                                      fill, 
                                      retc,
                                      R_NilValue,
                                      R_NilValue,
                                      rets, 
                                      env,
                                      coerce_to_double)); P++;
      }
      args = CDR(args);
    }

    index_len = length(GET_xtsIndex(_INDEX));
  
    args = argstart; // reset args
    int ii, jj, iijj, jj_result;
    int *int_result=NULL, *int_xtmp=NULL;
    double *real_result=NULL, *real_xtmp=NULL;

    PROTECT(result = allocVector(TYPEOF(_INDEX), index_len * ncs)); P++;
    switch(TYPEOF(result)) {
      case LGLSXP:
      case INTSXP:
        int_result = INTEGER(result);
        break;
      case REALSXP:
        real_result = REAL(result);
        break;
      default:
        error("unsupported data type");
    }

    SEXP ColNames, NewColNames;
    PROTECT(NewColNames = allocVector(STRSXP, ncs)); P++;
    ncs = 0;
    for(i = 0, nc=0; args != R_NilValue; i = i+nc, args = CDR(args)) { // merge each object with index
      // i is object current being merged/copied
      // nc is offset in current object
      if( isNull(CAR(args)) ) {
        i = i-nc;
        continue;  // if NULL is passed, skip to the next object.
      }

      xtmp = do_merge_xts(_INDEX,
                          CAR(args),
                          all,
                          fill,
                          retclass,
            /*colnames*/R_NilValue, 
                        R_NilValue,
                          retside,
                          env,
                          coerce_to_double);
      nc = ncols(xtmp);
      ncs += nc;
      nr = nrows(xtmp);
      PROTECT(ColNames = getAttrib(CAR(args),R_DimNamesSymbol));
      switch(TYPEOF(xtmp)) { // by type, insert merged data into result object
        case LGLSXP:
        case INTSXP:
          int_xtmp = INTEGER(xtmp);
          for(jj=0; jj < nc; jj++) {
            if(!isNull(ColNames) && !isNull(VECTOR_ELT(ColNames,1))) {
              /* if merged object has colnames, use these, otherwise use deparse names */
              SET_STRING_ELT(NewColNames, i+jj, STRING_ELT(VECTOR_ELT(ColNames,1),jj));
            } else {
              SET_STRING_ELT(NewColNames, i+jj, STRING_ELT(symnames,i+jj));
            }
            for(ii=0; ii < nr; ii++) {
              iijj = ii + jj * nr;
              jj_result = ii + ( (i+jj) * nr);
              int_result[ jj_result ] = int_xtmp[ iijj ];
            }
          }
          break;
        case REALSXP:
          real_xtmp = REAL(xtmp);
          for(jj=0; jj < nc; jj++) {
            if(!isNull(ColNames) && !isNull(VECTOR_ELT(ColNames,1))) {
              SET_STRING_ELT(NewColNames, i+jj, STRING_ELT(VECTOR_ELT(ColNames,1),jj));
            } else {
              SET_STRING_ELT(NewColNames, i+jj, STRING_ELT(symnames,i+jj));
            }
            for(ii=0; ii < nr; ii++) {
              iijj = ii + jj * nr;
              jj_result = ii + ( (i+jj) * nr);
              real_result[ jj_result ] = real_xtmp[ iijj ];
            }
          }
          break;
      }
      UNPROTECT(1); /* ColNames */
    }

    SEXP dim;
    PROTECT(dim = allocVector(INTSXP, 2)); P++;
    INTEGER(dim)[0] = index_len;
    INTEGER(dim)[1] = ncs;
    setAttrib(result, R_DimSymbol, dim);

    SEXP dimnames;
    PROTECT(dimnames = allocVector(VECSXP, 2)); P++;
    SET_VECTOR_ELT(dimnames, 0, R_NilValue); // rownames are always NULL in xts

    /* colnames, assure they are unique before returning */
    SEXP s, t, unique;
    PROTECT(s = t = allocList(3)); P++;
    SET_TYPEOF(s, LANGSXP);
    SETCAR(t, install("make.names")); t = CDR(t);
    SETCAR(t, NewColNames); t = CDR(t);
    PROTECT(unique = allocVector(LGLSXP, 1)); P++;  LOGICAL(unique)[0] = 1;
    SETCAR(t, unique);  SET_TAG(t, install("unique"));
    SET_VECTOR_ELT(dimnames, 1, eval(s, env));
    setAttrib(result, R_DimNamesSymbol, dimnames);

    SET_xtsIndex(result, GET_xtsIndex(_INDEX));
    SET_xtsIndexTZ(result, GET_xtsIndexTZ(_INDEX));
    copy_xtsCoreAttributes(_INDEX, result);
    copy_xtsAttributes(_INDEX, result);

  } else { /* 2-case optimization --- simply call main routine */
    /* likely bug in handling of merge(1, xts) case */
    PROTECT(result = do_merge_xts(_x,
                                  _y, 
                                 all,
                                fill,
                            retclass,
                            symnames /*R_NilValue*/,
                            suffixes,
                             retside,
                                 env,
                    coerce_to_double)); P++;
  }

  SEXP index_tmp = getAttrib(result, install("index"));
  PROTECT(index_tmp);
  if(isNull(tzone)) {
    setAttrib(index_tmp, install("tzone"), 
              getAttrib(getAttrib(_x,install("index")), install("tzone")));
  } else {
    setAttrib(index_tmp, install("tzone"), tzone);
  }
  setAttrib(result, install("index"), index_tmp);
  setAttrib(result, install(".indexTZ"), getAttrib(index_tmp, install("tzone")));
  UNPROTECT(1);

  if(P > 0) UNPROTECT(P); 
  return(result);
} //}}} end of mergeXts
