#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "xts.h"

#define ZWXTS 43434343
/* 

  This is a merge_join algorithm used to
  allow two xts objects to be merged as one
  along a common index efficiently and fast

  The code is branched for REAL and INTEGER indexed values
  which allows for efficient memory usage and minimal
  testing/coercion

  
  Copyright Jeffrey A. Ryan 2008

*/
// do_merge_xts {{{
SEXP do_merge_xts (SEXP x, SEXP y, SEXP all, SEXP fill, SEXP retclass, SEXP colnames)
{
  int nrx, ncx, nry, ncy, len, merge_all, original_index_type;
  int left_join, right_join;
  int i = 0, j = 0, xp = 1, yp = 1; // x and y positions in index
  int mode;
  int ij, ij_original, ij_result;
  int p = 0;
  SEXP xindex, yindex, index, result, attr;

  int *int_result, *int_x, *int_y;
  int *int_index, *int_xindex, *int_yindex;
  double *real_result, *real_x, *real_y;
  double *real_index, *real_xindex, *real_yindex;

  PROTECT( xindex = getAttrib(x, install("index")) );
  PROTECT( yindex = getAttrib(y, install("index")) );

  // if object is zero-width
  nrx = LENGTH(x)==0 ? nrows(xindex) : nrows(x);
  ncx = ncols(x);
  
  // if object is zero-width
  nry = LENGTH(y)==0 ? nrows(yindex) : nrows(y);
  ncy = ncols(y);

  len = nrx + nry;

  mode = TYPEOF(x);

  /* at present we are failing the call if the indexing is of
     mixed type.  This should probably instead simply coerce
     to REAL so as not to lose any information (at the expense
     of conversion cost and memory), and issue a warning. */
  //if( TYPEOF(xindex) != TYPEOF(yindex) ) 
  if( TYPEOF(xindex) != TYPEOF(yindex) )
  {
    PROTECT(xindex = coerceVector(xindex, REALSXP)); p++;
    PROTECT(yindex = coerceVector(yindex, REALSXP)); p++;
    //warning ("incompatible index type, coerced to type double"); //FIXME coerceVector???
  }


  if( TYPEOF(all) != LGLSXP )
    error("all must be a logical value of TRUE or FALSE");

  if( TYPEOF(fill) != TYPEOF(x) ) {
    PROTECT( fill = coerceVector(fill, TYPEOF(x)) ); p++; // p is used to make sure our UNPROTECT is correct
  } 

  //merge_all = INTEGER(all)[ 0 ];

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
      //if(merge_all) i++;
      if(right_join) i++;
    } else
    if( yp > nry ) {
      xp++;
      //if(merge_all) i++;
      if(left_join) i++;
    } else
    //if(REAL(xindex)[ xp-1 ] == REAL(yindex)[ yp-1 ]) {
    if( real_xindex[ xp-1 ] == real_yindex[ yp-1 ] ) {
      // INNER JOIN
      // this will be the only result all=FALSE
      yp++;
      xp++;
      i++;
    } else
    //if( REAL(xindex)[ xp-1 ]  < REAL(yindex)[ yp-1 ] ) {
    if( real_xindex[ xp-1 ] < real_yindex[ yp-1 ] ) {
      // LEFT JOIN
      xp++;
      //if(merge_all) i++;
      if(left_join) i++;
    } else
    //if( REAL(xindex)[ xp-1 ]  > REAL(yindex)[ yp-1 ] ) {
    if( real_xindex[ xp-1 ] > real_yindex[ yp-1 ] ) {
      // RIGHT JOIN
      yp++;
      //if(merge_all) i++;
      if(right_join) i++;
    }
  } 
  } else
  if( TYPEOF(xindex) == INTSXP ) {
  int_xindex = INTEGER(xindex);
  int_yindex = INTEGER(yindex);
  while( (xp + yp) <= (len + 1) ) {
    if( xp > nrx ) {
      yp++;
      //if(merge_all) i++;
      if(right_join) i++;
    } else
    if( yp > nry ) {
      xp++;
      //if(merge_all) i++;
      if(left_join) i++;
    } else
    //if(INTEGER(xindex)[ xp-1 ] == INTEGER(yindex)[ yp-1 ]) {
    if( int_xindex[ xp-1 ] == int_yindex[ yp-1 ] ) {
      // this will be the only result all=FALSE
      yp++;
      xp++;
      i++;
    } else
    //if( INTEGER(xindex)[ xp-1 ]  < INTEGER(yindex)[ yp-1 ] ) {
    if( int_xindex[ xp-1 ] < int_yindex[ yp-1 ] ) {
      xp++;
      //if(merge_all) i++;
      if(left_join) i++;
    } else
    //if( INTEGER(xindex)[ xp-1 ]  > INTEGER(yindex)[ yp-1 ] ) {
    if( int_xindex[ xp-1 ] > int_yindex[ yp-1 ] ) {
      yp++;
      //if(merge_all) i++;
      if(right_join) i++;
    }
  } 
  }

  if(i == 0) {
    /* if no rows match, return an empty xts object, similar in style to zoo */
    UNPROTECT(2 + p);
    return R_NilValue;
    /*
    PROTECT(result=allocVector(TYPEOF(x),0));
    setAttrib(result, install("index"), allocVector(TYPEOF(xindex), 0));
    if(LOGICAL(retclass)[0]) 
      setAttrib(result, install("class"), getAttrib(x, install("class")));
    setAttrib(result, install(".indexCLASS"), getAttrib(x, install(".indexCLASS")));
    setAttrib(result, install(".indexFORMAT"), getAttrib(x, install(".indexFORMAT")));
    setAttrib(result, install(".CLASS"), getAttrib(x, install(".CLASS")));
    UNPROTECT(1);
    return result;
    */
  }

  int num_rows = i;
  xp = 1; yp = 1;

  PROTECT( index  = allocVector(TYPEOF(xindex), num_rows) );
  /* coercion/matching of TYPE for x and y needs to be checked,
     either here or in the calling R code.  I suspect here is
     more useful if other function can call the C code as well. */
  PROTECT( result = allocVector(TYPEOF(x), (ncx + ncy) * num_rows) );

  /* need to coerce y to typeof x */
  if( TYPEOF(x) != TYPEOF(y) ) {
    PROTECT( y = coerceVector(y, TYPEOF(x)) ); p++;
  }

  /* use pointers instead of function calls */
  switch(TYPEOF(x)) {
    case INTSXP:
        int_x = INTEGER(x);
        int_y = INTEGER(y);
        int_result = INTEGER(result);
        break;
    case REALSXP:
        real_x = REAL(x);
        real_y = REAL(y);
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
      //if(merge_all) {
      if(right_join) {
        //copyIndex(index, yindex, i, yp-1);
        //REAL(index)[ i ] = REAL(yindex)[ yp-1 ]; 
        real_index[ i ] = real_yindex[ yp-1 ];
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          //REAL(result)[ ij_result ] = NA_REAL;
          switch( mode ) {
            case LGLSXP:
            case INTSXP:
              LOGICAL(result)[ ij_result ] = INTEGER(fill)[ 0 ]; //NA_INTEGER;
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
      // if attempting LJOIN or RJOIN, possibly use this to affect output
      //if(!merge_all) i--;  // if all=FALSE, we must decrement i for each non-match
      if(!right_join) i--;  // if all=FALSE, we must decrement i for each non-match
    } else

    /* past the last row of y */
    if( yp > nry ) {
      //if(merge_all) {
      if(left_join) {

        /* record new index value */
        //REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
        real_index[ i ] = real_xindex[ xp-1 ];

        /* copy values from x and y to result */
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          ij_original = (xp-1) + j * nrx; //num_rows;
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

        /* we are out of y-values, so fill merged result with NAs */
        for(j = 0; j < ncy; j++) { // y-values
          ij_result = i + (j+ncx) * num_rows;
          //REAL(result)[ ij_result ] = NA_REAL;
          switch( mode ) {
            case LGLSXP:
            case INTSXP:
              LOGICAL(result)[ ij_result ] = INTEGER(fill)[ 0 ]; //NA_INTEGER;
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
      //if(!merge_all) i--;
      if(!left_join) i--;
    } else

    /* matching index values copy all column values from x and y to results */
    //if( REAL(xindex)[ xp-1 ] == REAL(yindex)[ yp-1 ] ) {
    if( real_xindex[ xp-1 ] == real_yindex[ yp-1 ] ) {

      /* copy index FIXME this needs to handle INTEGER efficiently as well*/
      //REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
      real_index[ i ] = real_xindex[ xp-1 ];

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
      xp++;
      yp++;
    } else

    //if( REAL(xindex)[ xp-1 ]  < REAL(yindex)[ yp-1 ] ) {
    if( real_xindex[ xp-1 ] < real_yindex[ yp-1 ] ) {
      //if(merge_all) {
      if(left_join) {
        //copyIndex(index, xindex, i, xp-1);
        //REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
        real_index[ i ] = real_xindex[ xp-1 ];
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
        for(j = 0; j < ncy; j++) { // y-values
          ij_result = i + (j+ncx) * num_rows;
          //REAL(result)[ ij_result ] = NA_REAL;
          switch( mode ) {
            case LGLSXP:
            case INTSXP:
              LOGICAL(result)[ ij_result ] = INTEGER(fill)[ 0 ]; //NA_INTEGER;
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
      //if(!merge_all) i--;
      if(!left_join) i--;
    } else

    //if( REAL(xindex)[ xp-1 ]  > REAL(yindex)[ yp-1 ] ) {
    if( real_xindex[ xp-1 ] > real_yindex[ yp-1 ] ) {
      //if(merge_all) {
      if(right_join) {
        //copyIndex(index, yindex, i, yp-1);
        //REAL(index)[ i ] = REAL(yindex)[ yp-1 ]; 
        real_index[ i ] = real_yindex[ yp-1 ];
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          //REAL(result)[ ij_result ] = NA_REAL;
          switch( mode ) {
            case LGLSXP:
            case INTSXP:
              LOGICAL(result)[ ij_result ] = INTEGER(fill)[ 0 ]; //NA_INTEGER;
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
      //if(!merge_all) i--;
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
      //if(merge_all) {
      if(right_join) {
        //copyIndex(index, yindex, i, yp-1);
        int_index[ i ] = int_yindex[ yp-1 ];
        //INTEGER(index)[ i ] = INTEGER(yindex)[ yp-1 ]; 
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          //REAL(result)[ ij_result ] = NA_REAL;
          switch( mode ) {
            case LGLSXP:
            case INTSXP:
              INTEGER(result)[ ij_result ] = INTEGER(fill)[ 0 ]; //NA_INTEGER;
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
      //if(!merge_all) i--;  // if all=FALSE, we must decrement i for each non-match
      if(!right_join) i--;  // if all=FALSE, we must decrement i for each non-match
    } else

    /* past the last row of y */
    if( yp > nry ) {
      if(left_join) {

        /* record new index value */
        //INTEGER(index)[ i ] = INTEGER(xindex)[ xp-1 ]; 
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
            case INTSXP:
              INTEGER(result)[ ij_result ] = INTEGER(fill)[ 0 ]; //NA_INTEGER;
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
        for(j = 0; j < ncy; j++) { // y-values
          ij_result = i + (j+ncx) * num_rows;
          //REAL(result)[ ij_result ] = NA_REAL;
          switch( mode ) {
            case LGLSXP:
            case INTSXP:
              LOGICAL(result)[ ij_result ] = INTEGER(fill)[ 0 ]; //NA_INTEGER;
              break;
            case REALSXP:
              REAL(result)[ ij_result ] = REAL(fill)[ 0 ]; //NA_REAL;
              break;
            case CPLXSXP:
              COMPLEX(result)[ ij_result ].r = REAL(fill)[ 0 ]; //NA_REAL;  This is a bug...
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
            case INTSXP:
              LOGICAL(result)[ ij_result ] = INTEGER(fill)[ 0 ]; //NA_INTEGER;
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

  /* set Dim and DimNames */
  if(num_rows >= 0 && (ncx + ncy) >= 0) {
    /* DIM */
    PROTECT(attr = allocVector(INTSXP, 2));
    INTEGER(attr)[0] = num_rows;
    INTEGER(attr)[1] = ncx + ncy;
    setAttrib(result, R_DimSymbol, attr);
    UNPROTECT(1);
    /* DIMNAMES */
    if(!isNull(colnames)) {
      SEXP dimnames, newcolnames;
      PROTECT(dimnames = allocVector(VECSXP, 2));
      PROTECT(newcolnames = allocVector(STRSXP, ncx+ncy));
      for(i = 0; i < (ncx + ncy); i++) {
        SET_STRING_ELT(newcolnames, i, STRING_ELT(colnames, i));
      }
      SET_VECTOR_ELT(dimnames, 0, R_NilValue);  // ROWNAMES are NULL
      SET_VECTOR_ELT(dimnames, 1, newcolnames); // COLNAMES are passed in
      setAttrib(result, R_DimNamesSymbol, dimnames);
      UNPROTECT(2);
    }
  }
  

  setAttrib(result, xts_IndexSymbol, index);
  if(LOGICAL(retclass)[0])
    setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
  setAttrib(result, xts_IndexClassSymbol, getAttrib(x, xts_IndexClassSymbol));
  setAttrib(result, xts_IndexFormatSymbol, getAttrib(x, xts_IndexFormatSymbol));
  setAttrib(result, xts_ClassSymbol, getAttrib(x, xts_ClassSymbol));
  copy_xtsAttributes(x, result);

  UNPROTECT(4 + p);
  return result;  
} //}}}

SEXP mergeXts (SEXP args, SEXP all, SEXP fill, SEXP retclass, SEXP colnames)
{
  SEXP _x, _y, xtmp, ytmp, result;
  int a, largs, P=0;
  PROTECT(_x = VECTOR_ELT(args, 0)); P++;
  PROTECT(_y = VECTOR_ELT(args, 1)); P++;

  largs = LENGTH(args);

  PROTECT(xtmp = do_merge_xts(_x, _y, all, fill, retclass, colnames)); P++;

  for(a = 2; a < largs; a++) {
    PROTECT(ytmp = VECTOR_ELT(args, a));P++;
    PROTECT(xtmp = do_merge_xts(xtmp, ytmp, all, fill, retclass, colnames));P++;
  }
  UNPROTECT(P);
  return(xtmp);
}
