#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


#define OFFSET(r,c,rows) ((r-1) + (c-1) * rows)
/* 

  This is a merge_join algorithm used to
  allow two xts objects to be merged as one
  along a common index efficiently and fast

  Copyright Jeffrey A. Ryan 2008

*/

SEXP do_merge_xts (SEXP x, SEXP y, SEXP all) //, SEXP fill)
{
  int nrx, ncx, nry, ncy, len, merge_all;
  int i, j, xp = 1, yp = 1; // x and y positions in index
  int ij, ij_original, ij_result;
  SEXP xindex, yindex, index, result, attr;

  nrx = nrows(x);
  ncx = ncols(x);
  
  nry = nrows(y);
  ncy = ncols(y);

  len = nrx + nry;

  PROTECT( xindex = getAttrib(x, install("index")) );
  PROTECT( yindex = getAttrib(y, install("index")) );

  if( TYPEOF(xindex) != TYPEOF(yindex) )
    error("incompatible index types");

  if( TYPEOF(all) != LGLSXP )
    error("all must be a logical value of TRUE or FALSE");

  merge_all = INTEGER(all)[0];

  /* determine num_rows of final merged xts object
     
     this seems to only cost 1/1000 of a sec per
     1e6 observations.  Acceptable 'waste' given
     that now we can properly allocate space
     for our results */

  while( (xp + yp) <= (len + 1) ) {
    if( xp > nrx ) {
      yp++;
      if(merge_all) i++;
    } else
    if( yp > nry ) {
      xp++;
      if(merge_all) i++;
    } else
    if( REAL(xindex)[ xp-1 ] == REAL(yindex)[ yp-1 ] ) {
      // this will be the only result all=FALSE
      yp++;
      xp++;
      i++;
    } else
    if( REAL(xindex)[ xp-1 ]  < REAL(yindex)[ yp-1 ] ) {
      xp++;
      if(merge_all) i++;
    } else
    if( REAL(xindex)[ xp-1 ]  > REAL(yindex)[ yp-1 ] ) {
      yp++;
      if(merge_all) i++;
    }
  } 

  if(i == 0) {
    UNPROTECT(2);
    return allocVector(TYPEOF(x),0);
  }

  int num_rows = i;
  xp = 1; yp = 1;

  PROTECT( index  = allocVector(TYPEOF(xindex), num_rows) );
  PROTECT( result = allocVector(TYPEOF(x), (ncx + ncy) * num_rows) );

  for(i = 0; i < num_rows; i++) {
    /* If we are past the last row in x, assign NA to merged data 
       and copy the y column values to the second side of result
    */
    if( xp > nrx ) {
      if(merge_all) {
        REAL(index)[ i ] = REAL(yindex)[ yp-1 ]; 
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          REAL(result)[ ij_result ] = NA_REAL;
        }
        for(j = 0; j < ncy; j++) { // y-values
          ij_result = i + (j+ncx) * num_rows;
          ij_original = (yp-1) + j * nry; //num_rows;
          REAL(result)[ ij_result ] = REAL(y)[ ij_original ];
        }
      }
      yp++;
      if(!merge_all) i--;  // if all=FALSE, we must decrement i for each non-match
    } else

    if( yp > nry ) {
      if(merge_all) {
        REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          ij_original = (xp-1) + j * nrx; //num_rows;
          REAL(result)[ ij_result ] = REAL(x)[ ij_original ];
        }
        for(j = 0; j < ncy; j++) { // y-values
          ij_result = i + (j+ncx) * num_rows;
          REAL(result)[ ij_result ] = NA_REAL;
        }
      }
      xp++;
      if(!merge_all) i--;
    } else

    if( REAL(xindex)[ xp-1 ] == REAL(yindex)[ yp-1 ] ) {
      REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
      for(j = 0; j < ncx; j++) { // x-values
        ij_result = i + j * num_rows;
        ij_original = (xp-1) + j * nrx; //num_rows;
        REAL(result)[ ij_result ] = REAL(x)[ ij_original ];
      }
      for(j = 0; j < ncy; j++) { // y-values
        ij_result = i + (j+ncx) * num_rows;
        ij_original = (yp-1) + j * nry; //num_rows;
        REAL(result)[ ij_result ] = REAL(y)[ ij_original ];
      }
      xp++;
      yp++;
    } else

    if( REAL(xindex)[ xp-1 ]  < REAL(yindex)[ yp-1 ] ) {
      if(merge_all) {
        REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          ij_original = (xp-1) + j * nrx; //num_rows;
          REAL(result)[ ij_result ] = REAL(x)[ ij_original ];
        }
        for(j = 0; j < ncy; j++) { // y-values
          ij_result = i + (j+ncx) * num_rows;
          REAL(result)[ ij_result ] = NA_REAL;
        }
      }
      xp++;
      if(!merge_all) i--;
    } else

    if( REAL(xindex)[ xp-1 ]  > REAL(yindex)[ yp-1 ] ) {
      if(merge_all) {
        REAL(index)[ i ] = REAL(yindex)[ yp-1 ]; 
        for(j = 0; j < ncx; j++) { // x-values
          ij_result = i + j * num_rows;
          REAL(result)[ ij_result ] = NA_REAL;
        }
        for(j = 0; j < ncy; j++) { // y-values
          ij_result = i + (j+ncx) * num_rows;
          ij_original = (yp-1) + j * nry; //num_rows;
          REAL(result)[ ij_result ] = REAL(y)[ ij_original ];
        }
      }
      yp++;
      if(!merge_all) i--;
    }
  }

  UNPROTECT(4);
  if(num_rows >= 0 && (ncx + ncy) >= 0) {
    PROTECT(attr = allocVector(INTSXP, 2));
    INTEGER(attr)[0] = num_rows;
    INTEGER(attr)[1] = ncx + ncy;
    setAttrib(result, R_DimSymbol, attr);
    UNPROTECT(1);
  }

  setAttrib(result, install("index"), index);
  setAttrib(result, install("class"), getAttrib(x, install("class")));
  setAttrib(result, install(".indexCLASS"), getAttrib(x, install(".indexCLASS")));
  setAttrib(result, install(".indexFORMAT"), getAttrib(x, install(".indexFORMAT")));
  setAttrib(result, install(".CLASS"), getAttrib(x, install(".CLASS")));

  return result;  
}
