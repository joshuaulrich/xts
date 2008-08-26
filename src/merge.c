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

SEXP do_merge_xts (SEXP x, SEXP y) //, SEXP all, SEXP fill)
{
  int nrx, ncx, nry, ncy, len;
  int i, xp = 1, yp = 1; // x and y positions in index
  SEXP xindex, yindex, index, result, tmp_table;

  nrx = nrows(x);
  ncx = ncols(x);
  
  nry = nrows(y);
  ncy = ncols(y);

  len = nrx + nry;

  PROTECT( xindex = getAttrib(x, install("index")) );
  PROTECT( yindex = getAttrib(y, install("index")) );

  if( TYPEOF(xindex) != TYPEOF(yindex) )
    error("incompatible index types");

  while( (xp + yp) <= (len + 1) ) {
    if( xp > nrx ) {
    //  REAL(index)[ i ] = REAL(yindex)[ yp-1 ]; 
      yp++;
    } else
    if( yp > nry ) {
    //  REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
      xp++;
    } else
    if( REAL(xindex)[ xp-1 ] == REAL(yindex)[ yp-1 ] ) {
    //  REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
      yp++;
      xp++;
    } else
    if( REAL(xindex)[ xp-1 ]  < REAL(yindex)[ yp-1 ] ) {
    //  REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
      xp++;
    } else
    if( REAL(xindex)[ xp-1 ]  > REAL(yindex)[ yp-1 ] ) {
    //  REAL(index)[ i ] = REAL(yindex)[ yp-1 ]; 
      yp++;
    }
    i++;
  } 

  int num_rows = i;
  xp = 1; yp = 1;

  PROTECT( index  = allocVector(TYPEOF(xindex), num_rows) );
  PROTECT( result = allocVector(TYPEOF(x), (ncx + ncy) * num_rows) );

  for(i = 0; i < num_rows; i++) {
    if( xp > nrx ) {
      REAL(index)[ i ] = REAL(yindex)[ yp-1 ]; 
      yp++;
    } else
    if( yp > nry ) {
      REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
      xp++;
    } else
    if( REAL(xindex)[ xp-1 ] == REAL(yindex)[ yp-1 ] ) {
      REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
      yp++;
      xp++;
    } else
    if( REAL(xindex)[ xp-1 ]  < REAL(yindex)[ yp-1 ] ) {
      REAL(index)[ i ] = REAL(xindex)[ xp-1 ]; 
      xp++;
    } else
    if( REAL(xindex)[ xp-1 ]  > REAL(yindex)[ yp-1 ] ) {
      REAL(index)[ i ] = REAL(yindex)[ yp-1 ]; 
      yp++;
    }
  }

  /* the above routine minimizes memory allocation, at the
     small expense of an additional loop, which also
     will  make implementation more clean and obvious 

  int total_rows = i;
  PROTECT( result = allocVector(TYPEOF(x), (ncx + ncy) * total_rows) );

  for(i = 0; i < total_rows; i++) {
    // do x-values
    if( REAL(index)[ i ] == REAL(xindex)[ i ] ) {

    } else {

    }

    // do y-values 
  }
  */

  UNPROTECT(4);

  return index;  
}
