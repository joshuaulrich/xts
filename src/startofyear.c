
void do_startofyear (int *from, int *to, int *fromto, int *origin) {
// do_startofyear {{{
  int i;
  int nyear[1] = { (to[0] - from[0] + 1) };
  int leap[nyear[0]];

  // generate sequence of dates to work with
  fromto[0] = from[0];
  for(i=1; i < nyear[0]; i++) {
    fromto[i] = fromto[i-1] + 1;
  } 

  for(i = 0; i < nyear[0]; i++) {
    leap[ i ] = ( (fromto[ i ] % 4 == 0 && fromto[ i ] % 100 != 0)
                 ||
                   fromto[ i ] % 400 == 0) ? 1 : 0;
  }

  for(i=0; i < nyear[0]; i++) {
    if(leap[i] == 1) { // a leapyear (366 days)
      fromto[i] = 366;
    } else {           // a non-leapyear (365 days)
      fromto[i] = 365;
    }
  }
  
  /*
    fromto now has proper number of days per year

    now calculate the cumulative sum back from origin (negative)
    and from origin (positive)
  */

  int days_before_origin = origin[0] - from[0];
  int days_after_origin  = nyear[0] - days_before_origin - 1;

  int tmp=0;

  for(i = days_before_origin; i < nyear[0]; i++) {
    tmp += fromto[i];
    fromto[i] = tmp;
  }

  tmp = 0;
  for(i = days_before_origin-1; i >= 0; i--) {
    tmp -= fromto[i];
    fromto[i] = tmp;
  }

  /* now insert a 0 at the origin, by going backwards */

  for(i = nyear[0] - 1; i > days_before_origin; i--) 
    fromto[ i ] = fromto[ i-1 ];

  fromto[ days_before_origin ] = 0;

} //}}}
