`binsearch` <-
function(vec,  key, start=TRUE) {
  # vec is a numeric vector to search
  # key is the numeric key we are looking for
  # start is a logical indicating
  # TRUE return the _next_ observation (e.g. 20070801 if
  # missing would return 20070802 if available
  # FALSE would return the _previous_ obs.

  found <- FALSE
  lo <- 1; hi <- length(vec);
  rec <- NULL

  while(hi >= lo) {
    mid <- round((lo + hi) / 2)
    if(key < vec[mid]) {
      hi <- mid - 1
    } else 
    if( key > vec[mid] ) {
      lo <- mid + 1
    } else {
      found <- TRUE
      rec <- mid
      break
    }
  }
  # if not found return the appropriate bound
  if(is.null(rec)) {
    if(start) {
      lo
    } else {
      hi
    }
  # if found - return the exact match location
  } else rec
}

