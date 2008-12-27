`Ops.xts` <-
function(e1, e2)
{
  CLASS <- .Class
  e <- if (missing(e2)) {
      .Class <- "matrix"
      NextMethod(.Generic)
  }
  else if (any(nchar(.Method) == 0)) {
      .Class <- "matrix"
      NextMethod(.Generic)
  }
  else {
    if( NROW(e1)==NROW(e2) && identical(.index(e1),.index(e2)) ) {
    .Class <- "matrix"
    NextMethod(.Generic)
    } else {
      e1 <- merge.xts(e1, e2, all=FALSE, retclass=FALSE, retside=c(TRUE,FALSE))
      e2 <- merge.xts(e2, e1, all=FALSE, retclass=FALSE, retside=c(TRUE,FALSE))
      .Class <- "matrix"
      NextMethod(.Generic)
    }
  }
  if(.Generic %in% c("+","-","*","/","^","%%","%/%")) {
    #.Call('add_xts_class', e)
    .Call('add_class', e, CLASS, PACKAGE="xts")
  }
  else 
  if(is.null(attr(e,'index'))) {
    if(is.xts(e1)) {
      .xts(e, .index(e1))
    } else .xts(e, .index(e2))
  } else {
  e
  }
}
