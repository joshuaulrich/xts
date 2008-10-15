`Ops.xts` <-
function(e1, e2)
{
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
      e1 <- merge.xts(e1, .xts(,.index(e2)), all=FALSE, retclass=FALSE)
      e2 <- merge.xts(e2, .xts(,.index(e1)), all=FALSE, retclass=FALSE)
      .Class <- "matrix"
      NextMethod(.Generic)
    }
  }
  if(.Generic %in% c("+","-","*","/","^","%%","%/%")) {
    .Call('add_xts_class', e)
  }
  else e
}
