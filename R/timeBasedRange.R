`timeBasedRange` <-
function(x, ...) {
  # convert unquoted time range to
  if (!is.character(x)) 
    x <- deparse(match.call()$x)

  # determine start and end points
  tblist <- timeBasedSeq(x,NULL)

#  if(!is.null(tblist$length.out))
#    return(tblist$from)

  c(as.numeric(tblist$from), as.numeric(tblist$to))
}
