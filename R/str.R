`str.xts` <-
function(object,...) {
  cat(paste("An",sQuote('xts'),"object from",index(first(object)),"to",index(last(object)),"containing:\n"))
  cat(paste("  Data:"))
  str(coredata(object),...)
  cat(paste("  Indexed by objects of class: "))
  cat(paste('[',indexClass(object),']\n ',sep=''))
  if(!is.null(CLASS(object)))
    cat(paste("  Original class: '",CLASS(object),"' ",sep=""),"\n")
  cat(paste("  xts Attributes: "),"\n")
  str(xtsAttributes(object),...)
}

