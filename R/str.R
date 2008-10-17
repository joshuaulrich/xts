`str.xts` <-
function(object,...) {
  if(length(object) == 0) {
    cat("An 'xts' object of zero-width\n")
  } else {
  cat(paste("An",sQuote('xts'),"object from",index(first(object)),"to",index(last(object)),"containing:\n"))
  cat(paste("  Data:"))
  str(coredata(object))
  cat(paste("  Indexed by objects of class: "))
  cat(paste('[',paste(indexClass(object),collapse=','),']\n ',sep=''))
  if(!is.null(CLASS(object)))
    cat(paste("  Original class: '",CLASS(object),"' ",sep=""),"\n")
  cat(paste("  xts Attributes: "),"\n")
  str(xtsAttributes(object),...)
  }
}

