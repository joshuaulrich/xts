as.environment.xts <- function(x) {
  e <- new.env()
  lapply(1:NCOL(x), function(.) assign(colnames(x)[.], x[,.],envir=e))
  e
}

