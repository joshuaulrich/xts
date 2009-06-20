split.xts <-
function(x, f="months", drop=FALSE, k=1, ...) {
  if(is.character(f)) {
    ep <- endpoints(x, on=f, k=k)
    sp <- (ep + 1)[-length(ep)]
    ep <- ep[-1]
    lapply(1:length(ep), function(X) x[sp[X]:ep[X]])
  } else
  NextMethod("split")
}

