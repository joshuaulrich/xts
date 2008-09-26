`cbind.xts` <-
function(x, y, ..., all=TRUE, fill=NA, suffixes=NULL, join="outer") {
    merge.xts(x, y, ..., all=all, fill=fill, suffixes=suffixes, retclass="xts")
}

`c.xts` <-
function(x, y, ...) {
  rbind.xts(x, y, ...)
}


`rbind.xts` <-
function(x, y, ..., deparse.level=1) {
  .Call('do_rbind_xts',x, y)
}
