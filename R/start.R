`start.xts` <-
function(x, ...) {
  index(x[1,])
}

`end.xts` <-
function(x, ...) {
  index(x[NROW(x),])
}
