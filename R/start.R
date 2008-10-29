`start.xts` <-
function(x, ...) {
  index(x[1,])
}

`end.xts` <-
function(x, ...) {
  if(length(x)==0) {
    index(x[length(.index(x)),])
  } else 
  index(x[NROW(x),])
}
