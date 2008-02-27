# functions to handle timeSeries <--> xts conversions

`re.timeSeries` <-
function(x,...) {
  stopifnot("package:fSeries" %in% search() || require("fSeries", quietly=TRUE))

  # strip all non-'core' attributes so they're not attached to the Data slot
  x.attr <- attributes(x)
  xx <- structure(x,dimnames=x.attr$dimnames,index=x.attr$index)
  original.attr <- attributes(x)[!names(attributes(x)) %in% 
                                 c("dim","dimnames","index","class")]
  for(i in names(original.attr)) {
    attr(xx,i) <- NULL
  }

  timeSeries(xx,charvec=as.character(index(xx)),format=x.attr$format,
             zone=x.attr$FinCenter,FinCenter=x.attr$FinCenter,
             recordIDs=x.attr$recordIDs,title=x.attr$title,
             documentation=x.attr$documentation,...)
}

`as.xts.timeSeries` <-
function(x,dateFormat="POSIXct",FinCenter,recordIDs,title,documentation,...) {

  if(missing(FinCenter))
    FinCenter <- x@FinCenter
  if(missing(recordIDs))
    recordIDs <- x@recordIDs
  if(missing(title)) 
    title <- x@title
  if(missing(documentation)) 
    documentation <- x@documentation

  order.by <- do.call(paste('as',dateFormat,sep='.'),list(x@positions))

  xts(as.matrix(x@Data),
      order.by=order.by,
      format=x@format,
      FinCenter=FinCenter,
      recordIDs=recordIDs,
      title=title,
      documentation=documentation,
      .CLASS='timeSeries',
      .CLASSnames=c('FinCenter','recordIDs','title','documentation','format'),
      ...)
}

`xts.as.timeSeries` <- function(x,...) {}
