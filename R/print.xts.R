`print.xts` <-
function(x,...) {
  x.tmp <- x
  attributes(x.tmp) <- NULL
  zoo:::print.zoo(structure(x.tmp,
                            class='zoo',
                            index=index(x),
                            dim=dim(x),dimnames=dimnames(x)),...)
}

`[.xts` <-
function(x, i, j, drop = TRUE, ...) 
{
    original.cols <- ncol(x)
    original.names <- colnames(x)
    original.attr <- attributes(x)[!names(attributes(x)) %in% c('dim','dimnames','index','class')]
    if(length(original.attr) < 1) original.attr <- NULL

    class(x) <- "zoo"

    if (missing(i)) 
        i <- 1:nrow(x)
    if (is.character(i)) {
      # enables subsetting by date style strings
      # must be able to process - and then allow for operations???
      # supported formats:
      #   CCYY-MM-DD
      #   CCYY/MM/DD
      if(!identical(grep("::",i),integer(0))) {
        # range operator
        dates <- strsplit(i,'::')[[1]]
        by <- ifelse(length(dates)==3,dates[3],median(diff(time(x))))
        if(inherits(index(x),'Date')) {
          dates <- as.Date(dates)
        } else dates <- do.call(paste('as',class(index(x))[2],sep='.'),list(dates))
        dates <- seq(dates[1],dates[2],by=by)
      } else {
        # if single date is given - get start and end points if resolution of
        # series is greater than the time specified
        dates <- i
        if(inherits(index(x),'Date')) {
          dates <- as.Date(dates)
        } else dates <- do.call(paste('as',class(index(x))[2],sep='.'),list(dates))
      }      
      
      i <- which(index(x) %in% dates)
    }


    if (missing(j)) {
        x <- x[i = i, drop = drop, ...]
        if(!is.null(original.attr)) {
          for(i in 1:length(original.attr)) {
            attr(x,names(original.attr)[i]) <- original.attr[[i]]
          }
        }
        class(x) <- c("xts", "zoo")
        j <- 1:original.cols
    }
    else {
        x <- x[i = i, j = j, drop = drop, ...]
        if (is.null(dim(x))) 
            dim(x) <- c(NROW(x), NCOL(x))
        if(!is.null(original.attr)) {
          for(i in 1:length(original.attr)) {
            attr(x,names(original.attr)[i]) <- original.attr[[i]]
          }
        }
        class(x) <- c("xts", "zoo")
    }
    if (!is.null(dim(x))) 
        colnames(x) <- original.names[j]
    x
}

