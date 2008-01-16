`print.xts` <-
function(x,...) {
  x.tmp <- x
  attributes(x.tmp) <- NULL
  zoo:::print.zoo(structure(x.tmp,
                            class='zoo',
                            index=index(x),
                            dim=dim(x),dimnames=dimnames(x)),...)
}

`str.xts` <-
function(object,...) {
  cat(paste("An",sQuote('xts'),"object containing:\n"))
  cat(paste("  Data:"))
  str(coredata(object),...)
  cat(paste("  Indexed by: "))
  str(index(object),...)
  if(!is.null(CLASS(object)))
    cat(paste("  Original class: '",CLASS(object),"' ",sep=""),"\n")
  cat(paste("  xts Attributes: "),"\n")
  str(xtsAttributes(object),...)
}

`[.xts` <-
function(x, i, j, drop = TRUE, ...) 
{
    original.indexclass <- indexClass(x)
    original.class <- class(x)
    original.cols <- ncol(x)
    original.names <- colnames(x)
    original.attr <- attributes(x)[!names(attributes(x)) %in% c('dim','dimnames','index','class')]
    if(length(original.attr) < 1) original.attr <- NULL


    if (missing(i)) 
        i <- 1:nrow(x)
    if (is.character(i)) {
      # enables subsetting by date style strings
      # must be able to process - and then allow for operations???

      #POSIXindex <- tindex(x,'POSIXct')  attempt to remove tindex...
      indexClass(x) <- "POSIXct"
      POSIXindex <- index(x)

      if(!identical(grep("::",i),integer(0))) {
        # range operator
        dates <- strsplit(i,'::')[[1]]
        
        # test for single side range operation
        first.time <- ifelse(dates[1]=="",
                             POSIXindex[1],
                             do.call('firstof',
                                      as.list(as.numeric(strsplit(dates[1],':|-|/| ')[[1]]))))
        last.time <- ifelse(length(dates)==1,
                             POSIXindex[length(POSIXindex)],
                             do.call('lastof',
                                      as.list(as.numeric(strsplit(dates[2],':|-|/| ')[[1]]))))
      } else {
        # if single date is given - get start and end points if resolution of
        # series is greater than the time specified
        dates <- i
        first.time <- do.call('firstof',
                              as.list(as.numeric(strsplit(dates,':|-|/| ')[[1]])))
        last.time <- do.call('lastof',
                              as.list(as.numeric(strsplit(dates,':|-|/| ')[[1]])))
      }      
      
      i <- which(POSIXindex <= last.time & POSIXindex >= first.time)
    }

    class(x) <- "zoo"

    if (missing(j)) {
        x <- x[i = i, drop = drop, ...]
        if(!is.null(original.attr)) {
          for(i in 1:length(original.attr)) {
            attr(x,names(original.attr)[i]) <- original.attr[[i]]
          }
        }
        class(x) <- original.class
        if(!is.null(original.cols)) j <- 1:original.cols
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
        # handle future xts extensions without [. method rewrite
        class(x) <- original.class
    }
    if (!is.null(dim(x))) 
        colnames(x) <- original.names[j]
    indexClass(x) <- original.indexclass
    x
}

