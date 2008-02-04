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
  cat(paste("An",sQuote('xts'),"object from",start(object),"to",end(object),"containing:\n"))
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
    original.cols <- NCOL(x)
    original.names <- colnames(x)
    original.CLASS <- CLASS(x)

    sys.TZ <- Sys.getenv('TZ') 
    Sys.setenv(TZ='GMT')


    original.attr <- attributes(x)[!names(attributes(x)) %in% c('dim','dimnames','index','class')]
    if(length(original.attr) < 1) original.attr <- NULL


    if (missing(i)) 
        i <- 1:NROW(x)
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
        if(original.cols == 1) {
          # if data set only has one column:
          # it is necessary to replace the dimnames removed by [.zoo
          dn1 <- dimnames(x)[[1]]
          x <- x[i = i, drop = drop, ...]
          dim(x) <- c(NROW(x), NCOL(x))
          dn <- list(dn1[i],colnames(x))
          dimnames(x) <- dn
        } else {
          x <- x[i = i, drop = drop, ...]
        }

        if(!is.null(original.attr)) {
          for(ii in 1:length(original.attr)) {
            attr(x,names(original.attr)[ii]) <- original.attr[[ii]]
          }
        }
        class(x) <- original.class
        if(!is.null(original.cols)) j <- 1:original.cols
    }
    else {
        if(length(j) == 1) {
          # subsetting down to 1 cols - '[.zoo' will delete this info
          dn1 <- dimnames(x)[[1]]
          x <- x[i = i, j = j, drop = drop, ...]
          dim(x) <- c(NROW(x), NCOL(x))
          dn <- list(dn1[i],colnames(x))
          dimnames(x) <- dn
        } else {
          x <- x[i = i, j = j, drop = drop, ...]
        }

        if(!is.null(original.attr)) {
          for(ii in 1:length(original.attr)) {
            attr(x,names(original.attr)[ii]) <- original.attr[[ii]]
          }
        }
        # handle future xts extensions without [. method rewrite
        class(x) <- original.class
    }
    if (!is.null(dim(x))) 
        colnames(x) <- original.names[j]

    indexClass(x) <- original.indexclass

    Sys.setenv(TZ=sys.TZ)

    CLASS(x) <- original.CLASS
    x
}

