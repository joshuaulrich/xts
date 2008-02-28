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

`na.omit.xts` <- function(object, ...) {
  xx <- stats:::na.omit.default(object,...)
  naa <- attr(xx,'na.action')
  naa.index <- index(object)[naa]

  ROWNAMES <- attr(object,'.ROWNAMES')
  if(!is.null(ROWNAMES)) {
    naa.rownames <- ROWNAMES[naa]
  } else naa.rownames <- NULL

  attr(xx,'na.action') <- structure(naa,
                                    index=naa.index,
                                    .ROWNAMES=naa.rownames)
  return(xx) 
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

    #POSIXindex <- tindex(x,'POSIXct')  attempt to remove tindex...
    indexClass(x) <- "POSIXct"
    POSIXindex <- index(x)

    if (missing(i)) 
        i <- 1:NROW(x)
    if (is.character(i)) {
      # enables subsetting by date style strings
      # must be able to process - and then allow for operations???

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
              if(names(original.attr)[ii]=='.ROWNAMES') attr(x,'.ROWNAMES') <- original.attr[[ii]][i]
            }
        }
        class(x) <- original.class
        if(!is.null(original.cols)) j <- 1:original.cols
    }
    else {
        j <- sapply(j, function(xx) {
                         if(is.character(xx)) {
                           which(xx==colnames(x))
                         } else xx
                       })
        if(length(j) == 1) { # fix loss of column names when using colnames to subset
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

`rbind.xts` <-
function(..., deparse.level=1) {

 args <- list(...)

 # Store original class attributes
 xts.ROWNAMES <- sapply(args, function(x) attr(x, ".ROWNAMES"))
 xts.CLASS <- sapply(args, CLASS)
 xts.CLASSattr <- sapply(args, xtsAttributes, user=FALSE)
 xts.USERattr <- unlist(sapply(args, xtsAttributes, user=TRUE))

 # Bind objects
 ret <- zoo:::rbind.zoo(...)
 ret <- structure( ret, class=c('xts','zoo') )

 # Drop CLASS attribute if _not_ the same for all objects
 xts.CLASS.eq <- sapply(xts.CLASS, function(x) identical(x,xts.CLASS[[1]]))
 if( all(xts.CLASS.eq) ) {
   CLASS(ret) <- xts.CLASS[[1]]
   xts.CLASSattr <- xts.CLASSattr[[1]]
 } else {
   CLASS(ret) <- NULL
   xts.CLASSattr <- NULL
 }

 # Re-attach _xts_ attributes
 # Need a better way to deal with different xtsAttributes than
 # simply assigning them to the value of the first object...
 xtsAttributes(ret) <- c( xts.CLASSattr, xts.USERattr )

 return(ret)
}

`cbind.xts` <-
function(..., deparse.level=1) {
 args <- list(...)

 # Store original class attributes
 xts.ROWNAMES <- sapply(args, function(x) attr(x, ".ROWNAMES"))
 xts.CLASS <- sapply(args, CLASS)
 xts.CLASSattr <- sapply(args, xtsAttributes, user=FALSE)
 xts.USERattr <- unlist(sapply(args, xtsAttributes, user=TRUE))

 # Bind objects
 ret <- zoo:::cbind.zoo(...)
 ret <- structure( ret, class=c('xts','zoo') )

 # Drop CLASS attribute if _not_ the same for all objects
 xts.CLASS.eq <- sapply(xts.CLASS, function(x) identical(x,xts.CLASS[[1]]))
 if( all(xts.CLASS.eq) ) {
   CLASS(ret) <- xts.CLASS[[1]]
   xts.CLASSattr <- xts.CLASSattr[[1]]
 } else {
   CLASS(ret) <- NULL
   xts.CLASSattr <- NULL
 }

 # Re-attach _xts_ attributes
 # Need a better way to deal with different xtsAttributes than
 # simply assigning them to the value of the first object...
 xtsAttributes(ret) <- c( xts.CLASSattr, xts.USERattr )

 return(ret)
}

`c.xts` <-
function(...) {
  rbind.xts(...)
}

