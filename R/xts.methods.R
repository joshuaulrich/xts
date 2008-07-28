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

    # temporarily convert back to POSIXct.  This is what I am eliminating
    POSIXindex <- as.POSIXct(attr(x, 'index'))

    if (missing(i)) 
      # this is horribly wasteful  FIXME
      i <- 1:NROW(x)

    if (timeBased(i)) 
      # this shouldn't happen either, though less important I suspect  FIXME
      i <- as.character(as.POSIXct(i)) 

    if (is.character(i)) {
      # enables subsetting by date style strings
      # must be able to process - and then allow for operations???

      i.tmp <- NULL
      for(ii in i) {
        if(!identical(grep("(::)|/",ii),integer(0))) {
          tBR <- as.POSIXct(timeBasedRange(ii), origin="1970-01-01") # this will be left to numeric soon
          first.time <- tBR[1]
          last.time  <- tBR[2]
          # range operator
#          dates <- strsplit(ii,'(::)|/')[[1]]
#          
#          # test for single side range operation
#          first.time <- ifelse(dates[1]=="",
#                               POSIXindex[1],
#                               do.call('firstof',
#                                        as.list(as.numeric(strsplit(dates[1],':|-|/| ')[[1]]))))
#          last.time <- ifelse(length(dates)==1,
#                               POSIXindex[length(POSIXindex)],
#                               do.call('lastof',
#                                        as.list(as.numeric(strsplit(dates[2],':|-|/| ')[[1]]))))
        } else {
          # if single date is given - get start and end points if resolution of
          # series is greater than the time specified
          dates <- ii
          first.time <- do.call('firstof',
                                as.list(as.numeric(strsplit(dates,':|-|/| ')[[1]])))
          last.time <- do.call('lastof',
                                as.list(as.numeric(strsplit(dates,':|-|/| ')[[1]])))
        }      
        
        # this is probably the cleanest place to add binarySearch, as the full rowsearch is BAD!
        i.tmp <- c(i.tmp,which(POSIXindex <= last.time & POSIXindex >= first.time))
      }
      i <- i.tmp
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

