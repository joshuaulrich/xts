`[.xts` <-
function(x, i, j, drop = TRUE, ...) 
{
startTime <- Sys.time()
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
    #POSIXindex <- as.POSIXct(attr(x, 'index'))

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
          tBR <- timeBasedRange(ii)
          first.time <- tBR[1]
          last.time  <- tBR[2]
        } else {
          # if single date is given - get start and end points if resolution of
          # series is greater than the time specified
          dates <- ii
          first.time <- as.numeric(do.call('firstof',
                                as.list(as.numeric(strsplit(dates,':|-|/| ')[[1]]))))
          last.time <- as.numeric(do.call('lastof',
                                as.list(as.numeric(strsplit(dates,':|-|/| ')[[1]]))))
        }      
        
        # this is probably the cleanest place to add binarySearch, as the full rowsearch is BAD!
        #i.tmp <- c(i.tmp,which(POSIXindex <= last.time & POSIXindex >= first.time))
        i.tmp <- c(i.tmp,
                   seq.int(binsearch(attr(x, 'index'), first.time, TRUE),
                           binsearch(attr(x, 'index'), last.time, FALSE))
                  )
      }
      i <- i.tmp
    }
cat('calculate i',Sys.time()-startTime,'\n')

    if (missing(j)) {
        if(original.cols == 1) {
          # if data set only has one column:
          # it is necessary to replace the dimnames removed by [.zoo
          dn1 <- dimnames(x)[[1]]
          x.index <- attr(x, 'index')[i]
          nr <- NROW(x)
          #attributes(x) <- NULL
          #dim(x) <- c(NROW(x), 1)
cat('get all attributes for later',Sys.time()-startTime,'\n')
          #x.tmp <- x[i=i, drop=drop, ...]
          x.tmp <- .subset(x, i=i, drop=drop)
          rm(x)
          x <- x.tmp
cat('perform subset ',Sys.time()-startTime,'\n')
          attr(x, 'index') <- x.index
          dim(x) <- c(NROW(x), NCOL(x))
          dn <- list(dn1[i],colnames(x))
          dimnames(x) <- dn
cat('add dimensions and dimnames ',Sys.time()-startTime,'\n')
        } else {
          #x <- x[i = i, drop = drop, ...]
          x.index <- attr(x, 'index')[i]
          x <- coredata(x)[i = i, drop = drop, ...]
          attr(x, 'index') <- x.index
        }

        if(!is.null(original.attr)) {
            for(ii in 1:length(original.attr)) {
              attr(x,names(original.attr)[ii]) <- original.attr[[ii]]
              if(names(original.attr)[ii]=='.ROWNAMES') attr(x,'.ROWNAMES') <- original.attr[[ii]][i]
            }
        }
cat('added back attributes',Sys.time()-startTime,'\n')
        class(x) <- original.class
        if(!is.null(original.cols)) j <- 1:original.cols
cat('added back class and cols',Sys.time()-startTime,'\n')
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
          x.index <- attr(x, 'index')[i]
          nr <- NROW(x)
          nc <- NCOL(x)
          #x <- x[i = i, j = j, drop = drop, ...]
          x.tmp <- .subset(x, i=i, j=j, drop=drop)
          rm(x)
          x <- x.tmp
          attr(x, 'index') <- x.index
          dim(x) <- c(NROW(x), NCOL(x))
          dn <- list(dn1[i],colnames(x))
          dimnames(x) <- dn
        } else {
          #x <- x[i = i, j = j, drop = drop, ...]
          x <- .subset(x, i=i, j=j, drop=drop)
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

