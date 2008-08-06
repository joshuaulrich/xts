`[.xts` <-
function(x, i, j, drop = TRUE, ...) 
{
    sys.TZ <- Sys.getenv('TZ') 
    Sys.setenv(TZ='GMT')

    original.cols <- NCOL(x)
    original.attr <- xtsAttributes(x)

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
          
          # the first index value to be found
          if(is.na(tBR[1])) {
            first.time <- .index(x)[1]
          } else first.time <- tBR[1]

          # the last index value ot be found
          if(is.na(tBR[2])) {
            last.time  <- .index(x)[NROW(x)]
          } else last.time <- tBR[2]

        } else {
          # if single date is given - get start and end points if resolution of
          # series is greater than the time specified
          dates <- paste(ii,ii,sep='/')
          tBR <- timeBasedRange(dates)
          first.time <- tBR[1]
          last.time  <- tBR[2]
        }      
        
        i.tmp <- c(i.tmp,
                   seq.int(binsearch(first.time, attr(x, 'index'), TRUE),
                           binsearch(last.time, attr(x, 'index'), FALSE))
                  )
      }
      i <- i.tmp
    }
  
    # .subset is picky, 0's in the 'i' position cause failures
    zero.index <- binsearch(0, i, NULL)
    if(!is.na(zero.index))
      i <- i[ -zero.index ]

    if (missing(j)) {
      x <- .Call('xtsSubset', x, as.integer(i), as.integer(1:original.cols), PACKAGE='xts')
      if(!is.null(original.attr)) {
        for(ii in 1:length(original.attr)) {
          attr(x,names(original.attr)[ii]) <- original.attr[[ii]]
          if(names(original.attr)[ii]=='.ROWNAMES') attr(x,'.ROWNAMES') <- original.attr[[ii]][i]
        }
      }
      if(!is.null(original.cols)) j <- 1:original.cols
    }
    else {
        j <- sapply(j, function(xx) {
                         if(is.character(xx)) {
                           which(xx==colnames(x))
                         } else xx
                       })
        x <- .Call('xtsSubset', x, as.integer(i), as.integer(j), PACKAGE='xts')
        if(!is.null(original.attr)) {
          for(ii in 1:length(original.attr)) {
            attr(x,names(original.attr)[ii]) <- original.attr[[ii]]
          }
        }
    }
    x
}
