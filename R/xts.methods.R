`.subset.xts` <- `[.xts` <-
function(x, i, j, drop = TRUE, ...) 
{
    sys.TZ <- Sys.getenv('TZ') 
    Sys.setenv(TZ='GMT')
    on.exit(Sys.setenv(TZ=sys.TZ))

    original.cols <- NCOL(x)
    original.attr <- xtsAttributes(x)
    
    # test for negative subscripting in i
    if (!missing(i) && is.numeric(i) ) {
      if(any(i < 0)) {
        if(!all(i < 0))
          stop('only zeros may be mixed with negative subscripts')
        i <- (1:NROW(x))[i]
      }
    }

    # test for negative subscripting in j
    if (!missing(j) && is.numeric(j) && any(j < 0)) {
      if(!all(j < 0))
        stop('only zeros may be mixed with negative subscripts')
      j <- (1:NCOL(x))[j]
    }

    if (missing(i)) 
      # this is horribly wasteful  FIXME
      i <- 1:NROW(x)
    
    if (timeBased(i)) 
      # this shouldn't happen either, though less important I suspect  FIXME
      i <- as.character(as.POSIXct(i)) 

    if(is.logical(i))
      i <- which(i) #(1:NROW(x))[rep(i,length.out=NROW(x))]

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
                   seq.int(binsearch(first.time, .index(x),  TRUE),
                           binsearch(last.time,  .index(x), FALSE))
                  )
      }
      i <- i.tmp
    }
  
    if(!isOrdered(i,strictly=FALSE)) {
      i <- sort(i)
      # possibly can test for dup=TRUE here, if we implement a global option
    }
    # .subset is picky, 0's in the 'i' position cause failures -- is this still nec? -jar
    zero.index <- binsearch(0, i, NULL)
    if(!is.na(zero.index))
      i <- i[ -zero.index ]

    if (missing(j)) {
      if(length(x)==0) {
        return(.xts(rep(NA,length(i)), .index(x)[i]))
      } else 
      return(.Call('do_subset_xts', x, as.integer(i), as.integer(1:original.cols), PACKAGE='xts'))
#      if(!is.null(original.attr)) {
#        for(ii in 1:length(original.attr)) {
#          attr(x,names(original.attr)[ii]) <- original.attr[[ii]]
#          if(names(original.attr)[ii]=='.ROWNAMES') attr(x,'.ROWNAMES') <- original.attr[[ii]][i]
#        }
#      }
      #if(!is.null(original.cols)) j <- 1:original.cols # -- this is dead
    }
    else {
        j <- sapply(j, function(xx) {
                         if(is.character(xx)) {
                           which(xx==colnames(x))
                         } else xx
                       })
        return(.Call('do_subset_xts', x, as.integer(i), as.integer(j), PACKAGE='xts'))
#        if(!is.null(original.attr)) {
#          for(ii in 1:length(original.attr)) {
#            attr(x,names(original.attr)[ii]) <- original.attr[[ii]]
#          }
#        }
    }
#    x
}

# Replacement method for xts objects
#
# Adapted from [.xts code, making use of NextGeneric as
# replacement function in R already preserves all attributes
# and index value is left untouched

`[<-.xts` <-
#`xtsreplacement` <-
function(x, i, j, value) 
{
    sys.TZ <- Sys.getenv('TZ') 
    Sys.setenv(TZ='GMT')
    on.exit(Sys.setenv(TZ=sys.TZ))
    if (!missing(i)) {

    if (timeBased(i)) 
      # this shouldn't happen either, though less important I suspect  FIXME
      i <- as.character(as.POSIXct(i)) 

    if(is.logical(i))
      i <- which(i) #(1:NROW(x))[rep(i,length.out=NROW(x))]

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
                   seq.int(binsearch(first.time, .index(x),  TRUE),
                           binsearch(last.time,  .index(x), FALSE))
                  )
      }
      i <- i.tmp
    }
  
    # .subset is picky, 0's in the 'i' position cause failures -- is this still nec? -jar
    zero.index <- binsearch(0, i, NULL)
    if(!is.na(zero.index))
      i <- i[ -zero.index ]
    }

    .Class <- "matrix"
    NextMethod(.Generic)
}
