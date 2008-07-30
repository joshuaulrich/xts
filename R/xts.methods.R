`[.xts` <-
function(x, i, j, drop = TRUE, ...) 
{

DEBUG <- TRUE
AT <- FALSE # This is for internal testing at present will remove soon

    original.indexclass <- indexClass(x)
    original.class <- class(x)
    original.cols <- NCOL(x)
    original.names <- colnames(x)
    original.CLASS <- CLASS(x)

    sys.TZ <- Sys.getenv('TZ') 
    Sys.setenv(TZ='GMT')

    DIM <- dim(x)

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
          
          # the first index value to be found
          if(is.na(tBR[1])) {
            first.time <- rindex(x)[1]
          } else first.time <- tBR[1]

          # the last index value ot be found
          if(is.na(tBR[2])) {
            last.time  <- rindex(x)[NROW(x)]
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
                   seq.int(binsearch(attr(x, 'index'), first.time, TRUE),
                           binsearch(attr(x, 'index'), last.time, FALSE))
                  )
      }
      i <- i.tmp
    }
  
START <- Sys.time() 
    # .subset is picky, 0's in the 'i' position cause failures
    zero.index <- binsearch(i, 0, NULL)
    if(!is.na(zero.index))
      i <- i[ -zero.index ]

    if (missing(j)) {
        dn1 <- dimnames(x)[[1]]
        x.index <- attr(x, 'index')[i]
        cat("SUBSET index: ", Sys.time() - START,"\n")
        if(is.null(DIM)) {
          x.tmp <- .subset(x, i=i, drop=drop)
        }
        else x.tmp <- .subset(x, i=i, j=1:original.cols, drop=drop)
        cat("SUBSET data: ", Sys.time() - START,"\n")
        rm(x)
        x <- x.tmp
        rm(x.tmp)
        if(AT) {  
          attr(x, 'index') <- x.index
          dim(x) <- c(length(i), original.cols)
          dn <- list(dn1[i],original.names)
          dimnames(x) <- dn
        } 
        cat("GLUE dimnames: ", Sys.time() - START,"\n")
        dim2 <- original.cols

      if(!is.null(original.attr)) {
        for(ii in 1:length(original.attr)) {
          attr(x,names(original.attr)[ii]) <- original.attr[[ii]]
          if(names(original.attr)[ii]=='.ROWNAMES') attr(x,'.ROWNAMES') <- original.attr[[ii]][i]
        }
      }

      if(DEBUG) cat("GLUE all attributes back: ", Sys.time() - START,"\n")

      #class(x) <- original.class
      if(!is.null(original.cols)) j <- 1:original.cols
    }
    else {

        if(DEBUG) cat("Processing j: ", Sys.time() - START,"\n")

        j <- sapply(j, function(xx) {
                         if(is.character(xx)) {
                           which(xx==colnames(x))
                         } else xx
                       })
        dn1 <- dimnames(x)[[1]]
        x.index <- attr(x, 'index')[i]
        if(is.null(DIM)) {
          x.tmp <- .subset(x, i=i, drop=drop)
        }
        else x.tmp <- .subset(x, i=i, j=j, drop=drop)
        rm(x)
        x <- x.tmp
        rm(x.tmp)
        if(AT) {
          attr(x, 'index') <- x.index
          dim(x) <- c(length(i), length(j))
          dn <- list(dn1[i],original.names[j])
          dimnames(x) <- dn
        }
        dim2 <- length(j)
        if(!is.null(original.attr)) {
          for(ii in 1:length(original.attr)) {
            attr(x,names(original.attr)[ii]) <- original.attr[[ii]]
          }
        }
        #class(x) <- original.class
    }
if(AT) {
    class(x) <- original.class
if(DEBUG) cat("Set class: ", Sys.time() - START,"\n")
    #if (!is.null(dim(x))) 
    #    colnames(x) <- original.names[j]
if(DEBUG) cat("Set colnames: ", Sys.time() - START,"\n")

    indexClass(x) <- original.indexclass
if(DEBUG) cat("Set indexClass: ", Sys.time() - START,"\n")

    Sys.setenv(TZ=sys.TZ)
if(DEBUG) cat("Set Sys.setenv: ", Sys.time() - START,"\n")

    #attributes(x) <- list(.CLASS=original.CLASS, .indexCLASS=original.indexclass)
    CLASS(x) <- original.CLASS
if(DEBUG) cat("Set CLASS: ", Sys.time() - START,"\n")
} else {
    attributes(x) <- c(attributes(x),list(index=x.index, dim=c(length(i), dim2),
                       dimnames=list(dn1[i],original.names[j]),class=original.class,
                       .indexCLASS=original.indexclass, .CLASS=original.CLASS))
if(DEBUG) cat("Set colnames, indexClass and CLASS all at once: ", Sys.time()-START,"\n")
}
    x
}

