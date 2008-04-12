axTicksByTime <- function(x, ticks.on='auto', k=1, 
                          labels=TRUE, format.labels=TRUE, ends=TRUE,
                           gt = 2, lt = 30) {
    # if a vector of times/dates, convert to dummy xts object
    if(timeBased(x)) x <- xts(rep(1,length(x)),x)
    
    tick.opts <- c("years", "months", "weeks", "days", "hours", 
        "minutes", "seconds")
    tick.k.opts <- c(10, 5, 2, 1, 7, 1, 1, 1, 4, 2, 1, 30, 15, 
        1, 1)
    if (ticks.on %in% tick.opts) {
        cl <- ticks.on[1]
        ck <- k
    }   
    else {
        tick.opts <- paste(rep(tick.opts, c(4, 2, 1, 1, 3, 3,  
            1)), tick.k.opts)
        is <- structure(rep(0,length(tick.opts)),.Names=tick.opts)
        for(i in 1:length(tick.opts)) {
          y <- strsplit(tick.opts[i],' ')[[1]]
          ep <-endpoints(x,y[1],as.numeric(y[2]))
          is[i] <- length(ep) -1
          if(is[i] > lt) 
            break
        }   
        nms <- rev(names(is)[which(is > gt & is < lt)])[1]
        cl <- strsplit(nms, " ")[[1]][1]
        ck <- as.numeric(strsplit(nms, " ")[[1]][2])
    }   

    if (is.null(cl)) {
        ep <- NULL
    } else  ep <- endpoints(x, cl, ck) 
    if(ends)
      ep <- ep + c(rep(1,length(ep)-1),0)
    if(labels) {
      if(format.labels) {
        # format by level of time detail
        time.scale <- periodicity(x)$scale
        x.labels <- format(index(x)[ep], "%n%b%n%Y")
        if (time.scale == "weekly" | time.scale == "daily") 
            x.labels <- format(index(x)[ep], "%b %d%n%Y")
        if (time.scale == "minute" | time.scale == "hourly") 
            x.labels <- format(index(x)[ep], "%b %d%n%H:%M")
        if (time.scale == "seconds")
            x.labels <- format(index(x)[ep], "%b %d%n%H:%M:%S")
        names(ep) <- x.labels
      } else names(ep) <- as.character(index(x)[ep])
    }
    ep  
}
axTicksByTime0 <- function(ival, ticks.on='auto', k=1, labels=TRUE, ends=FALSE, gt = 2, lt = 30) {
    
    tick.opts <- c("years", "months", "weeks", "days", "hours", 
        "minutes", "seconds")
    tick.k.opts <- c(10, 5, 2, 1, 7, 1, 1, 1, 4, 2, 1, 30, 15, 
        1, 1)
    if (ticks.on %in% tick.opts) {
        cl <- ticks.on[1]
        ck <- k
    }   
    else {
        tick.opts <- paste(rep(tick.opts, c(4, 2, 1, 1, 3, 3,  
            1)), tick.k.opts)
        is <- structure(rep(0,length(tick.opts)),.Names=tick.opts)
        for(i in 1:length(tick.opts)) {
          y <- strsplit(tick.opts[i],' ')[[1]]
          ep <-endpoints(ival,y[1],as.numeric(y[2]))
          is[i] <- length(ep) -1
          if(is[i] > lt) 
            break
        }   
        nms <- rev(names(is)[which(is > gt & is < lt)])[1]
        cl <- strsplit(nms, " ")[[1]][1]
        ck <- as.numeric(strsplit(nms, " ")[[1]][2])
    }   

    if (is.null(cl)) {
        ep <- NULL
    } else  ep <- endpoints(ival, cl, ck) 
    if(ends)
      ep <- ep + c(rep(1,length(ep)-1),0)
    if(labels) 
      names(ep) <- as.character(index(ival)[ep])
    ep  
}
