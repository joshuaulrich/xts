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
      if(is.logical(format.labels) || is.character(format.labels)) {
        # format by level of time detail, and platform 
        unix <- ifelse(.Platform$OS.type=="unix", TRUE, FALSE)

        time.scale <- periodicity(x)$scale
        fmt <- ifelse(unix, '%n%b%n%Y', '%b %Y')

        if (time.scale == "weekly" | time.scale == "daily") 
          fmt <- ifelse(unix, '%b %d%n%Y', '%b %d %Y')

        if (time.scale == "minute" | time.scale == "hourly") 
          fmt <- ifelse(unix, '%b %d%n%H:%M', '%b %d %H:%M')

        if (time.scale == "seconds")
          fmt <- ifelse(unix, '%b %d%n%H:%M:%S', '%b %d %H:%M:%S')

        if(is.character(format.labels)) fmt <- format.labels
        names(ep) <- format(index(x)[ep],fmt)
      } else names(ep) <- as.character(index(x)[ep])
    }
    ep  
}
