axTicksByTime <- function(ival, ticks.on, k=1, gt = 2, lt = 30) {
    tick.opts <- c("years", "months", "weeks", "days", "hours", 
        "minutes", "seconds")
    tick.k.opts <- c(10, 5, 2, 1, 7, 1, 1, 1, 4, 2, 1, 30, 15, 
        1, 1)
    if (ticks.on %in% tick.opts) {
        cl <- ticks.on[1]
        ck <- k
        ep <- endpoints(ival, cl, ck) 
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
    }   
    ep  
}
