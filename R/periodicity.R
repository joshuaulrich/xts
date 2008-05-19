# functions on the move from quantmod to xts
# all should remain unexported until transition
# is complete - jar
#
# periodicity
# print.periodicity
# apply.daily
# apply.weekly
# apply.monthly
# apply.quarterly
# apply.yearly

`periodicity` <-
function (x, ...) 
{
    if(!is.xts(x)) x <- as.xts(x)

    # convert if necessary to usable format
    if(!indexClass(x)[[1]] %in% c('Date','POSIXt')) indexClass(x) <- "POSIXct"

    # this takes a long time on big data - possibly use some sort of sampling instead???
    p <- median(diff(time(x)))

    if (is.na(p)) 
        stop("cannot calculate periodicity from one observation")

    p.numeric <- as.numeric(p)
    units <- attr(p, "units")

    if (units == "secs") {
        scale <- "seconds"
    }
    if (units == "mins") {
        scale <- "minute"
        if (p.numeric > 59) 
            scale <- "hourly"
    }
    if (units == "hours") {
        scale <- "hourly"
    }
    if (units == "days") {
        scale <- "daily"
        if (p.numeric > 1) 
            scale <- "weekly"
        if (p.numeric > 7) 
            scale <- "monthly"
        if (p.numeric > 31) 
            scale <- "quarterly"
        if (p.numeric > 91) 
            scale <- "yearly"
    }

    structure(list(difftime = p, frequency = p.numeric, start = index(first(x)), 
        end = index(last(x)), units = units, scale = scale),class="periodicity")
#    class(xx) <- "periodicity"
#    xx  # used when structure was assigned to xx, useless now, remain until testing is done though -jar
}

`print.periodicity` <-
function (x, ...) 
{
    x.freq <- ifelse(x$scale %in% c("minute", "seconds"), x$frequency, 
        "")
    if (x.freq == "") {
        cap.scale <- paste(toupper(substring(x$scale, 1, 1)), 
            substring(x$scale, 2), sep = "")
        cat(paste(cap.scale, "periodicity from", x$start, "to", 
            x$end, "\n", sep = " "))
    }
    else {
        cat(paste(x.freq, x$scale, "periodicity from", x$start, 
            "to", x$end, "\n", sep = " "))
    }
}

`period.apply` <-
function(x, INDEX, FUN, ...)
{
    x <- try.xts(x, error = FALSE)
    FUN <- match.fun(FUN)
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
                   FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
                })  
    reclass(xx, x[INDEX])  
}


`period.apply.original` <-
function (x, INDEX, FUN, ...) 
{
  x <- use.xts(x,error=FALSE)

  if(!is.xts(x)) {
    FUN <- match.fun(FUN)
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
          FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
    })
  } else {
    FUN <- match.fun(FUN)
    new.index <- index(x)[INDEX]
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
          FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
    })
    xx <- xts(xx,new.index)
    CLASS(xx) <- CLASS(x)
    xtsAttributes(xx) <- xtsAttributes(x)
    xx <- reclass(xx)
  }
  xx
}

`apply.daily` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'days')
  period.apply(x,ep,FUN, ...)
}
`apply.weekly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'weeks')
  period.apply(x,ep,FUN, ...)
}

`apply.monthly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'months')
  period.apply(x,ep,FUN, ...)
}

`apply.quarterly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'quarters')
  period.apply(x,ep,FUN, ...)
}

`apply.yearly` <-
function(x,FUN, ...)
{
  ep <- endpoints(x,'years')
  period.apply(x,ep,FUN, ...)
}
