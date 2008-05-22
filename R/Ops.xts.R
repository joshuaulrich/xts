`Ops.xts` <-
function (e1, e2) 
{
    # code borrowed heavily from Ops.zoo
    e <- if (missing(e2)) {
        NextMethod(.Generic)
    }
    else if (any(nchar(.Method) == 0)) {
        NextMethod(.Generic)
    }
    else {
        merge(e1, e2, all = FALSE, retclass = NULL)
        NextMethod(.Generic)
    }
    if(is.zoo(e2)) XTS <- e2
    if(is.zoo(e1)) XTS <- e1

    if (is.null(attr(e, "index"))) 
        e <- xts(coredata(e), index(XTS), attr(XTS, "frequency"))
    else e <- xts(coredata(e),index(e))

    e
}

