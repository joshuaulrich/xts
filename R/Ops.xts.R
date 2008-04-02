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
    if (is.null(attr(e, "index"))) 
        e <- xts(as.matrix(e), index(e1), attr(e1, "frequency"))
    else e <- xts(e,index(e1))
    e
}

