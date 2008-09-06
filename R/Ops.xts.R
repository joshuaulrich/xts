`Ops.xts` <-
function (e1, e2) 
{
    # code borrowed heavily from Ops.zoo
    if(!missing(e2))
      if(is.xts(e2)) XTS <- e2

    if(is.xts(e1)) XTS <- e1

    e <- if (missing(e2)) {
        NextMethod(.Generic)
    }
    else if (any(nchar(.Method) == 0)) {
        NextMethod(.Generic)
    }
    else {
        #merge(e1, e2, all = FALSE, retclass = NULL)
        merge.xts0(e1, e2, all = FALSE, retclass = NULL)
        indx <- .index(e1)
        indxCLASS <- indexClass(e1)
        e1 <- coredata(e1)
        e2 <- coredata(e2)
        switch(.Generic,
               "+"=e1+e2,
               "-"=e1-e2,
               "*"=e1*e2,
               "/"=e1/e2)
        #NextMethod(.Generic) ... some reason this is super-slow?!!!? (20x)
    }
    if (is.null(attr(e, "index"))) 
        e <- .xts(e, indx, .indexCLASS=indxCLASS)#, attr(XTS, "frequency"))
    else e <- .xts(e, .index(XTS), .indexCLASS=indexClass(XTS))

    e
}

