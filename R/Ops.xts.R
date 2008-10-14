`Ops.xts` <-
function(e1, e2)
{
  e <- if (missing(e2)) {
      switch(.Generic,
        "!"  = !unclass(e1),
        "-"  = -unclass(e1),
        "+"  = +unclass(e1))
  }
  else if (any(nchar(.Method) == 0)) {
      switch(.Generic,
        "+"   = unclass(e1) + unclass(e2),
        "-"   = unclass(e1) - unclass(e2),
        "*"   = unclass(e1) * unclass(e2),
        "/"   = unclass(e1) / unclass(e2),
        "^"   = unclass(e1) ^ unclass(e2),
        "%%"  = unclass(e1) %% unclass(e2),
        "%/%" = unclass(e1) %/% unclass(e2),
        "!="  = unclass(e1) == unclass(e2),
        ">"   = unclass(e1) > unclass(e2),
        "<"   = unclass(e1) < unclass(e2),
        ">="  = unclass(e1) >= unclass(e2),
        "<="  = unclass(e1) <= unclass(e2),
        "=="  = unclass(e1) == unclass(e2))
  }
  else {
    if( NROW(e1)==NROW(e2) && identical(.index(e1),.index(e2)) ) {
      switch(.Generic,
        "+"   = unclass(e1) + unclass(e2),
        "-"   = unclass(e1) - unclass(e2),
        "*"   = unclass(e1) * unclass(e2),
        "/"   = unclass(e1) / unclass(e2),
        "^"   = unclass(e1) ^ unclass(e2),
        "%%"  = unclass(e1) %% unclass(e2),
        "%/%" = unclass(e1) %/% unclass(e2),
        "!="  = unclass(e1) == unclass(e2),
        ">"   = unclass(e1) > unclass(e2),
        "<"   = unclass(e1) < unclass(e2),
        ">="  = unclass(e1) >= unclass(e2),
        "<="  = unclass(e1) <= unclass(e2),
        "=="  = unclass(e1) == unclass(e2))
    } else {
      nc1 <- 1:NCOL(e1)
      #E <- merge.xts0(e1, e2, all=FALSE, retclass=FALSE)
      #if(is.null(E)) 
      #  return(E)
      #e1 <- .subset.xts(E, j= nc1) #[,  nc1] 
      #e2 <- .subset.xts(E, j=-nc1) #E[, -nc1] 
      e1 <- merge(e1, .xts(,.index(e2)), all=FALSE, retclass=FALSE)
      e2 <- merge(e2, .xts(,.index(e1)), all=FALSE, retclass=FALSE)
      switch(.Generic,
        "+"   = e1 + e2,
        "-"   = e1 - e2,
        "*"   = e1 * e2,
        "/"   = e1 / e2,
        "^"   = e1 ^ e2,
        "%%"  = e1 %% e2,
        "%/%"  = e1 %/% e2,
        "!="  = e1 != e2,
        ">"   = e1 > e2,
        "<"   = e1 < e2,
        ">="  = e1 >= e2,
        "<="  = e1 <= e2,
        "=="  = e1 == e2)
    }
  }
  if(.Generic %in% c("+","-","*","/","^","%%","%/%")) {
    .Call('add_xts_class', e)
  }
  else e
}
