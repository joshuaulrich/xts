xdata <- .xts(c(1, NA, 3, 4, 5, 6), c(0, 4, 10, 19, 24, 29))
xindex <- .xts(rep(0, 5), c(5, 10, 20, 25, 28))
types <- c("double", "integer", "character", "logical")

info_msg <- "test.naomit"
for (type in types) {
  xdat <- xdata
  xidx <- xindex
  storage.mode(xdat) <- storage.mode(xidx) <- type
  zdat <- as.zoo(xdat)
  zidx <- as.zoo(xidx)

  x <- na.omit(xdat)
  z <- na.omit(zdat)
  # na.omit.xts adds "index" attribute to the "na.action" attribute
  attr(attr(x, "na.action"), "index") <- NULL
  #expect_identical(x, as.xts(z))  # FALSE (attribute order differs)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.naomit_by_column"
for (type in types) {
  xdat <- xdata
  xidx <- xindex
  storage.mode(xdat) <- storage.mode(xidx) <- type
  zdat <- as.zoo(xdat)
  zidx <- as.zoo(xidx)

  x <- na.omit(merge(one = xdat, two = xdat))
  z <- na.omit(merge(one = zdat, two = zdat))
  # na.omit.xts adds "index" attribute to the "na.action" attribute
  attr(attr(x, "na.action"), "index") <- NULL
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}
