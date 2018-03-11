xdat <- .xts(c(1, NA, 3, 4, 5, 6), c(0, 4, 10, 19, 24, 29))
xidx <- .xts(rep(0, 5), c(5, 10, 20, 25, 28))
zdat <- as.zoo(xdat)
zidx <- as.zoo(xidx)

test.nalocf <- function() {
  x <- na.locf(xdat)
  z <- na.locf(zdat)
  #checkIdentical(x, as.xts(z))  # FALSE (attribute order differs)
  checkEquals(x, as.xts(z), check.attributes = TRUE)
}

test.nalocf_by_column <- function() {
  x <- na.locf(merge(one = xdat, two = xdat))
  z <- na.locf(merge(one = zdat, two = zdat))
  checkEquals(x, as.xts(z), check.attributes = TRUE)
}

test.nalocf_leading_NA <- function() {
  xdat[1] <- NA
  zdat[1] <- NA

  x <- na.locf(xdat, na.rm = TRUE)
  z <- na.locf(zdat, na.rm = TRUE)
  checkEquals(x, as.xts(z), check.attributes = TRUE)

  x <- na.locf(xdat, na.rm = FALSE)
  z <- na.locf(zdat, na.rm = FALSE)
  checkEquals(x, as.xts(z), check.attributes = TRUE)
}

test.nalocf_fromLast <- function() {
  x <- na.locf(xdat, fromLast = TRUE)
  z <- na.locf(zdat, fromLast = TRUE)
  checkEquals(x, as.xts(z), check.attributes = TRUE)
}
