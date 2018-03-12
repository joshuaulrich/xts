XDAT <- .xts(c(1, NA, 3, 4, 5, 6), c(0, 4, 10, 19, 24, 29))
XIDX <- .xts(rep(0, 5), c(5, 10, 20, 25, 28))
MODES <- c("double", "integer", "character", "logical")

test.nalocf <- function() {
  for (m in MODES) {
    xdat <- XDAT
    xidx <- XIDX
    storage.mode(xdat) <- storage.mode(xidx) <- m
    zdat <- as.zoo(xdat)
    zidx <- as.zoo(xidx)

    x <- na.locf(xdat)
    z <- na.locf(zdat)
    #checkIdentical(x, as.xts(z))  # FALSE (attribute order differs)
    checkEquals(x, as.xts(z), check.attributes = TRUE)
  }
}

test.nalocf_by_column <- function() {
  for (m in MODES) {
    xdat <- XDAT
    xidx <- XIDX
    storage.mode(xdat) <- storage.mode(xidx) <- m
    zdat <- as.zoo(xdat)
    zidx <- as.zoo(xidx)

    x <- na.locf(merge(one = xdat, two = xdat))
    z <- na.locf(merge(one = zdat, two = zdat))
    checkEquals(x, as.xts(z), check.attributes = TRUE)
  }
}

test.nalocf_leading_NA <- function() {
  for (m in MODES) {
    xdat <- XDAT
    xidx <- XIDX
    storage.mode(xdat) <- storage.mode(xidx) <- m
    zdat <- as.zoo(xdat)
    zidx <- as.zoo(xidx)

    xdat[1] <- NA
    zdat[1] <- NA

    x <- na.locf(xdat, na.rm = TRUE)
    z <- na.locf(zdat, na.rm = TRUE)
    checkEquals(x, as.xts(z), check.attributes = TRUE)

    x <- na.locf(xdat, na.rm = FALSE)
    z <- na.locf(zdat, na.rm = FALSE)
    checkEquals(x, as.xts(z), check.attributes = TRUE)
  }
}

test.nalocf_fromLast <- function() {
  for (m in MODES) {
    xdat <- XDAT
    xidx <- XIDX
    storage.mode(xdat) <- storage.mode(xidx) <- m
    zdat <- as.zoo(xdat)
    zidx <- as.zoo(xidx)

    x <- na.locf(xdat, fromLast = TRUE)
    z <- na.locf(zdat, fromLast = TRUE)
    checkEquals(x, as.xts(z), check.attributes = TRUE)
  }
}

test.nalocf_x <- function() {
  for (m in MODES) {
    xdat <- XDAT
    xidx <- XIDX
    storage.mode(xdat) <- storage.mode(xidx) <- m
    zdat <- as.zoo(xdat)
    zidx <- as.zoo(xidx)

    xidx <- rbind(xidx, .xts(0, 30))
    zidx <- as.zoo(xidx)

    x <- na.locf(xdat, x = index(xidx))
    z <- na.locf(zdat, x = index(zidx))
    checkEquals(x, as.xts(z), check.attributes = TRUE)
  }
}

test.nalocf_xout <- function() {
  for (m in MODES) {
    xdat <- XDAT
    xidx <- XIDX
    storage.mode(xdat) <- storage.mode(xidx) <- m
    zdat <- as.zoo(xdat)
    zidx <- as.zoo(xidx)

    x <- na.locf(xdat, xout = index(xidx))
    z <- na.locf(zdat, xout = index(zidx))
    checkEquals(x, as.xts(z), check.attributes = TRUE)
  }
}
