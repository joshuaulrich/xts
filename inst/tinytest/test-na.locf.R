xdata <- .xts(c(1, NA, 3, 4, 5, 6), c(0, 4, 10, 19, 24, 29))
xdata2 <- merge(one = xdata, two = xdata)
xindex <- .xts(rep(0, 5), c(5, 10, 20, 25, 28))
types <- c("double", "integer", "character", "logical")

### na.locf.xts() on a univariate xts object
info_msg <- "test.nalocf"
for (type in types) {
  xdat <- xdata
  storage.mode(xdat) <- type
  zdat <- as.zoo(xdat)

  x <- na.locf(xdat)
  z <- na.locf(zdat)
  #expect_identical(x, as.xts(z))  # FALSE (attribute order differs)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_leading_NA"
for (type in types) {
  xdat <- xdata
  storage.mode(xdat) <- type
  zdat <- as.zoo(xdat)

  xdat[1] <- NA
  zdat[1] <- NA

  x <- na.locf(xdat, na.rm = TRUE)
  z <- na.locf(zdat, na.rm = TRUE)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))

  x <- na.locf(xdat, na.rm = FALSE)
  z <- na.locf(zdat, na.rm = FALSE)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_fromLast"
for (type in types) {
  xdat <- xdata
  storage.mode(xdat) <- type
  zdat <- as.zoo(xdat)

  x <- na.locf(xdat, fromLast = TRUE)
  z <- na.locf(zdat, fromLast = TRUE)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_x"
for (type in types) {
  xdat <- xdata
  xidx <- xindex
  storage.mode(xdat) <- storage.mode(xidx) <- type
  zdat <- as.zoo(xdat)
  zidx <- as.zoo(xidx)

  xidx <- rbind(xidx, .xts(vector(type, 1), 30))
  zidx <- as.zoo(xidx)

  x <- na.locf(xdat, x = index(xidx))
  z <- na.locf(zdat, x = index(zidx))
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_xout"
for (type in types) {
  xdat <- xdata
  xidx <- xindex
  storage.mode(xdat) <- storage.mode(xidx) <- type
  zdat <- as.zoo(xdat)
  zidx <- as.zoo(xidx)

  x <- na.locf(xdat, xout = index(xidx))
  z <- na.locf(zdat, xout = index(zidx))
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}


### na.locf.xts() on a multivariate xts object
info_msg <- "test.nalocf_by_column"
for (type in types) {
  xdat <- xdata2
  storage.mode(xdat) <- type
  zdat <- as.zoo(xdat)

  x <- na.locf(xdat)
  z <- na.locf(zdat)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_by_column_leading_NA"
for (type in types) {
  xdat <- xdata2
  storage.mode(xdat) <- type
  zdat <- as.zoo(xdat)

  xdat[1] <- NA
  zdat[1] <- NA

  if (FALSE) {
    ### bug w/zoo causes this to fail
    ### zoo:::na.locf.default() does not remove the first row
    x <- na.locf(xdat, na.rm = TRUE)
    z <- na.locf(zdat, na.rm = TRUE)
    expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
  }

  x <- na.locf(xdat, na.rm = FALSE)
  z <- na.locf(zdat, na.rm = FALSE)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_by_column_fromLast"
for (type in types) {
  xdat <- xdata2
  storage.mode(xdat) <- type
  zdat <- as.zoo(xdat)

  x <- na.locf(xdat, fromLast = TRUE)
  z <- na.locf(zdat, fromLast = TRUE)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_by_column_x"
for (type in types) {
  xdat <- xdata2
  xidx <- xindex
  storage.mode(xdat) <- storage.mode(xidx) <- type
  zdat <- as.zoo(xdat)
  zidx <- as.zoo(xidx)

  xidx <- rbind(xidx, .xts(vector(type, 1), 30))
  zidx <- as.zoo(xidx)

  x <- na.locf(xdat, x = index(xidx))
  z <- na.locf(zdat, x = index(zidx))
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_by_column_xout"
for (type in types) {
  xdat <- xdata2
  xidx <- xindex
  storage.mode(xdat) <- storage.mode(xidx) <- type
  zdat <- as.zoo(xdat)
  zidx <- as.zoo(xidx)

  x <- na.locf(xdat, xout = index(xidx))
  z <- na.locf(zdat, xout = index(zidx))
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_by_column_1NA"
narow <- 1L
for (type in types) {
  xdrow <- xdata2[narow,]
  xdat <- xdata2 * NA
  xdat[narow,] <- xdrow
  storage.mode(xdat) <- type
  zdat <- as.zoo(xdat)

  x <- na.locf(xdat)
  z <- na.locf(zdat)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_by_column_1NA_fromLast"
narow <- nrow(xdata2)
for (type in types) {
  xdrow <- xdata2[narow,]
  xdat <- xdata2 * NA
  xdat[narow,] <- xdrow
  storage.mode(xdat) <- type
  zdat <- as.zoo(xdat)

  x <- na.locf(xdat, fromLast = TRUE)
  z <- na.locf(zdat, fromLast = TRUE)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_first_column_all_NA"
nacol <- 1L
for (type in types) {
  xdat <- xdata2
  xdat[,nacol] <- xdat[,nacol] * NA
  storage.mode(xdat) <- type
  zdat <- as.zoo(xdat)

  x <- na.locf(xdat)
  z <- na.locf(zdat)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}

info_msg <- "test.nalocf_last_column_all_NA"
nacol <- NCOL(xdata2)
for (type in types) {
  xdat <- xdata2
  xdat[,nacol] <- xdat[,nacol] * NA
  storage.mode(xdat) <- type
  zdat <- as.zoo(xdat)

  x <- na.locf(xdat)
  z <- na.locf(zdat)
  expect_equal(x, as.xts(z), info = paste(info_msg, "-", type))
}
