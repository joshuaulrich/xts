# i = missing, j = NA, object has column names
# See #181
test.i_missing_j_NA_has_colnames <- function() {
  iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
  idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
  dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
  ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)
  colnames(iina) <- colnames(idna) <-
  colnames(dina) <- colnames(ddna) <- rep(NA_character_, 2)

  # int data, int index
  ii <- .xts(matrix(1:10, 5, 2), 1:5)
  colnames(ii) <- c("a", "b")
  checkIdentical(ii[, NA], iina)
  checkIdentical(ii[, 1][, NA], iina[, 1])

  # int data, dbl index
  id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
  colnames(id) <- c("a", "b")
  checkIdentical(id[, NA], idna)
  checkIdentical(id[, 1][, NA], idna[, 1])

  # dbl data, int index
  di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
  colnames(di) <- c("a", "b")
  checkIdentical(di[, NA], dina)
  checkIdentical(di[, 1][, NA], dina[, 1])

  # dbl data, dbl index
  dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
  colnames(dd) <- c("a", "b")
  checkIdentical(dd[, NA], ddna)
  checkIdentical(dd[, 1][, NA], ddna[, 1])
}

# i = missing, j = NA, object does not have column names
# See #97
test.i_missing_j_NA_no_colnames <- function() {
  iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
  idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
  dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
  ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)

  # int data, int index
  ii <- .xts(matrix(1:10, 5, 2), 1:5)
  checkIdentical(ii[, NA], iina)
  checkIdentical(ii[, 1][, NA], iina[, 1])

  # int data, dbl index
  id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
  checkIdentical(id[, NA], idna)
  checkIdentical(id[, 1][, NA], idna[, 1])

  # dbl data, int index
  di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
  checkIdentical(di[, NA], dina)
  checkIdentical(di[, 1][, NA], dina[, 1])

  # dbl data, dbl index
  dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
  checkIdentical(dd[, NA], ddna)
  checkIdentical(dd[, 1][, NA], ddna[, 1])
}

# i = integer, j = NA, object has column names
# See #97
test.i_integer_j_NA_has_colnames <- function() {
  iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
  idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
  dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
  ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)
  colnames(iina) <- colnames(idna) <-
  colnames(dina) <- colnames(ddna) <- rep(NA_character_, 2)

  i <- 1:3

  # int data, int index
  ii <- .xts(matrix(1:10, 5, 2), 1:5)
  colnames(ii) <- c("a", "b")
  checkIdentical(ii[i, NA], iina[i,])
  checkIdentical(ii[i, 1][, NA], iina[i, 1])

  # int data, dbl index
  id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
  colnames(id) <- c("a", "b")
  checkIdentical(id[i, NA], idna[i,])
  checkIdentical(id[i, 1][, NA], idna[i, 1])

  # dbl data, int index
  di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
  colnames(di) <- c("a", "b")
  checkIdentical(di[i, NA], dina[i,])
  checkIdentical(di[i, 1][, NA], dina[i, 1])

  # dbl data, dbl index
  dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
  colnames(dd) <- c("a", "b")
  checkIdentical(dd[i, NA], ddna[i,])
  checkIdentical(dd[i, 1][, NA], ddna[i, 1])
}

# i = integer, j = NA, object does not have column names
# See #97
test.i_integer_j_NA_no_colnames <- function() {
  iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
  idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
  dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
  ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)

  i <- 1:3

  # int data, int index
  ii <- .xts(matrix(1:10, 5, 2), 1:5)
  checkIdentical(ii[i, NA], iina[i,])
  checkIdentical(ii[i, 1][, NA], iina[i, 1])

  # int data, dbl index
  id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
  checkIdentical(id[i, NA], idna[i,])
  checkIdentical(id[i, 1][, NA], idna[i, 1])

  # dbl data, int index
  di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
  checkIdentical(di[i, NA], dina[i,])
  checkIdentical(di[i, 1][, NA], dina[i, 1])

  # dbl data, dbl index
  dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
  checkIdentical(dd[i, NA], ddna[i,])
  checkIdentical(dd[i, 1][, NA], ddna[i, 1])
}

test.i_0 <- function() {
  x <- .xts(matrix(1:10, 5, 2), 1:5)
  z <- as.zoo(x)
  xz0 <- as.xts(z[0,])

  checkEquals(x[0,], xz0, check.attributes = TRUE)
}

# Subset by non-numeric classes
X <- xts(1:5, as.Date("2018-04-21") - 5:1)

test.i_character <- function() {
  x <- X

  for (r in c(1L, 3L, 5L)) {
    y <- x[r,]
    i <- as.character(index(y))
    checkIdentical(y, x[i, ])
  }
}

test.i_asis_character <- function() {
  x <- X

  for (r in c(1L, 3L, 5L)) {
    y <- x[r,]
    i <- as.character(index(y))
    checkIdentical(y, x[I(i), ])
  }
}

test.i_Date <- function() {
  x <- X

  for (r in c(1L, 3L, 5L)) {
    y <- x[r,]
    i <- index(y)
    checkIdentical(y, x[i, ])
  }
}

test.i_POSIXct <- function() {
  x <- X
  index(x) <- as.POSIXct(index(x), tz = "UTC")

  for (r in c(1L, 3L, 5L)) {
    y <- x[r,]
    i <- index(y)
    checkIdentical(y, x[i, ])
  }
}

test.i_POSIXlt <- function() {
  x <- X
  index(x) <- as.POSIXlt(index(x), tz = "UTC")

  for (r in c(1L, 3L, 5L)) {
    y <- x[r,]
    i <- index(y)
    checkIdentical(y, x[i, ])
  }
}

# invalid date/time
test.i_invalid_date_string <- function() {
  x <- xts(1:10, as.Date("2015-02-20")+0:9)
  y <- x["2012-02-30"]
  checkIdentical(y, x[NA,])
}
test.i_only_range_separator_or_empty_string <- function() {
  x <- xts(1:10, as.Date("2015-02-20")+0:9)
  y <- x["/",]
  checkIdentical(y, x)
  y <- x["::",]
  checkIdentical(y, x)
  y <- x["",]
  checkIdentical(y, x)
}
test.i_date_range_open_end <- function() {
  x <- xts(1:10, as.Date("2015-02-20")+0:9)
  y <- x["2015-02-23/",]
  checkIdentical(y, x[4:10,])
}
test.i_date_range_open_start <- function() {
  x <- xts(1:10, as.Date("2015-02-20")+0:9)
  y <- x["/2015-02-26",]
  checkIdentical(y, x[1:7,])
}

# subset empty xts
test.empty_i_datetime <- function() {
  d0 <- as.Date(integer())
  zl <- xts(, d0)
  empty <- .xts(logical(), d0, "Date", "UTC", dim = 0:1, dimnames = list(NULL, NULL))

  i <- Sys.Date()
  checkIdentical(zl[i,], empty)
  checkIdentical(zl[i],  empty)

  i <- Sys.time()
  checkIdentical(zl[i,], empty)
  checkIdentical(zl[i],  empty)
}

test.empty_i_zero <- function() {
  d0 <- as.Date(integer())
  zl <- xts(, d0)
  empty <- .xts(logical(), d0, "Date", "UTC", dim = 0:1, dimnames = list(NULL, NULL))

  checkIdentical(zl[0,], empty)
  checkIdentical(zl[0],  empty)
}

test.empty_i_negative <- function() {
  d0 <- as.Date(integer())
  zl <- xts(, d0)
  empty <- .xts(logical(), d0, "Date", "UTC", dim = 0:1, dimnames = list(NULL, NULL))

  checkIdentical(zl[-1,], empty)
  checkIdentical(zl[-1],  empty)
}

test.empty_i_NA <- function() {
  d0 <- as.Date(integer())
  zl <- xts(, d0)
  empty <- .xts(logical(), d0, "Date", "UTC", dim = 0:1, dimnames = list(NULL, NULL))

  checkIdentical(zl[NA,], empty)
  checkIdentical(zl[NA],  empty)
}

test.empty_i_NULL <- function() {
  d0 <- as.Date(integer())
  zl <- xts(, d0)
  empty <- .xts(logical(), d0, "Date", "UTC", dim = 0:1, dimnames = list(NULL, NULL))

  checkIdentical(zl[NULL,], empty)
  checkIdentical(zl[NULL],  empty)
}

test.duplicate_index_duplicate_i <- function() {
  dates <-
    structure(c(15770, 16257, 16282, 16291, 16296, 16296, 16298, 16301,
                16432, 16452), class = "Date")
  x <- xts(c(1, 2, 2, 3, 3, 3, 3, 3, 4, 4), dates)

  dupdates <-
    structure(c(15770, 16257, 16282, 16291, 16296, 16296, 16296, 16296,
                16298, 16301, 16432, 16452), class = "Date")
  y <- xts(c(1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4), dupdates)

  checkIdentical(x[index(x),],  y)
}

test.window_yearmon_yearqtr_tclass_dispatches_to_zoo <- function() {
  i1 <- seq(as.yearmon(2007), by = 1/12, length.out = 36)
  x1 <- xts(1:36, i1)
  i2 <- seq(as.yearqtr(2007), by = 1/4, length.out = 36)
  x2 <- xts(1:36, i2)

  r1 <- x1["2015"]
  r2 <- x2["2015"]

  # zoo supports numeric start for yearmon and yearqtr
  w1 <- window(x1, start = 2015.01)  # to window.zoo()
  w2 <- window(x2, start = 2015.1)   # to window.zoo()
  checkEquals(r1, w1, "window, yearmon, numeric start")
  checkEquals(r2, w2, "window, yearqtr, numeric start")

  w1 <- window(x1, start = "2015-01-01")  # to window.xts()
  w2 <- window(x2, start = "2015Q1")      # to window.zoo()
  checkEquals(r1, w1, "window, yearmon, character start")
  checkEquals(r2, w2, "window, yearqtr, character start")
}

test.zero_width_subset_does_not_drop_class <- function(x) {
  target <- c("custom", "xts", "zoo")
  x <- .xts(1:10, 1:10, class = target)
  y <- x[,0]
  checkEquals(target, class(y))
}

test.zero_width_subset_does_not_drop_user_attributes <- function(x) {
  x <- .xts(1:10, 1:10, my_attr = "hello")
  y <- x[,0]
  checkEquals("hello", attr(y, "my_attr"))
}

test.zero_length_subset_xts_returns_same_tclass <- function() {
  x <- .xts(matrix(1)[0,], integer(0), "Date")
  checkTrue(tclass(x[0,]) == "Date")
  x <- .xts(matrix(1)[0,], integer(0), "POSIXct", "America/Chicago")
  checkTrue(tclass(x[0,]) == "POSIXct")
  checkTrue(tzone(x[0,]) == "America/Chicago")
}

test.zero_length_subset_returns_same_storage_mode <- function() {
  # integer
  x <- .xts(matrix(integer(0), 0), integer(0))
  checkEquals(storage.mode(x), storage.mode(x[0, ]))
  checkEquals(storage.mode(x), storage.mode(x[0, 0]))
  checkEquals(storage.mode(x), storage.mode(x[0, FALSE]))

  x <- .xts(matrix(integer(0), 0, 2), integer(0))
  checkEquals(storage.mode(x), storage.mode(x[0,]))
  checkEquals(storage.mode(x), storage.mode(x[0, 1]))
  checkEquals(storage.mode(x), storage.mode(x[0, c(TRUE, FALSE)]))

  # numeric
  x <- .xts(matrix(numeric(0), 0), integer(0))
  checkEquals(storage.mode(x), storage.mode(x[0, ]))
  checkEquals(storage.mode(x), storage.mode(x[0, 0]))
  checkEquals(storage.mode(x), storage.mode(x[0, FALSE]))

  x <- .xts(matrix(numeric(0), 0, 2), integer(0))
  checkEquals(storage.mode(x), storage.mode(x[0,]))
  checkEquals(storage.mode(x), storage.mode(x[0, 1]))
  checkEquals(storage.mode(x), storage.mode(x[0, c(TRUE, FALSE)]))
}
