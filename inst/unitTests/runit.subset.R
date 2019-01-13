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
  empty <- .xts(logical(), d0, dim = 0:1, dimnames = list(NULL, NULL))

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
  empty <- .xts(logical(), d0, dim = 0:1, dimnames = list(NULL, NULL))

  checkIdentical(zl[0,], empty)
  checkIdentical(zl[0],  empty)
}

test.empty_i_negative <- function() {
  d0 <- as.Date(integer())
  zl <- xts(, d0)
  empty <- .xts(logical(), d0, dim = 0:1, dimnames = list(NULL, NULL))

  checkIdentical(zl[-1,], empty)
  checkIdentical(zl[-1],  empty)
}

test.empty_i_NA <- function() {
  d0 <- as.Date(integer())
  zl <- xts(, d0)
  empty <- .xts(logical(), d0, dim = 0:1, dimnames = list(NULL, NULL))

  checkIdentical(zl[NA,], empty)
  checkIdentical(zl[NA],  empty)
}

test.empty_i_NULL <- function() {
  d0 <- as.Date(integer())
  zl <- xts(, d0)
  empty <- .xts(logical(), d0, dim = 0:1, dimnames = list(NULL, NULL))

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

# Time-of-day subset

test.time_of_day_when_DST_starts <- function() {
  # 2017-03-12: no 0200
  tz <- "America/Chicago"
  tmseq <- seq(as.POSIXct("2017-03-11", tz),
               as.POSIXct("2017-03-14", tz), by = "1 hour")
  x <- xts(seq_along(tmseq), tmseq)
  i <- structure(c(1489215600, 1489219200, 1489222800, 1489302000,
                   1489305600, 1489384800, 1489388400, 1489392000),
       tzone = "America/Chicago", tclass = c("POSIXct", "POSIXt"))
  checkIdentical(.index(x["T01:00:00/T03:00:00"]), i)
}

test.time_of_day_when_DST_ends <- function() {
  # 2017-11-05: 0200 occurs twice
  tz <- "America/Chicago"
  tmseq <- seq(as.POSIXct("2017-11-04", tz),
               as.POSIXct("2017-11-07", tz), by = "1 hour")
  x <- xts(seq_along(tmseq), tmseq)
  i <- structure(c(1509775200, 1509778800, 1509782400, 1509861600, 1509865200,
       1509868800, 1509872400, 1509951600, 1509955200, 1509958800),
       tzone = "America/Chicago", tclass = c("POSIXct", "POSIXt"))
  checkIdentical(.index(x["T01:00:00/T03:00:00"]), i)
}

test.time_of_day_start_equals_end <- function() {
  i <- 0:47
  x <- .xts(i, i * 3600, tz = "UTC")
  i1 <- .index(x[c(2L, 26L)])

  checkIdentical(.index(x["T01:00/T01:00"]), i1)
}

test.time_of_day_end_before_start <- function() {
  # Yes, this actually makes sense and is useful for financial markets
  # E.g. some futures markets open at 18:00 and close at 16:00 the next day
  i <- 0:47
  x <- .xts(i, i * 3600, tz = "UTC")
  i1 <- .index(x[-c(18L, 42L)])

  checkIdentical(.index(x["T18:00/T16:00"]), i1)
}

# TODO: Add tests for possible edge cases and/or errors
# end time before start time
# start time and/or end time missing "T" prefix
# start time and/or end time missing ":" separator

test.time_of_day_on_zero_width <- function() {
  # return relevant times and a column of NA; consistent with zoo
  i <- 0:47
  tz <- "America/Chicago"
  x <- .xts(, i * 3600, tzone = tz)
  y <- x["T18:00/T20:00"]
  checkIdentical(y, .xts(rep(NA, 6), c(0:2, 24:26)*3600, tzone = tz))
}
