test.basic_2way_outer <- function() {
  merge.data <-
  function(data.type, index.type)
  {
    x <- .xts(rep(1, 5), 1:5)
    y <- .xts(rep(2, 5), 4:8)
    o <- .xts(matrix(c(rep(1, 5), rep(NA, 6), rep(2, 5)), 8, 2), 1:8)
    colnames(o) <- c("x", "y")

    storage.mode(x) <- data.type
    storage.mode(y) <- data.type
    storage.mode(o) <- data.type
    storage.mode(.index(x)) <- index.type
    storage.mode(.index(y)) <- index.type
    storage.mode(.index(o)) <- index.type

    list(x = x, y = y, out = o,
         msg = paste(data.type, "data,", index.type, "index"))
  }

  # numeric coredata
  md <- merge.data("numeric", "numeric")
  checkIdentical(md$out, merge(x = md$x, y = md$y), md$msg)

  md <- merge.data("numeric", "integer")
  checkIdentical(md$out, merge(x = md$x, y = md$y), md$msg)

  # integer coredata
  md <- merge.data("integer", "numeric")
  checkIdentical(md$out, merge(x = md$x, y = md$y), md$msg)

  md <- merge.data("integer", "integer")
  checkIdentical(md$out, merge(x = md$x, y = md$y), md$msg)

  # logical coredata
  md <- merge.data("logical", "numeric")
  checkIdentical(md$out, merge(x = md$x, y = md$y), md$msg)

  md <- merge.data("logical", "integer")
  checkIdentical(md$out, merge(x = md$x, y = md$y), md$msg)

  # character coredata
  md <- merge.data("character", "numeric")
  checkIdentical(md$out, merge(x = md$x, y = md$y), md$msg)

  md <- merge.data("character", "integer")
  checkIdentical(md$out, merge(x = md$x, y = md$y), md$msg)

  # complex coredata
  md <- merge.data("complex", "numeric")
  checkEqualsNumeric(md$out, merge(x = md$x, y = md$y), md$msg)

  md <- merge.data("complex", "integer")
  checkEqualsNumeric(md$out, merge(x = md$x, y = md$y), md$msg)
}

test.basic_2way_inner <- function() {
  merge.data <-
  function(data.type, index.type)
  {
    x <- .xts(rep(1, 5), 1:5)
    y <- .xts(rep(2, 5), 4:8)
    o <- .xts(matrix(c(rep(1, 2), rep(2, 2)), 2, 2), 4:5)
    colnames(o) <- c("x", "y")

    storage.mode(x) <- data.type
    storage.mode(y) <- data.type
    storage.mode(o) <- data.type
    storage.mode(.index(x)) <- index.type
    storage.mode(.index(y)) <- index.type
    storage.mode(.index(o)) <- index.type

    list(x = x, y = y, out = o,
         msg = paste(data.type, "data,", index.type, "index"))
  }

  # numeric coredata
  md <- merge.data("numeric", "numeric")
  x <- merge(x = md$x, y = md$y, join = "inner")
  checkIdentical(md$out, x, md$msg)

  md <- merge.data("numeric", "integer")
  x <- merge(x = md$x, y = md$y, join = "inner")
  checkIdentical(md$out, x, md$msg)

  # integer coredata
  md <- merge.data("integer", "numeric")
  x <- merge(x = md$x, y = md$y, join = "inner")
  checkIdentical(md$out, x, md$msg)

  md <- merge.data("integer", "integer")
  x <- merge(x = md$x, y = md$y, join = "inner")
  checkIdentical(md$out, x, md$msg)

  # logical coredata
  md <- merge.data("logical", "numeric")
  x <- merge(x = md$x, y = md$y, join = "inner")
  checkIdentical(md$out, x, md$msg)

  md <- merge.data("logical", "integer")
  x <- merge(x = md$x, y = md$y, join = "inner")
  checkIdentical(md$out, x, md$msg)

  # character coredata
  md <- merge.data("character", "numeric")
  x <- merge(x = md$x, y = md$y, join = "inner")
  checkIdentical(md$out, x, md$msg)

  md <- merge.data("character", "integer")
  x <- merge(x = md$x, y = md$y, join = "inner")
  checkIdentical(md$out, x, md$msg)

  # complex coredata
  md <- merge.data("complex", "numeric")
  x <- merge(x = md$x, y = md$y, join = "inner")
  checkEqualsNumeric(md$out, x, md$msg)

  md <- merge.data("complex", "integer")
  x <- merge(x = md$x, y = md$y, join = "inner")
  checkEqualsNumeric(md$out, x, md$msg)
}

test.merge_empty_xts_with_2_scalars <- function() {
  m1 <- merge(xts(), 1, 1)
  m2 <- merge(merge(xts(), 1), 1)
  checkIdentical(m1, m2)
}

# Tests for NA in index. Construct xts object using structure() because
# xts constructors should not allow users to create objects with NA in
# the index
indexHasNA_dbl <-
structure(1:5, .Dim = c(5L, 1L),
          index = structure(c(1, 2, 3, 4, NA), tzone = "",
                            tclass = c("POSIXct", "POSIXt")),
          .indexCLASS = c("POSIXct", "POSIXt"),
          .indexTZ = "", tclass = c("POSIXct", "POSIXt"),
          tzone = "", class = c("xts", "zoo"))

indexHasNA_int <-
structure(1:5, .Dim = c(5L, 1L),
          index = structure(c(1L, 2L, 3L, 4L, NA), tzone = "",
                            tclass = c("POSIXct", "POSIXt")),
          .indexCLASS = c("POSIXct", "POSIXt"),
          .indexTZ = "", tclass = c("POSIXct", "POSIXt"),
          tzone = "", class = c("xts", "zoo"))

test.merge_index_contains_NA_integer <- function() {
  checkException(merge(indexHasNA_int, indexHasNA_int), silent = TRUE)
}

test.merge_index_contains_NA_double <- function() {
  checkException(merge(indexHasNA_dbl, indexHasNA_dbl), silent = TRUE)
}

test.merge_index_contains_NaN <- function() {
  x <- indexHasNA_dbl
  idx <- attr(x, "index")
  idx[length(idx)] <- NaN
  attr(x, "index") <- idx
  checkException(merge(x, x), silent = TRUE)
}

test.merge_index_contains_Inf <- function() {
  x <- indexHasNA_dbl
  idx <- attr(x, "index")
  idx[length(idx)] <- Inf
  attr(x, "index") <- idx
  checkException(merge(x, x), silent = TRUE)

  idx <- rev(idx)
  idx[1L] <- -Inf
  attr(x, "index") <- idx
  checkException(merge(x, x), silent = TRUE)
}
# /end Tests for NA in index

# zero-length fill argument
test.merge_fill_NULL <- function() {
  x1 <- .xts(1, 1)
  x2 <- .xts(2, 2)
  x <- merge(x1, x2, fill = NULL)

  out <- .xts(matrix(c(1, NA, NA, 2), 2), c(1,2))
  colnames(out) <- c("x1", "x2")
  checkIdentical(x, out)
}

test.merge_fill_zero_length <- function() {
  x1 <- .xts(1, 1)
  x2 <- .xts(2, 2)
  x <- merge(x1, x2, fill = numeric())

  out <- .xts(matrix(c(1, NA, NA, 2), 2), c(1,2))
  colnames(out) <- c("x1", "x2")
  checkIdentical(x, out)
}
