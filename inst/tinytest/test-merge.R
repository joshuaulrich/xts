library(xts)

zero_width_xts <- xts()

info_msg <- "test.merge_empty_xts_with_2_scalars"
m1 <- merge(zero_width_xts, 1, 1)
m2 <- merge(merge(zero_width_xts, 1), 1)
expect_identical(m1, m2)

info_msg <- "test.merge_more_than_2_zero_width_objects"
m1 <- merge(zero_width_xts, zero_width_xts, zero_width_xts)
expect_identical(m1, zero_width_xts)

### Tests for NA in index. Construct xts object using structure() because
### xts constructors should not allow users to create objects with NA in
### the index
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

info_msg <- "test.merge_index_contains_NA_integer"
expect_error(merge(indexHasNA_int, indexHasNA_int), info = info_msg)

info_msg <- "test.merge_index_contains_NA_double"
expect_error(merge(indexHasNA_dbl, indexHasNA_dbl), info = info_msg)

info_msg <- "test.merge_index_contains_NaN"
x <- indexHasNA_dbl
idx <- attr(x, "index")
idx[length(idx)] <- NaN
attr(x, "index") <- idx
expect_error(merge(x, x), info = info_msg)

info_msg <- "test.merge_index_contains_Inf"
x <- indexHasNA_dbl
idx <- attr(x, "index")
idx[length(idx)] <- Inf
attr(x, "index") <- idx
expect_error(merge(x, x), info = info_msg)

idx <- rev(idx)
idx[1L] <- -Inf
attr(x, "index") <- idx
expect_error(merge(x, x), info = info_msg)
### /end Tests for NA in index


### zero-length fill argument
info_msg <- "test.merge_fill_NULL"
x1 <- .xts(1, 1)
x2 <- .xts(2, 2)
x <- merge(x1, x2, fill = NULL)
out <- .xts(matrix(c(1, NA, NA, 2), 2), c(1,2))
colnames(out) <- c("x1", "x2")
expect_identical(x, out, info = info_msg)

info_msg <- "test.merge_fill_zero_length"
x1 <- .xts(1, 1)
x2 <- .xts(2, 2)
x <- merge(x1, x2, fill = numeric())
out <- .xts(matrix(c(1, NA, NA, 2), 2), c(1,2))
colnames(out) <- c("x1", "x2")
expect_identical(x, out, info = info_msg)

info_msg <- "test.merge_with_zero_width_returns_original_type"
M1 <- .xts(1:3, 1:3, dimnames = list(NULL, "m1"))
types <- c("double", "integer", "logical", "character")
for (type in types) {
  m1 <- M1
  storage.mode(m1) <- type
  e1 <- .xts(,1:3)
  m2 <- merge(m1, e1)
  expect_identical(m1, m2, info = paste(info_msg, "- type =", type))
}

info_msg <- "test.n_way_merge_on_all_types"
D1 <- as.Date("2018-01-03")-2:0
M1 <- xts(1:3, D1, dimnames = list(NULL, "m"))
M3 <- xts(cbind(1:3, 1:3, 1:3), D1,
          dimnames = list(NULL, c("m", "m.1", "m.2")))
types <- c("double", "integer", "logical", "character", "complex")
for (type in types) {
  m1 <- M1
  m3 <- M3
  storage.mode(m1) <- storage.mode(m3) <- type
  m <- merge(m1, m1, m1)
  expect_identical(m, m3, info = paste(info_msg, "- type =", type))
}

info_msg <- "test.shorter_colnames_for_unnamed_args"
X <- .xts(rnorm(10, 10), 1:10)
types <- c("double", "integer", "logical", "character", "complex")
for (type in types) {
  x <- X
  storage.mode(x) <- type
  mx <- do.call(merge, list(x, x))
  expect_true(all(nchar(colnames(mx)) < 200),
              info = paste(info_msg, "- type = ", type))
}


info_msg <- "test.check_names_false"
x <- .xts(1:3, 1:3, dimnames = list(NULL, "42"))
y <- .xts(1:3, 1:3, dimnames = list(NULL, "21"))
z <- merge(x, y)                       # leading "X" added
expect_identical(colnames(z), c("X42", "X21"), info = info_msg)
z <- merge(x, y, check.names = TRUE)   # same
expect_identical(colnames(z), c("X42", "X21"), info = info_msg)
z <- merge(x, y, check.names = FALSE)  # should have numeric column names
expect_identical(colnames(z), c("42", "21"), info = info_msg)


info_msg <- "test.merge_fills_complex_types"
data. <- cbind(c(1:5*1i, NA, NA), c(NA, NA, 3:7*1i))
colnames(data.) <- c("x", "y")
d21 <- data.
d21[is.na(d21)] <- 21i
x <- xts(1:5 * 1i, as.Date(1:5, origin = "1970-01-01"))
y <- xts(3:7 * 1i, as.Date(3:7, origin = "1970-01-01"))
z <- merge(x, y)
expect_equivalent(coredata(z), data.,
                  info = paste(info_msg, "- default fill"))
z <- merge(x, y, fill = 21i)
expect_equivalent(coredata(z), d21,
                  info = paste(info_msg, "- fill = 21i"))

.index(x) <- as.integer(.index(x))
.index(y) <- as.integer(.index(y))
z <- merge(x, y)
expect_equivalent(coredata(z), data.,
                  info = paste(info_msg, "- default fill, integer index"))
z <- merge(x, y, fill = 21i)
expect_equivalent(coredata(z), d21,
                  info = paste(info_msg, "- fill = 21i, integer index"))


info_msg <- "test.suffixes_appended"
x <- xts(data.frame(x = 1), as.Date("2012-01-01"))
y <- xts(data.frame(x = 2), as.Date("2012-01-01"))
suffixes <- c("truex", "truey")
out <- merge(x, y, suffixes = suffixes)
expect_equal(paste0("x", suffixes), colnames(out), info = info_msg)

info_msg <- "test.suffix_append_order"
idx <- Sys.Date() - 1:10
x1 <- xts(cbind(alpha = 1:10, beta = 2:11), idx)
x2 <- xts(cbind(alpha = 3:12, beta = 4:13), idx)
x3 <- xts(cbind(alpha = 5:14, beta = 6:15), idx)

suffixes <- LETTERS[1:3]

mx <- merge(x1, x2, x3, suffixes = paste0('.', suffixes))
mz <- merge.zoo(x1, x2, x3, suffixes = suffixes)

expect_equal(mx, as.xts(mz), info = info_msg)


### merging zero-width objects
z1 <- structure(numeric(0),
  index = structure(1:10, class = "Date"), class = "zoo")
x1 <- as.xts(z1)
z2 <- structure(numeric(0),
  index = structure(5:14, class = "Date"), class = "zoo")
x2 <- as.xts(z2)

info_msg <- "merge.xts() on zero-width objects and all = TRUE matches merge.zoo()"
z3 <- merge(z1, z2, all = TRUE)
x3 <- merge(x1, x2, all = TRUE)
# use expect_equivalent because xts index has tclass and tzone and zoo doesn't
expect_equivalent(index(z3), index(x3), info = info_msg)

info_msg <- "merge.xts() zero-width objects and all = FALSE matches merge.zoo()"
z4 <- merge(z1, z2, all = FALSE)
x4 <- merge(x1, x2, all = FALSE)
# use expect_equivalent because xts index has tclass and tzone and zoo doesn't
expect_equivalent(index(z4), index(x4), info = info_msg)

info_msg <- "merge.xts() on zero-width objects and all = c(TRUE, FALSE) matches merge.zoo()"
z5 <- merge(z1, z2, all = c(TRUE, FALSE))
x5 <- merge(x1, x2, all = c(TRUE, FALSE))
# use expect_equivalent because xts index has tclass and tzone and zoo doesn't
expect_equivalent(index(z5), index(x5), info = info_msg)

info_msg <- "merge.xts() on zero-width objects and all = c(FALSE, TRUE) matches merge.zoo()"
z6 <- merge(z1, z2, all = c(FALSE, TRUE))
x6 <- merge(x1, x2, all = c(FALSE, TRUE))
# use expect_equivalent because xts index has tclass and tzone and zoo doesn't
expect_equivalent(index(z6), index(x6), info = info_msg)
