# period.apply() doesn't care what generates the INDEX,
# but it does care that INDEX has the following characteristics:
# 1) the first element is zero,
# 2) the last element is nrow(x),
# 3) there are no duplicate elements,
# 4) the elements are sorted.
#

test.duplicate_INDEX <- function() {
  x <- .xts(1:10, 1:10)
  ep <- c(0, 2, 4, 6, 8, 10)
  nodup <- period.apply(x, ep, mean)
  dup <- period.apply(x, c(ep, 10), mean)
  checkIdentical(nodup, dup)
}

test.duplicate_INDEX_vector <- function() {
  x <- 1:10
  ep <- c(0, 2, 4, 6, 8, 10)
  nodup <- period.apply(x, ep, mean)
  dup <- period.apply(x, c(ep, 10), mean)
  checkIdentical(nodup, dup)
}

test.unsorted_INDEX <- function() {
  x <- .xts(1:10, 1:10)
  ep.s <- c(2, 4, 6, 8)
  ep.u <- sample(ep.s)
  s <- period.apply(x, c(0, ep.s, 10), mean)
  u <- period.apply(x, c(0, ep.u, 10), mean)
  checkIdentical(s, u)
}

test.unsorted_INDEX_vector <- function() {
  x <- 1:10
  ep.s <- c(2, 4, 6, 8)
  ep.u <- sample(ep.s)
  s <- period.apply(x, c(0, ep.s, 10), mean)
  u <- period.apply(x, c(0, ep.u, 10), mean)
  checkIdentical(s, u)
}

test.INDEX_starts_with_zero <- function() {
  x <- .xts(1:10, 1:10)
  ep <- c(2, 4, 6, 8, 10)
  a <- period.apply(x, ep, mean)
  z <- period.apply(x, c(0, ep), mean)
  checkIdentical(a, z)
}

test.INDEX_starts_with_zero_vector <- function() {
  x <- 1:10
  ep <- c(2, 4, 6, 8, 10)
  a <- period.apply(x, ep, mean)
  z <- period.apply(x, c(0, ep), mean)
  checkIdentical(a, z)
}

test.INDEX_ends_with_lengthX <- function() {
  x <- .xts(1:10, 1:10)
  ep <- c(0, 2, 4, 6, 8)
  a <- period.apply(x, ep, mean)
  z <- period.apply(x, c(ep, 10), mean)
  checkIdentical(a, z)
}

test.INDEX_ends_with_lengthX_vector <- function() {
  x <- 1:10
  ep <- c(0, 2, 4, 6, 8)
  a <- period.apply(x, ep, mean)
  z <- period.apply(x, c(ep, 10), mean)
  checkIdentical(a, z)
}
