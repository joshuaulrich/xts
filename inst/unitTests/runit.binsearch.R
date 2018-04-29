na <- NA_integer_

# vector with even length, odd length

# no/yes result (potential infinite loop)
# https://www.topcoder.com/community/data-science/data-science-tutorials/binary-search/
test.integer_predicate_no_yes_stops <- function() {
  ans <- 2L
  ivec <- 3:4
  ikey <- ivec[ans]
  checkIdentical(ans, xts:::binsearch(ikey, ivec, TRUE))
  checkIdentical(ans, xts:::binsearch(ikey, ivec, FALSE))
  #checkIdentical(ans, xts:::binsearch(ikey, ivec, NULL))
}

# small steps between vector elements (test that we actually stop)
test.double_with_small_delta_stops <- function() {
  ans <- 10L
  dvec <- 1 + (-10:10 / 1e8)
  dkey <- dvec[ans]
  checkIdentical(ans, xts:::binsearch(dkey, dvec, TRUE))
  checkIdentical(ans, xts:::binsearch(dkey, dvec, FALSE))
  #checkIdentical(ans, xts:::binsearch(dkey, dvec, NULL))
}

test.find_first_zero_even_length <- function() {
  ivec <- sort(c(0L, -3:5L))
  dvec <- ivec * 1.0
  checkIdentical(4L, xts:::binsearch(0L,  ivec, TRUE))
  checkIdentical(4L, xts:::binsearch(0.0, dvec, TRUE))
}

test.find_last_zero_even_length <- function() {
  ivec <- sort(c(0L, -3:5L))
  dvec <- ivec * 1.0
  checkIdentical(5L, xts:::binsearch(0L,  ivec, FALSE))
  checkIdentical(5L, xts:::binsearch(0.0, dvec, FALSE))
  #checkIdentical(5L, xts:::binsearch(0L,  ivec, NULL))
  #checkIdentical(5L, xts:::binsearch(0.0, dvec, NULL))
}

test.find_first_zero_odd_length <- function() {
  ivec <- sort(c(0L, -3:5L))
  dvec <- ivec * 1.0
  checkIdentical(4L, xts:::binsearch(0L,  ivec, TRUE))
  checkIdentical(4L, xts:::binsearch(0.0, dvec, TRUE))
}

test.find_last_zero_odd_length <- function() {
  ivec <- sort(c(0L, -3:5L))
  dvec <- ivec * 1.0
  checkIdentical(5L, xts:::binsearch(0L,  ivec, FALSE))
  checkIdentical(5L, xts:::binsearch(0.0, dvec, FALSE))
  #checkIdentical(5L, xts:::binsearch(0L,  ivec, NULL))
  #checkIdentical(5L, xts:::binsearch(0.0, dvec, NULL))
}

# key is outside of vector
test.key_less_than_min <- function() {
  # FIXME: should binsearch() return a value < length(vec)?
  ivec <- 1:6
  checkIdentical(1L, xts:::binsearch(-9L, ivec, TRUE))
#  checkIdentical(0L, xts:::binsearch(-9L, ivec, FALSE))
  #checkIdentical(na, xts:::binsearch(-9L, ivec, NULL))
  dvec <- ivec * 1.0
  checkIdentical(1L, xts:::binsearch(-9,  dvec, TRUE))
#  checkIdentical(0L, xts:::binsearch(-9,  dvec, FALSE))
  #checkIdentical(na, xts:::binsearch(-9,  dvec, NULL))
}

test.key_greater_than_max <- function() {
  # FIXME: should binsearch() return a value > length(vec)?
  ivec <- 1:6
#  checkIdentical(7L, xts:::binsearch( 9L, ivec, TRUE))
  checkIdentical(6L, xts:::binsearch( 9L, ivec, FALSE))
  #checkIdentical(na, xts:::binsearch( 9L, ivec, NULL))
  dvec <- ivec * 1.0
#  checkIdentical(7L, xts:::binsearch( 9,  dvec, TRUE))
  checkIdentical(6L, xts:::binsearch( 9,  dvec, FALSE))
  #checkIdentical(na, xts:::binsearch( 9,  dvec, NULL))
}

# key is NA
test.key_is_NA <- function() {
  # FIXME: should binsearch() return non-NA if key is NA?
  # NA_integer_ will be first and NA_real_ will be last in sorted vec
  ivec <- 1:6
  ikey <- NA_integer_
#  checkIdentical(na, xts:::binsearch(ikey, ivec, TRUE))
  checkIdentical(na, xts:::binsearch(ikey, ivec, FALSE))
  #checkIdentical(na, xts:::binsearch(ikey, ivec, NULL))
  dvec <- ivec * 1.0
  dkey <- NA_real_
  checkIdentical(na, xts:::binsearch(dkey, dvec, TRUE))
#  checkIdentical(na, xts:::binsearch(dkey, dvec, FALSE))
  #checkIdentical(na, xts:::binsearch(dkey, dvec, NULL))
}

# key is zero-length
test.key_is_zero_length <- function() {
  # TODO: determine what should happen
  ivec <- 1:6
  ikey <- integer()
  checkIdentical(na, xts:::binsearch(ikey, ivec, TRUE))
#  checkIdentical(na, xts:::binsearch(ikey, ivec, FALSE))
  #checkIdentical(na, xts:::binsearch(ikey, ivec, NULL))
  dvec <- ivec * 1.0
  dkey <- double()
#  checkIdentical(na, xts:::binsearch(dkey, dvec, TRUE))
  checkIdentical(na, xts:::binsearch(dkey, dvec, FALSE))
  #checkIdentical(na, xts:::binsearch(dkey, dvec, NULL))
}

# vec is zero-length
test.vec_is_zero_length <- function() {
  # TODO: determine what should happen
  ivec <- integer()
  ikey <- 0L
  checkIdentical(na, xts:::binsearch(ikey, ivec, TRUE))
  checkIdentical(na, xts:::binsearch(ikey, ivec, FALSE))
  #checkIdentical(na, xts:::binsearch(ikey, ivec, NULL))
  dvec <- double()
  dkey <- 0.0
  checkIdentical(na, xts:::binsearch(dkey, dvec, TRUE))
  checkIdentical(na, xts:::binsearch(dkey, dvec, FALSE))
  #checkIdentical(na, xts:::binsearch(dkey, dvec, NULL))
}

