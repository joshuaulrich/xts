na <- NA_integer_

# vector with even length, odd length

# no/yes result (potential infinite loop)
# https://www.topcoder.com/community/data-science/data-science-tutorials/binary-search/
info_msg <- "integer predicate no yes stops"
ans <- 2L
ivec <- 3:4
ikey <- ivec[ans]
expect_identical(ans, xts:::binsearch(ikey, ivec, TRUE), paste(info_msg, TRUE))
expect_identical(ans, xts:::binsearch(ikey, ivec, FALSE), paste(info_msg, FALSE))


# small steps between vector elements (test that we actually stop)
info_msg <- "test.double_with_small_delta_stops"
ans <- 10L
dvec <- 1 + (-10:10 / 1e8)
dkey <- dvec[ans]
expect_identical(ans, xts:::binsearch(dkey, dvec, TRUE))
expect_identical(ans, xts:::binsearch(dkey, dvec, FALSE))

info_msg <- "test.find_first_zero_even_length"
ivec <- sort(c(0L, -3:5L))
dvec <- ivec * 1.0
expect_identical(4L, xts:::binsearch(0L,  ivec, TRUE))
expect_identical(4L, xts:::binsearch(0.0, dvec, TRUE))

info_msg <- "test.find_last_zero_even_length"
ivec <- sort(c(0L, -3:5L))
dvec <- ivec * 1.0
expect_identical(5L, xts:::binsearch(0L,  ivec, FALSE))
expect_identical(5L, xts:::binsearch(0.0, dvec, FALSE))

info_msg <- "test.find_first_zero_odd_length"
ivec <- sort(c(0L, -3:5L))
dvec <- ivec * 1.0
expect_identical(4L, xts:::binsearch(0L,  ivec, TRUE))
expect_identical(4L, xts:::binsearch(0.0, dvec, TRUE))

info_msg <- "test.find_last_zero_odd_length"
ivec <- sort(c(0L, -3:5L))
dvec <- ivec * 1.0
expect_identical(5L, xts:::binsearch(0L,  ivec, FALSE))
expect_identical(5L, xts:::binsearch(0.0, dvec, FALSE))


# key is outside of vector
info_msg <- "test.key_less_than_min"
ivec <- 1:6
expect_identical(1L, xts:::binsearch(-9L, ivec, TRUE))
expect_identical(na, xts:::binsearch(-9L, ivec, FALSE))
dvec <- ivec * 1.0
expect_identical(1L, xts:::binsearch(-9,  dvec, TRUE))
expect_identical(na, xts:::binsearch(-9,  dvec, FALSE))

info_msg <- "test.key_greater_than_max"
ivec <- 1:6
expect_identical(na, xts:::binsearch( 9L, ivec, TRUE))
expect_identical(6L, xts:::binsearch( 9L, ivec, FALSE))
dvec <- ivec * 1.0
expect_identical(na, xts:::binsearch( 9,  dvec, TRUE))
expect_identical(6L, xts:::binsearch( 9,  dvec, FALSE))


# key is NA
info_msg <- "test.key_is_NA"
ivec <- 1:6
ikey <- NA_integer_
expect_identical(na, xts:::binsearch(ikey, ivec, TRUE))
expect_identical(na, xts:::binsearch(ikey, ivec, FALSE))

dvec <- ivec * 1.0
dkey <- NA_real_
expect_identical(na, xts:::binsearch(dkey, dvec, TRUE))
expect_identical(na, xts:::binsearch(dkey, dvec, FALSE))


# key is zero-length
info_msg <- "test.key_is_zero_length"
# have empty key return NA
ivec <- 1:6
ikey <- integer()
expect_identical(na, xts:::binsearch(ikey, ivec, TRUE))
expect_identical(na, xts:::binsearch(ikey, ivec, FALSE))

dvec <- ivec * 1.0
dkey <- double()
expect_identical(na, xts:::binsearch(dkey, dvec, TRUE))
expect_identical(na, xts:::binsearch(dkey, dvec, FALSE))


# vec is zero-length
info_msg <- "test.vec_is_zero_length"
# have empty vector return NA
ivec <- integer()
ikey <- 0L
expect_identical(na, xts:::binsearch(ikey, ivec, TRUE))
expect_identical(na, xts:::binsearch(ikey, ivec, FALSE))

dvec <- double()
dkey <- 0.0
expect_identical(na, xts:::binsearch(dkey, dvec, TRUE))
expect_identical(na, xts:::binsearch(dkey, dvec, FALSE))
