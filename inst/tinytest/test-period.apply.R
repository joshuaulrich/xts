# period.apply() doesn't care what generates the INDEX,
# but it does care that INDEX has the following characteristics:
# 1) the first element is zero,
# 2) the last element is nrow(x),
# 3) there are no duplicate elements,
# 4) the elements are sorted.
#

info_msg <- "test.duplicate_INDEX"
x <- .xts(1:10, 1:10)
ep <- c(0, 2, 4, 6, 8, 10)
nodup <- period.apply(x, ep, mean)
dup <- period.apply(x, c(ep, 10), mean)
expect_identical(nodup, dup, info = info_msg)

info_msg <- "test.duplicate_INDEX_vector"
x <- 1:10
ep <- c(0, 2, 4, 6, 8, 10)
nodup <- period.apply(x, ep, mean)
dup <- period.apply(x, c(ep, 10), mean)
expect_identical(nodup, dup, info = info_msg)

info_msg <- "test.unsorted_INDEX"
x <- .xts(1:10, 1:10)
ep.s <- c(2, 4, 6, 8)
ep.u <- sample(ep.s)
s <- period.apply(x, c(0, ep.s, 10), mean)
u <- period.apply(x, c(0, ep.u, 10), mean)
expect_identical(s, u, info = info_msg)

info_msg <- "test.unsorted_INDEX_vector"
x <- 1:10
ep.s <- c(2, 4, 6, 8)
ep.u <- sample(ep.s)
s <- period.apply(x, c(0, ep.s, 10), mean)
u <- period.apply(x, c(0, ep.u, 10), mean)
expect_identical(s, u, info = info_msg)

info_msg <- "test.INDEX_starts_with_zero"
x <- .xts(1:10, 1:10)
ep <- c(2, 4, 6, 8, 10)
a <- period.apply(x, ep, mean)
z <- period.apply(x, c(0, ep), mean)
expect_identical(a, z, info = info_msg)

info_msg <- "test.INDEX_starts_with_zero_vector"
x <- 1:10
ep <- c(2, 4, 6, 8, 10)
a <- period.apply(x, ep, mean)
z <- period.apply(x, c(0, ep), mean)
expect_identical(a, z, info = info_msg)

info_msg <- "test.INDEX_ends_with_lengthX"
x <- .xts(1:10, 1:10)
ep <- c(0, 2, 4, 6, 8)
a <- period.apply(x, ep, mean)
z <- period.apply(x, c(ep, 10), mean)
expect_identical(a, z, info = info_msg)

info_msg <- "test.INDEX_ends_with_lengthX_vector"
x <- 1:10
ep <- c(0, 2, 4, 6, 8)
a <- period.apply(x, ep, mean)
z <- period.apply(x, c(ep, 10), mean)
expect_identical(a, z, info = info_msg)

# check specific period.* functions
data(sample_matrix)
x <- as.xts(sample_matrix[,1], dateFormat = "Date")
e <- endpoints(x, "months")

info_msg <- "test.period.min_equals_apply.monthly"
# min
am <- apply.monthly(x, min)
pm <- period.min(x, e)
expect_equivalent(am, pm, info = info_msg)

info_msg <- "test.period.max_equals_apply.monthly"
# max
am <- apply.monthly(x, max)
pm <- period.max(x, e)
expect_equivalent(am, pm, info = info_msg)

info_msg <- "test.period.sum_equals_apply.monthly"
# sum
am <- apply.monthly(x, sum)
pm <- period.sum(x, e)
expect_equivalent(am, pm, info = info_msg)

info_msg <- "test.period.prod_equals_apply.monthly"
# prod
am <- apply.monthly(x, prod)
pm <- period.prod(x, e)
expect_equivalent(am, pm, info = info_msg)

# test that non-integer INDEX is converted to integer
info_msg <- "test.period.min_converts_index_to_integer"
storage.mode(e) <- "numeric"
pm <- period.min(x, e)

info_msg <- "test.period.max_converts_index_to_integer"
storage.mode(e) <- "numeric"
pm <- period.max(x, e)

info_msg <- "test.period.sum_converts_index_to_integer"
storage.mode(e) <- "numeric"
pm <- period.sum(x, e)

info_msg <- "test.period.prod_converts_index_to_integer"
storage.mode(e) <- "numeric"
pm <- period.prod(x, e)

# test conversion from intraday to daily or lower frequency
info_msg <- "test.intraday_to_daily"
set.seed(21)
i <- as.POSIXct("2013-02-05 01:01", tz = "America/Chicago")
x <- xts(rnorm(10000), i - 10000:1 * 60)
d <- to.daily(x)
dateseq <- seq(as.Date("2013-01-29"), as.Date("2013-02-05"), "day")
expect_equivalent(index(d), dateseq, info = info_msg)
