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



# results when function returns more than one row
dates <- seq.Date(as.Date("2010-01-01"), as.Date("2013-12-01"), by = "month")
x <- xts(cbind(seq_along(dates), rev(seq_along(dates))), dates)
y <- x
colnames(y) <- c("a", "b")
sx <- split(x, "years")
sy <- split(y, "years")

# this should be the same as do.call(rbind, split(x, "years"))
func <- cumsum  # function returns an xts object
px <- apply.yearly(x, func)
py <- apply.yearly(y, func)
target_x <- do.call(rbind, lapply(sx, func))
target_y <- do.call(rbind, lapply(sy, func))
expect_equal(target_x, px, info = "function returns multi-column xts object w/o colnames")
expect_equal(target_y, py, info = "function returns multi-column xts object w/colnames")

# should return one row per endpoint, where index(output[i]) <- end(input[i]) with a column for each function output.
func <- range  # function does *not* return an xts object
px <- apply.yearly(x, range)
py <- apply.yearly(y, range)
years <- seq.Date(as.Date("2010-12-01"), as.Date("2013-12-01"), by = "years")
target_x <- xts(do.call(rbind, lapply(sx, range)), years, dimnames = NULL)
target_y <- xts(do.call(rbind, lapply(sy, range)), years, dimnames = list(NULL, colnames(y)))
expect_equal(target_x, px, info = "function returns multi-column non-xts object w/o colnames")
expect_equal(target_y, py, info = "function returns multi-column non-xts object w/colnames")

## Always rbind() the rollapply() output if it's an xts object
## TODO: How do we handle if it isn't an xts object?
#roll_func <- function(x, bycol) {
#  rollapply(x, 3, sum, fill = NA, by.column = bycol)
#}
#p1 <- apply.yearly(x, roll_func, bycol = TRUE)
#t1 <- xts(do.call(rbind, lapply(sx, roll_func, bycol = TRUE)), dates, dimnames = NULL)
#expect_equal(t1, p1, info = "rolling function returns multi-column non-xts object w/o colnames")
#
#p2 <- apply.yearly(x, roll_func, bycol = FALSE)
#t2 <- xts(do.call(rbind, lapply(sx, roll_func, bycol = FALSE)), dates, dimnames = NULL)
#expect_equal(t2, p2, info = "rolling function returns single-column non-xts object w/o colnames")
#
#p1 <- apply.yearly(y, roll_func, bycol = TRUE)
#t1 <- xts(do.call(rbind, lapply(sy, roll_func, bycol = TRUE)), dates)
#expect_equal(t1, p1, info = "rolling function returns multi-column non-xts object w/colnames")
#
#p2 <- apply.yearly(y, roll_func, bycol = FALSE)
#t2 <- xts(do.call(rbind, lapply(sy, roll_func, bycol = FALSE)), dates, dimnames = NULL)
#expect_equal(t2, p2, info = "rolling function returns single-column non-xts object w/colnames")
#
## rolling function returns multiple columns
#roll_qtile <- function(x) {
#    rollapply(x, 5, quantile, fill = NA, by.column = FALSE)
#}
#p1 <- apply.yearly(x, roll_qtile)
#t1 <- xts(do.call(rbind, lapply(sx, roll_qtile)), dates)
#expect_equal(t1, p1, info = "rolling function returns multi-column non-xts object w/colnames")
#
#
#p1 <- apply.yearly(x, roll_func, bycol = TRUE)
#p2 <- apply.yearly(y, roll_func, bycol = FALSE)
#target_x <- xts(do.call(rbind, lapply(sx, roll_func, bycol = TRUE)), dates, dimnames = NULL)
#target_y <- xts(do.call(rbind, lapply(sy, roll_func, bycol = FALSE)), years, dimnames = list(NULL, colnames(y)))
#
## always rbind() the result when FUN returns a 1-row matrix
## . The index should be set to the last index value in the period.
##p <- period.apply()
#
#
