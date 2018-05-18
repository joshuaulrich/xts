# Tests for isOrdered()
#

# Utility functions for tests {{{
run.isOrdered <- function(x) {
  c(isOrdered(x,  TRUE,  TRUE),
    isOrdered(x,  TRUE, FALSE),
    isOrdered(x, FALSE, FALSE),
    isOrdered(x, FALSE,  TRUE))
}
check.isOrdered <- function(x, v = rep(TRUE, 4)) {
  xc <- paste(capture.output(dput(x)), collapse = " ")
  checkIdentical(v[1], isOrdered(x,  TRUE,  TRUE), paste(xc, v[1], "increasing, strictly"))
  checkIdentical(v[2], isOrdered(x,  TRUE, FALSE), paste(xc, v[2], "increasing"))
  checkIdentical(v[3], isOrdered(x, FALSE, FALSE), paste(xc, v[3], "decreasing"))
  checkIdentical(v[4], isOrdered(x, FALSE,  TRUE), paste(xc, v[4], "decreasing, strictly"))
}
# }}}

TTTT <- rep(TRUE, 4)
FFFF <- !TTTT
TTFF <- c(TRUE, TRUE, FALSE, FALSE)
FFTT <- !TTFF

# Increasing {{{
test.isOrdered_incr <- function() {
  check.isOrdered(1:3, TTFF)
  check.isOrdered(-1:1, TTFF)
  check.isOrdered(c(1, 2, 3), TTFF)
  check.isOrdered(c(-1, 0, 1), TTFF)
}
### NA, NaN, Inf
# beg
test.isOrdered_incr_begNA <- function() {
  check.isOrdered(c(NA_integer_, 1L, 2L), FFFF)
  check.isOrdered(c(NA_real_, 1, 2), TTFF)
  check.isOrdered(c(NaN, 1, 2), TTFF)
  check.isOrdered(c(Inf, 1, 2), FFFF)
  check.isOrdered(c(-Inf, 1, 2), TTFF)
}
# mid
test.isOrdered_incr_midNA <- function() {
  check.isOrdered(c(1L, NA_integer_, 2L), FFFF)
  check.isOrdered(c(1, NA_real_, 2), FFFF)
  check.isOrdered(c(1, NaN, 2), FFFF)
  check.isOrdered(c(1, Inf, 2), FFFF)
  check.isOrdered(c(1, -Inf, 2), FFFF)
}
# end
test.isOrdered_incr_endNA <- function() {
  check.isOrdered(c(1L, 2L, NA_integer_), TTFF)
  check.isOrdered(c(1, 2, NA_real_), TTFF)
  check.isOrdered(c(1, 2, NaN), TTFF)
  check.isOrdered(c(1, 2, Inf), TTFF)
  check.isOrdered(c(1, 2, -Inf), FFFF)
}
###
# }}}

# Decreasing {{{
test.isOrdered_decr <- function() {
  check.isOrdered(1:-1, FFTT)
  check.isOrdered(3:1, FFTT)
  check.isOrdered(c(3, 2, 1), FFTT)
  check.isOrdered(c(1, 0, -1), FFTT)
}
### NA, NaN, Inf
# beg
test.isOrdered_decr_begNA <- function() {
  check.isOrdered(c(NA_integer_, 2L, 1L), FFTT)
  check.isOrdered(c(NA_real_, 2, 1), FFTT)
  check.isOrdered(c(NaN, 2, 1), FFTT)
  check.isOrdered(c(Inf, 2, 1), FFTT)
  check.isOrdered(c(-Inf, 2, 1), FFFF)
}
# mid
test.isOrdered_decr_midNA <- function() {
  check.isOrdered(c(2L, NA_integer_, 1L), FFFF)
  check.isOrdered(c(2, NA_real_, 1), TTTT)
  check.isOrdered(c(2, NaN, 1), TTTT)
  check.isOrdered(c(2, Inf, 1), FFFF)
  check.isOrdered(c(2, -Inf, 1), FFFF)
}
# end
test.isOrdered_decr_endNA <- function() {
  check.isOrdered(c(2L, 1L, NA_integer_), FFFF)
  check.isOrdered(c(2, 1, NA_real_), FFTT)
  check.isOrdered(c(2, 1, NaN), FFTT)
  check.isOrdered(c(2, 1, Inf), FFFF)
  check.isOrdered(c(2, 1, -Inf), FFTT)
}
###
# }}}

FTTF <- c(FALSE, TRUE, TRUE, FALSE)
TFFT <- !FTTF

# Zero length
test.isOrdered_length_0 <- function() {
  check.isOrdered(numeric(0), TTTT)
  check.isOrdered(integer(0), TTTT)
}

# "Scalar"
test.isOrdered_length_1 <- function() {
  check.isOrdered(1.0, TTTT)
  check.isOrdered(1L, TTTT)
}

# Two-elements
test.isOrdered_length_2_dup <- function() {
  check.isOrdered(c(1.0, 1.0), FTTF)
  check.isOrdered(c(1L, 1L), FTTF)
}

test.isOrdered_length_2_incr <- function() {
  check.isOrdered(c(1.0, 2.0), TTFF)
  check.isOrdered(c(1L, 2L), TTFF)
}

test.isOrdered_length_2_decr <- function() {
  check.isOrdered(c(2.0, 1.0), FFTT)
  check.isOrdered(c(2L, 1L), FFTT)
}

# From xts/src/merge.c
#/* Check for illegal values before looping. Due to ordered index,
# * -Inf must be first, while NA, Inf, and NaN must be last. */
# if (!R_FINITE(real_xindex[0]) || !R_FINITE(real_xindex[nrx-1])

# From xts/src/merge.c
#/* Check for NA before looping; logical ops on NA may yield surprising
# * results. Note that the NA_integer_ will appear in the last value of
# * the index because of sorting at the R level, even though NA_INTEGER
# * equals INT_MIN at the C level. */
# if (int_xindex[nrx-1] == NA_INTEGER || int_yindex[nry-1] == NA_INTEGER)

# Consecutive NA
# Note that zoo always puts NA at the end of the index. Always.
FTFF <- c(FALSE, TRUE, FALSE, FALSE)

test.isOrdered_incr_beg_consecNA <- function() {
  check.isOrdered(c(NA_integer_, NA_integer_, 1L, 2L), FTFF)
  check.isOrdered(c(NA_real_, NA_real_, 1, 2), FTFF)
  check.isOrdered(c(NaN, NaN, 1, 2), FTFF)
  check.isOrdered(c(Inf, Inf, 1, 2), FFFF)
  check.isOrdered(c(-Inf, -Inf, 1, 2), TTFF)
}

test.isOrdered_decr_end_consecNA <- function() {
  check.isOrdered(c(2L, 1L, NA_integer_, NA_integer_), FFFF)
  check.isOrdered(c(2, 1, NA_real_, NA_real_), FFTT)
  check.isOrdered(c(2, 1, NaN, NaN), FFTT)
  check.isOrdered(c(2, 1, Inf, Inf), FFFF)
  check.isOrdered(c(2, 1, -Inf, -Inf), FFTT)
}
