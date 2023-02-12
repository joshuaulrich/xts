# Tests for isOrdered()

# Utility functions for tests {{{
check.isOrdered <- function(x, v = rep(TRUE, 4), msg = "") {
  xc <- paste(capture.output(dput(x)), collapse = " ")
  expect_identical(v[1], isOrdered(x,  TRUE,  TRUE), info = paste(msg, xc, v[1], "increasing, strictly"))
  expect_identical(v[2], isOrdered(x,  TRUE, FALSE), info = paste(msg, xc, v[2], "increasing"))
  expect_identical(v[3], isOrdered(x, FALSE, FALSE), info = paste(msg, xc, v[3], "decreasing"))
  expect_identical(v[4], isOrdered(x, FALSE,  TRUE), info = paste(msg, xc, v[4], "decreasing, strictly"))
}
# }}}

TTTT <- rep(TRUE, 4)
FFFF <- !TTTT
TTFF <- c(TRUE, TRUE, FALSE, FALSE)
FFTT <- !TTFF

# Increasing {{{
info_msg <- "test.isOrdered_incr"
check.isOrdered(1:3, TTFF, info_msg)
check.isOrdered(-1:1, TTFF, info_msg)
check.isOrdered(c(1, 2, 3), TTFF, info_msg)
check.isOrdered(c(-1, 0, 1), TTFF, info_msg)

### NA, NaN, Inf
# beg
info_msg <- "test.isOrdered_incr_begNA"
check.isOrdered(c(NA_integer_, 1L, 2L), FFFF, info_msg)
check.isOrdered(c(NA_real_, 1, 2), TTFF, info_msg)
check.isOrdered(c(NaN, 1, 2), TTFF, info_msg)
check.isOrdered(c(Inf, 1, 2), FFFF, info_msg)
check.isOrdered(c(-Inf, 1, 2), TTFF, info_msg)

# mid
info_msg <- "test.isOrdered_incr_midNA"
check.isOrdered(c(1L, NA_integer_, 2L), FFFF, info_msg)
check.isOrdered(c(1, NA_real_, 2), TTTT, info_msg)
check.isOrdered(c(1, NaN, 2), TTTT, info_msg)
check.isOrdered(c(1, Inf, 2), FFFF, info_msg)
check.isOrdered(c(1, -Inf, 2), FFFF, info_msg)

# end
info_msg <- "test.isOrdered_incr_endNA"
check.isOrdered(c(1L, 2L, NA_integer_), TTFF, info_msg)
check.isOrdered(c(1, 2, NA_real_), TTFF, info_msg)
check.isOrdered(c(1, 2, NaN), TTFF, info_msg)
check.isOrdered(c(1, 2, Inf), TTFF, info_msg)
check.isOrdered(c(1, 2, -Inf), FFFF, info_msg)

###
# }}}

# Decreasing {{{
info_msg <- "test.isOrdered_decr"
check.isOrdered(1:-1, FFTT, info_msg)
check.isOrdered(3:1, FFTT, info_msg)
check.isOrdered(c(3, 2, 1), FFTT, info_msg)
check.isOrdered(c(1, 0, -1), FFTT, info_msg)

### NA, NaN, Inf
# beg
info_msg <- "test.isOrdered_decr_begNA"
check.isOrdered(c(NA_integer_, 2L, 1L), FFTT, info_msg)
check.isOrdered(c(NA_real_, 2, 1), FFTT, info_msg)
check.isOrdered(c(NaN, 2, 1), FFTT, info_msg)
check.isOrdered(c(Inf, 2, 1), FFTT, info_msg)
check.isOrdered(c(-Inf, 2, 1), FFFF, info_msg)

# mid
info_msg <- "test.isOrdered_decr_midNA"
check.isOrdered(c(2L, NA_integer_, 1L), FFFF, info_msg)
check.isOrdered(c(2, NA_real_, 1), TTTT, info_msg)
check.isOrdered(c(2, NaN, 1), TTTT, info_msg)
check.isOrdered(c(2, Inf, 1), FFFF, info_msg)
check.isOrdered(c(2, -Inf, 1), FFFF, info_msg)

# end
info_msg <- "test.isOrdered_decr_endNA"
check.isOrdered(c(2L, 1L, NA_integer_), FFFF, info_msg)
check.isOrdered(c(2, 1, NA_real_), FFTT, info_msg)
check.isOrdered(c(2, 1, NaN), FFTT, info_msg)
check.isOrdered(c(2, 1, Inf), FFFF, info_msg)
check.isOrdered(c(2, 1, -Inf), FFTT, info_msg)
###
# }}}
