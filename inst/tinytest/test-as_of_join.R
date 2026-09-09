# Create mock data for tests
library(xts)
set.seed(42)
hours <- timeBasedSeq("2023-01-01/2023-01-02/H")
hours_xts <- xts(seq_along(hours), hours + 60 * runif(length(hours)))

mins <- timeBasedSeq("2023-01-01/2023-01-02/M")
mins_xts <- xts(seq_along(mins), mins)

# Test 1: Left join using our fast C function
info_msg <- "Left join result row count and index match left object"
left_res <- as_of_join(hours_xts, mins_xts, join = "left")
expect_equal(nrow(left_res), nrow(hours_xts), info = info_msg)
expect_equal(index(left_res), index(hours_xts), info = info_msg)

# Minimal reproducible example:
# Ensure as-of join works on simple data
info_msg <- "as-of join merges nearest previous record"
x <- .xts(1:3, c(1, 5, 10))
y <- .xts(11:13, c(2, 4, 9))
res <- as_of_join(x, y, join="left")
expect_equal(NROW(res), 3, info = info_msg)
expect_equal(as.numeric(res[,2]), c(NA, 12, 13), info = info_msg)

# Test 3: Edge cases with zero-column objects
info_msg <- "as-of join handles zero-column objects gracefully"
y_empty <- .xts(matrix(numeric(0), nrow=3, ncol=0), c(1, 5, 10))
res_empty <- as_of_join(x, y_empty, join="left")
expect_equal(NROW(res_empty), 3, info = info_msg)
expect_equal(NCOL(res_empty), 1, info = info_msg) # only x columns

info_msg <- "as-of join handles right zero-column object"
res_empty2 <- as_of_join(y_empty, x, join="left")
expect_equal(NROW(res_empty2), 3, info = info_msg)
expect_equal(NCOL(res_empty2), 1, info = info_msg)

# Test 4: Missing index matches
info_msg <- "as-of join returns NA for indices before the first y value"
x2 <- .xts(1, 1)
y2 <- .xts(1, 2)
res2 <- as_of_join(x2, y2, join="left")
expect_true(is.na(res2[,2]), info = info_msg)
