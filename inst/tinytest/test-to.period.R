# ensure first group is included in output
info_msg <- "test.to.frequency_includes_first_group"
data(sample_matrix)
x <- as.xts(sample_matrix)
x$Volume <- 1
tf <- xts:::to.frequency(x, x$Volume, 90, name=NULL)
tp <- .Call(xts:::C_toPeriod, x, c(0L, 90L, 180L), TRUE, 5L, FALSE, FALSE,
            c("Open", "High", "Low", "Close", "Volume"))
expect_identical(tf, tp, info = info_msg)

info_msg <- "test.to.period_custom_endpoints"
data(sample_matrix)
x <- as.xts(sample_matrix)
ep <- endpoints(x, "months", 1)
y1 <- to.period(x, "months", 1)
y2 <- to.period(x, ep)
expect_identical(y1, y2, info = info_msg)

# period must be character or numeric
expect_error(to.period(x, TRUE), info = "period must be character or numeric")

# 'k' and 'indexAt' are ignored
expect_warning(to.period(x, ep, k = 2),
               info = "'k' is ignored when endpoints are provided")
expect_warning(to.period(x, ep, indexAt = ""),
               info = "'indexAt' is ignored when endpoints are provided")
