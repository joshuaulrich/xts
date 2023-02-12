# make.index.unique

info_msg <- "make.index.unique() uses 1 microsecond epsilon by default"
x <- .xts(1:5, rep(1e-6, 5))
y <- make.index.unique(x)
expect_equivalent(target = cumsum(rep(1e-6, 5)),
                  current = .index(y),
                  info = info_msg)

info_msg <- "make.index.unique() warns when index value will be overwritten"
x <- .xts(1:5, c(rep(1e-6, 4), 3e-6))
expect_warning(make.index.unique(x, eps = 1e-6),
               pattern = "index value is unique but will be replaced",
               info = info_msg)

info_msg <- "make.index.unique() returns unique and sorted index"
expect_equivalent(target = cumsum(rep(1e-6, 5)),
                  current = .index(y),
                  info = info_msg)

info_msg <- "test.make.index.unique_adds_eps_to_duplicates"
epsilon <- c(1e-6, 1e-7, 1e-8)
for (eps in epsilon) {
    x <- .xts(1:5, rep(eps, 5))
    y <- make.index.unique(x, eps = eps)
    expect_equivalent(target = .index(y),
                      current = cumsum(rep(eps, 5)),
                      info = info_msg)
}

info_msg <- "test.make.index.unique_no_warn_if_unique_timestamps_unchanged"
x <- .xts(1:10, c(rep(1e-6, 9), 1e-5))
y <- make.index.unique(x, eps = 1e-6)
expect_equivalent(target = .index(y),
                  current = cumsum(rep(1e-6, 10)),
                  info = info_msg)

# There should be a warning if the cumulative epsilon for a set of duplicate
# index values is larger than the first unique index value that follows.
# When this happens, we will overwrite that non-duplicate index value with
# the prior index value + eps.
info_msg <- "test.make.index.unique_warns_if_unique_timestamp_changes"
x <- .xts(1:5, c(rep(0, 4), 2e-6))
expect_warning(make.index.unique(x, eps = 1e-6))

# There should be a warning if the cumulative epsilon for a set of duplicate
# index values is larger than the first unique index value that follows.
# When this happens, we will overwrite that non-duplicate index value with
# the prior index value + eps.
info_msg <- "test.make.index.unique_warns_ONCE_if_unique_timestamp_changes"
x <- .xts(1:5, c(rep(0, 3), 2, 3) * 1e-6)
count <- 0L
expect_warning(make.index.unique(x, eps = 1e-6))

info_msg <- "test.make.index.unique_converts_date_index_to_POSIXct"
# It doesn't make sense to add a small epsilon to a date index. The C code
# converts the integer index to a double, but it keeps the same index class.
# The index class should be converted to POSIXct.
