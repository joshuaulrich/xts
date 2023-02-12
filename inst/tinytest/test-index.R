info_msg <- "test.get_index_does_not_error_if_index_has_no_attributes"
x <- .xts(1:3, 1:3, tzone = "UTC")
ix <- index(x)
ix <- ix + 3
attr(x, "index") <- 4:6
# get index (test will fail if it errors)
expect_warning(index(x), info = info_msg)

info_msg <- "test.set_.index_copies_index_attributes"
x <- .xts(1:3, 1:3, tzone = "UTC")
ix <- index(x)
ix <- ix + 3
.index(x) <- 4:6
expect_equal(index(x), ix, info = info_msg)

info_msg <- "test.set_index_copies_index_attributes"
x <- .xts(1:3, 1:3, tzone = "UTC")
ix <- index(x)
ix <- ix + 3
index(x) <- .POSIXct(4:6, "UTC")
expect_equal(index(x), ix, info = info_msg)

# x index must be numeric, because index<-.xts coerces RHS to numeric
info_msg <- "test.set_index_restores_tzone_attribute"
x <- .xts(1:3, 1:3+0, tzone = "")
y <- x
# Ops.POSIXt drops tzone attribute when tzone = ""
index(y) <- index(y) + 0
expect_identical(x, y, info = info_msg)

info_msg <- "test.get_index_zero_length_Date_returns_correct_index_type"
xd <- xts(1, .Date(1))
zd <- as.zoo(xd)
xd_index <- index(xd[0,])
expect_true(length(xd_index) == 0, info = paste(info_msg, "- length(index) == 0"))
expect_equal(index(xd[0,]), index(zd[0,]), info = info_msg)
expect_equal(index(xd[0,]), .Date(numeric()), info = info_msg)

info_msg <- "test.get_index_zero_length_POSIXct_returns_correct_index_type"
xp <- xts(1, .POSIXct(1), tzone = "UTC")
zp <- as.zoo(xp)
xp_index <- index(xp[0,])
zp_index <- index(zp[0,])
zl_index <- .POSIXct(numeric(), tz = "UTC")

expect_true(length(xp_index) == 0, info = paste(info_msg, "- length(index) == 0"))
expect_equal(tzone(xp_index), tzone(zp_index), info = info_msg)
expect_inherits(xp_index, c("POSIXct", "POSIXt"), info = info_msg)
