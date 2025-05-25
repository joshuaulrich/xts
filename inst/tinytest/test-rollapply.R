info_msg <- "test rollapply.xts() handles multi-element 'width'"
x <- .xts(1:6, 1:6)
rsum <- rollapply(x, width = 1:6, FUN = sum)
csum <- cumsum(x)
expect_equal(rsum, csum, info = info_msg)
# ensure rollapply.xts() gives same result as rollapply.zoo()
zsum <- as.xts(rollapplyr(as.zoo(x), width = 1:6, FUN = sum))
expect_equivalent(rsum, zsum, info = info_msg)
