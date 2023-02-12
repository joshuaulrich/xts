info_msg <- "na.fill.xts() matches na.fill.zoo() when object has 1 column and 'fill' is scalar"
x <- .xts(1:20, 1:20)
is.na(x) <- sample(20, 10)
z <- as.zoo(x)
x_out <- coredata(na.fill(x, 0))
z_out <- coredata(na.fill(z, 0))
expect_equal(z_out, x_out, info = info_msg)

info_msg <- "na.fill.xts() matches na.fill.zoo() when object has 2 columns and 'fill' is scalar"
x <- .xts(cbind(1:10, 1:10), 1:10)
is.na(x[,1]) <- sample(10, 5)
is.na(x[,2]) <- sample(10, 5)
z <- as.zoo(x)
x_out <- coredata(na.fill(x, 0))
z_out <- coredata(na.fill(z, 0))
# z_out has dimnames (both NULL) for some reason
dimnames(z_out) <- NULL
expect_equal(z_out, x_out, info = info_msg)
