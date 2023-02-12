info_msg <- "test.coredata_vector"
x <- xts(1, as.Date("2018-03-02"))
z <- as.zoo(x)
expect_identical(target = coredata(z),
                 current = coredata(x),
                 info = info_msg)


info_msg <- "test.coredata_named_vector"
x <- xts(c(hello = 1), as.Date("2018-03-02"))
z <- as.zoo(x)
expect_identical(coredata(z),
                 coredata(x),
                 info = info_msg)


info_msg <- "test.coredata_matrix"
x <- xts(cbind(1, 9), as.Date("2018-03-02"))
z <- as.zoo(x)
expect_identical(coredata(z),
                 coredata(x),
                 info = info_msg)


info_msg <- "test.coredata_named_matrix"
x <- xts(cbind(hello = 1, world = 9), as.Date("2018-03-02"))
z <- as.zoo(x)
expect_identical(coredata(z),
                 coredata(x),
                 info = info_msg)


info_msg <- "test.coredata_data.frame"
x <- xts(data.frame(hello = 1, world = 9), as.Date("2018-03-02"))
z <- as.zoo(x)

expect_identical(coredata(z),
                 coredata(x),
                 info = info_msg)


info_msg <- "test.coredata_ts"
x <- xts(ts(1), as.Date("2018-03-02"))
z <- as.zoo(x)

expect_identical(coredata(z),
                 coredata(x),
                 info = info_msg)


# empty objects
info_msg <- "test.coredata_empty"
x <- xts(, as.Date("2018-03-02"))
z <- as.zoo(x)

expect_identical(coredata(z),
                 coredata(x),
                 info = info_msg)


info_msg <- "test.coredata_empty_dim"
x <- xts(cbind(1, 9), as.Date("2018-03-02"))
z <- as.zoo(x)
x0 <- x[0,]
z0 <- z[0,]

expect_identical(coredata(z0),
                 coredata(x0),
                 info = info_msg)


info_msg <- "test.coredata_empty_dim_dimnames"
x <- xts(cbind(hello = 1, world = 9), as.Date("2018-03-02"))
z <- as.zoo(x)
x0 <- x[0,]
z0 <- z[0,]

expect_identical(coredata(z0),
                 coredata(x0),
                 info = info_msg)
