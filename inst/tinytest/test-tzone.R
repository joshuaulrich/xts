# These tests check the timezone attribute is attached to the expected
# component of the xts object. The xts constructors should no longer add
# 'tzone' or '.indexTZ' attributes to the xts object itself. Only the index
# should have a 'tzone' attribute. Construct xts objects using structure() to
# test behavior when functions encounter xts objects created before 0.10-3.
x <-
structure(1:5, .Dim = c(5L, 1L),
          index = structure(1:5, tzone = "", tclass = c("POSIXct", "POSIXt")),
          .indexCLASS = c("POSIXct", "POSIXt"),
          tclass = c("POSIXct", "POSIXt"),
          .indexTZ = "UTC", tzone = "UTC",
          class = c("xts", "zoo"))

info_msg <- "test.get_tzone"
expect_identical(tzone(x), "", info = info_msg)

info_msg <- "indexTZ(x) warns"
expect_warning(indexTZ(x))

info_msg <- "indexTZ(x) <- warns"
expect_warning(indexTZ(x) <- "GMT")

info_msg <- "tzone(x) <- `foo` removes tzone and .indexTZ from xts object"
y <- x
tzone(y) <- "GMT"
expect_identical(NULL, attr(y, "tzone"), info = info_msg)
expect_identical(NULL, attr(y, ".indexTZ"), info = info_msg)

info_msg <- "tzone(x) <- `foo` sets the tzone attribute on the index"
y <- x
tzone(y) <- "GMT"
expect_identical("GMT", attr(attr(y, "index"), "tzone"), info = info_msg)
expect_null(attr(y, ".indexTZ"),
            info = "tzone(x) <- `foo` removes .indexTZ attribute from xts object")



info_msg <- "tzone(x) <- NULL sets the tzone attribute on the index to '' (empty string)"
y <- x
tzone(y) <- NULL
expect_identical("", attr(attr(y, "index"), "tzone"), info = info_msg)

info_msg <- "coredata(x) removes tzone and .indexTZ from xts object"
y <- coredata(x)
expect_identical(NULL, attr(y, "tzone"), info = info_msg)
expect_identical(NULL, attr(y, ".indexTZ"), info = info_msg)

info_msg <- "xtsAttributes(x) does not include tzone or .indexTZ"
y <- xtsAttributes(x)
expect_identical(NULL, y$tzone, info = info_msg)
expect_identical(NULL, y$.indexTZ, info = info_msg)

info_msg <- "xtsAttributes(x) <- 'foo' removes tzone and .indexTZ"
y <- x
xtsAttributes(y) <- xtsAttributes(x)
expect_identical(NULL, attr(y, "tzone"), info = info_msg)
expect_identical(NULL, attr(y, ".indexTZ"), info = info_msg)

info_msg <- "tzone(x) <- `foo` always creates a character tzone"
x <- "hello"
tzone(x) <- 1
expect_identical(storage.mode(attr(x, "tzone")), "character", info = info_msg)

info_msg <- "zero-width subset has the same tzone as the input"
target <- "Ima/Tzone"
x <- .xts(1:10, 1:10, tzone = target)
y <- x[,0]
expect_equal(target, tzone(y), info = info_msg)
