# These tests check the time class attribute is attached to the expected
# component of the xts object. The xts constructors should no longer add
# 'tclass' or '.indexClass' attributes to the xts object itself. Only the index
# should have a 'tclass' attribute. Construct xts objects using structure() to
# test behavior when functions encounter xts objects created before 0.10-3.
x <-
structure(1:5, .Dim = c(5L, 1L),
          index = structure(1:5, tzone = "", tclass = c("POSIXct", "POSIXt")),
          .indexCLASS = c("POSIXct", "POSIXt"),
          tclass = c("POSIXct", "POSIXt"),
          .indexTZ = "UTC", tzone = "UTC",
          class = c("xts", "zoo"))

info_msg <- "tclass(x) gets tclass attribute from index, not the xts object"
expect_identical(tclass(x), c("POSIXct", "POSIXt"), info = info_msg)

info_msg <- "indexClass(x) warns"
expect_warning(indexClass(x), info = info_msg)

info_msg <- "indexClass(x) <- 'Date'  warns"
expect_warning(indexClass(x) <- "Date", info = info_msg)

info_msg <- "tclass(x) <- 'POSIXct' removes tclass and .indexCLASS from xts object"
y <- x
tclass(y) <- "POSIXct"
expect_identical(NULL, attr(y, "tclass"), info = info_msg)
expect_identical(NULL, attr(y, ".indexCLASS"), info = info_msg)

info_msg <- "tclass<- sets tclass attribute on index"
y <- x
tclass(y) <- "Date"
expect_identical("Date", attr(attr(y, "index"), "tclass"), info = info_msg)

info_msg <- "tclass<- removes .indexCLASS attribute from xts object"
expect_identical("Date", attr(.index(y), "tclass"), info = info_msg)

info_msg <- "coredata(x) removes tclass and .indexCLASS from xts object"
y <- coredata(x)
expect_identical(NULL, attr(y, "tclass"), info = info_msg)
expect_identical(NULL, attr(y, ".indexCLASS"), info = info_msg)

info_msg <- "xtsAttributes(x) does not include tclass or .indexCLASS"
y <- xtsAttributes(x)
expect_identical(NULL, y$tclass, info = info_msg)
expect_identical(NULL, y$.indexCLASS, info = info_msg)

info_msg <- "xtsAttributes(x) <- 'foo' removes tclass and .indexCLASS"
y <- x
xtsAttributes(y) <- xtsAttributes(x)
expect_identical(NULL, attr(y, "tclass"), info = info_msg)
expect_identical(NULL, attr(y, ".indexCLASS"), info = info_msg)

info_msg <- "tclass(x) <- `foo` always creates a character tclass"
x <- "hello"
tclass(x) <- 1
expect_identical(storage.mode(attr(x, "tclass")), "character")

info_msg <- "zero-width subset has the same tclass as the input"
target <- "Imatclass"
x <- .xts(1:10, 1:10, tclass = target)
y <- x[,0]
expect_equal(target, tclass(y))

info_msg <- "tclass() on object with no tclass/.indexCLASS returns POSIXct"
x <- structure(1:5, .Dim = c(5L, 1L), index = 1:5, class = c("xts", "zoo"))
expect_warning(xtc <- tclass(x), "index does not have a 'tclass' attribute")
expect_identical(c("POSIXct", "POSIXt"), xtc)

info_msg <- "tclass<-() updates index"
x <- xts(1, .POSIXct(14400, tz = "Europe/Berlin"))
tclass(x) <- "Date"
expect_identical(as.numeric(.index(x)), 0, info = paste(info_msg, "values"))
expect_identical(tzone(x), "UTC", info = paste(info_msg, "timezone"))
