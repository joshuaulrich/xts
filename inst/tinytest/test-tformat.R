# These tests check the 'tformat' attribute is attached to the expected
# component of the xts object. The xts constructors should no longer add the
# '.indexFORMAT' attribute to the xts object itself. Only the index should
# have a 'tformat' attribute. Construct xts objects using structure() to
# test behavior when functions encounter xts objects created before 0.10-3.
x <-
structure(1:5, .Dim = c(5L, 1L),
          index = structure(1:5, tzone = "",
                                 tclass = c("POSIXct", "POSIXt"),
                                 tformat = "%Y-%m-%d"),
          .indexCLASS = c("POSIXct", "POSIXt"),
          tclass = c("POSIXct", "POSIXt"),
          .indexTZ = "UTC", tzone = "UTC",
          .indexFORMAT = "%Y-%m-%d %H:%M:%S",
          class = c("xts", "zoo"))

info_msg <- "test.get_tformat"
expect_identical(tformat(x), "%Y-%m-%d", info = info_msg)

info_msg <- "test.get_indexFORMAT_warns"
expect_warning(indexFormat(x), info = info_msg)

info_msg <- "test.set_indexFORMAT_warns"
expect_warning(indexFormat(x) <- "GMT", info = info_msg)

info_msg <- "test.set_tformat_drops_xts_indexFORMAT"
y <- x
tformat(y) <- "%Y-%m-%d %H:%M"
expect_identical(NULL, attr(y, ".indexFORMAT"), info = info_msg)

info_msg <- "test.set_tformat_changes_index_tformat"
y <- x
fmt <- "%Y-%m-%d %H:%M"
tformat(y) <- fmt
expect_identical(fmt, attr(attr(y, "index"), "tformat"), info = info_msg)

info_msg <- "test.get_coredata_drops_xts_indexFORMAT"
y <- coredata(x)
expect_identical(NULL, attr(y, ".indexFORMAT"), info = info_msg)

info_msg <- "test.get_xtsAttributes_excludes_indexFORMAT"
y <- xtsAttributes(x)
expect_identical(NULL, y$.indexFORMAT, info = info_msg)

info_msg <- "test.set_xtsAttributes_removes_indexFORMAT"
y <- x
xtsAttributes(y) <- xtsAttributes(x)
expect_identical(NULL, attr(y, ".indexFORMAT"), info = info_msg)
