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

info_msg <- "test.get_tclass"
expect_identical(tclass(x), c("POSIXct", "POSIXt"), info = info_msg)

info_msg <- "test.get_indexClass_warns"
expect_warning(indexClass(x), info = info_msg)

info_msg <- "test.set_indexClass_warns"
expect_warning(indexClass(x) <- "Date", info = info_msg)

info_msg <- "test.set_tclass_drops_xts_tclass_indexCLASS"
y <- x
tclass(y) <- "POSIXct"
expect_identical(NULL, attr(y, "tclass"), info = info_msg)
expect_identical(NULL, attr(y, ".indexCLASS"), info = info_msg)

info_msg <- "test.set_tclass_changes_index_tclass"
y <- x
tclass(y) <- "Date"
expect_identical("Date", attr(attr(y, "index"), "tclass"), info = info_msg)

info_msg <- "test.get_coredata_drops_xts_tclass_indexCLASS"
y <- coredata(x)
expect_identical(NULL, attr(y, "tclass"), info = info_msg)
expect_identical(NULL, attr(y, ".indexCLASS"), info = info_msg)

info_msg <- "test.get_xtsAttributes_excludes_tclass_indexCLASS"
y <- xtsAttributes(x)
expect_identical(NULL, y$tclass, info = info_msg)
expect_identical(NULL, y$.indexCLASS, info = info_msg)

info_msg <- "test.set_xtsAttributes_removes_tclass_indexClass"
y <- x
xtsAttributes(y) <- xtsAttributes(x)
expect_identical(NULL, attr(y, "tclass"), info = info_msg)
expect_identical(NULL, attr(y, ".indexCLASS"), info = info_msg)

info_msg <- "test.set_tclass_default_always_character"
x <- "hello"
tclass(x) <- 1
expect_identical(storage.mode(attr(x, "tclass")), "character")

info_msg <- "test.tclass_matches_input_for_zero_width_subset"
target <- "Imatclass"
x <- .xts(1:10, 1:10, tclass = target)
y <- x[,0]
expect_equal(target, tclass(y))
