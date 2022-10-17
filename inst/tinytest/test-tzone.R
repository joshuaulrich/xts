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

info_msg <- "test.get_indexTZ_warns"
expect_warning(indexTZ(x))

info_msg <- "test.set_indexTZ_warns"
expect_warning(indexTZ(x) <- "GMT")

info_msg <- "test.set_tzone_drops_xts_tzone_indexTZ"
y <- x
tzone(y) <- "GMT"
expect_identical(NULL, attr(y, "tzone"), info = info_msg)
expect_identical(NULL, attr(y, ".indexTZ"), info = info_msg)

info_msg <- "test.set_tzone_changes_index_tzone"
y <- x
tzone(y) <- "GMT"
expect_identical("GMT", attr(attr(y, "index"), "tzone"), info = info_msg)

info_msg <- "test.set_tzone_to_NULL_sets_empty_string"
y <- x
tzone(y) <- NULL
expect_identical("", attr(attr(y, "index"), "tzone"), info = info_msg)

info_msg <- "test.get_coredata_drops_xts_tzone_indexTZ"
y <- coredata(x)
expect_identical(NULL, attr(y, "tzone"), info = info_msg)
expect_identical(NULL, attr(y, ".indexTZ"), info = info_msg)

info_msg <- "test.get_xtsAttributes_excludes_tzone_indexTZ"
y <- xtsAttributes(x)
expect_identical(NULL, y$tzone, info = info_msg)
expect_identical(NULL, y$.indexTZ, info = info_msg)

info_msg <- "test.set_xtsAttributes_removes_tzone_indexTZ"
y <- x
xtsAttributes(y) <- xtsAttributes(x)
expect_identical(NULL, attr(y, "tzone"), info = info_msg)
expect_identical(NULL, attr(y, ".indexTZ"), info = info_msg)

info_msg <- "test.set_tzone_default_always_character"
x <- "hello"
tzone(x) <- 1
expect_identical(storage.mode(attr(x, "tzone")), "character", info = info_msg)

info_msg <- "test.tzone_matches_input_for_zero_width_subset"
target <- "Ima/Tzone"
x <- .xts(1:10, 1:10, tzone = target)
y <- x[,0]
expect_equal(target, tzone(y), info = info_msg)
