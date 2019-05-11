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

test.get_tformat <- function() {
  checkIdentical(tformat(x), "%Y-%m-%d")
}

test.get_indexFORMAT_warns <- function() {
  op <- options(warn = 2)
  on.exit(options(warn = op$warn))
  checkException(indexFormat(x))
}

test.set_indexFORMAT_warns <- function() {
  op <- options(warn = 2)
  on.exit(options(warn = op$warn))
  checkException(indexFormat(x) <- "GMT")
}

test.set_tformat_drops_xts_indexFORMAT <- function() {
  y <- x
  tformat(y) <- "%Y-%m-%d %H:%M"
  checkIdentical(NULL, attr(y, ".indexFORMAT"))
}

test.set_tformat_changes_index_tformat <- function() {
  y <- x
  fmt <- "%Y-%m-%d %H:%M"
  tformat(y) <- fmt
  checkIdentical(fmt, attr(attr(y, "index"), "tformat"))
}

test.get_coredata_drops_xts_indexFORMAT <- function() {
  y <- coredata(x)
  checkIdentical(NULL, attr(y, ".indexFORMAT"))
}

test.get_xtsAttributes_excludes_indexFORMAT <- function() {
  y <- xtsAttributes(x)
  checkIdentical(NULL, y$.indexFORMAT)
}

test.set_xtsAttributes_removes_indexFORMAT <- function() {
  y <- x
  xtsAttributes(y) <- xtsAttributes(x)
  checkIdentical(NULL, attr(y, ".indexFORMAT"))
}
