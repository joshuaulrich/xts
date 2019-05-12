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

test.get_tzone <- function() {
  checkIdentical(tzone(x), "")
}

test.get_indexTZ_warns <- function() {
  op <- options(warn = 2)
  on.exit(options(warn = op$warn))
  checkException(indexTZ(x))
}

test.set_indexTZ_warns <- function() {
  op <- options(warn = 2)
  on.exit(options(warn = op$warn))
  checkException(indexTZ(x) <- "GMT")
}

test.set_tzone_drops_xts_tzone_indexTZ <- function() {
  y <- x
  tzone(y) <- "GMT"
  checkIdentical(NULL, attr(y, "tzone"))
  checkIdentical(NULL, attr(y, ".indexTZ"))
}

test.set_tzone_changes_index_tzone <- function() {
  y <- x
  tzone(y) <- "GMT"
  checkIdentical("GMT", attr(attr(y, "index"), "tzone"))
}

test.set_tzone_to_NULL_sets_empty_string <- function() {
  y <- x
  tzone(y) <- NULL
  checkIdentical("", attr(attr(y, "index"), "tzone"))
}

test.get_coredata_drops_xts_tzone_indexTZ <- function() {
  y <- coredata(x)
  checkIdentical(NULL, attr(y, "tzone"))
  checkIdentical(NULL, attr(y, ".indexTZ"))
}

test.get_xtsAttributes_excludes_tzone_indexTZ <- function() {
  y <- xtsAttributes(x)
  checkIdentical(NULL, y$tzone)
  checkIdentical(NULL, y$.indexTZ)
}

test.set_xtsAttributes_removes_tzone_indexTZ <- function() {
  y <- x
  xtsAttributes(y) <- xtsAttributes(x)
  checkIdentical(NULL, attr(y, "tzone"))
  checkIdentical(NULL, attr(y, ".indexTZ"))
}
