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

test.get_indexClass_warns <- function() {
  op <- options(warn = 2)
  on.exit(options(warn = op$warn))
  checkException(indexClass(x))
}

test.set_indexClass_warns <- function() {
  op <- options(warn = 2)
  on.exit(options(warn = op$warn))
  checkException(indexClass(x) <- "Date")
}

test.set_tclass_drops_xts_tclass_indexCLASS <- function() {
  y <- x
  tclass(y) <- "POSIXct"
  checkIdentical(NULL, attr(y, "tclass"))
  checkIdentical(NULL, attr(y, ".indexCLASS"))
}

test.set_tclass_changes_index_tclass <- function() {
  y <- x
  tclass(y) <- "Date"
  checkIdentical("Date", attr(attr(y, "index"), "tclass"))
}

test.ctors_dont_add_tclass_indexCLASS_to_object <- function() {
  x <- xts(1, as.Date("2018-05-02"))
  checkIdentical(NULL, attr(x, "tclass"))
  checkIdentical(NULL, attr(x, ".indexCLASS"))
  y <- .xts(1, 1)
  checkIdentical(NULL, attr(y, "tclass"))
  checkIdentical(NULL, attr(y, ".indexCLASS"))
}

test.get_coredata_drops_xts_tclass_indexCLASS <- function() {
  y <- coredata(x)
  checkIdentical(NULL, attr(y, "tclass"))
  checkIdentical(NULL, attr(y, ".indexCLASS"))
}

test.get_xtsAttributes_excludes_tclass_indexCLASS <- function() {
  y <- xtsAttributes(x)
  checkIdentical(NULL, y$tclass)
  checkIdentical(NULL, y$.indexCLASS)
}

test.set_xtsAttributes_removes_tclass_indexClass <- function() {
  y <- x
  xtsAttributes(y) <- xtsAttributes(x)
  checkIdentical(NULL, attr(y, "tclass"))
  checkIdentical(NULL, attr(y, ".indexCLASS"))
}
