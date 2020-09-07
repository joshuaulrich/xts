# Tests for xts constructors
#

### NA in order.by {{{
# .xts()
test..xts_order.by_NA_integer <- function() {
  checkException(.xts(1:3, c(1L, 2L, NA)))
  checkException(.xts(1:3, c(NA, 2L, 3L)))
  checkException(.xts(1:3, c(1L, NA, 3L)))
}
test..xts_order.by_NA_double <- function() {
  checkException(.xts(1:3, c(1, 2, NA)))
  checkException(.xts(1:3, c(NA, 2, 3)))
  checkException(.xts(1:3, c(1, NA, 3)))
}
test..xts_order.by_NaN_double <- function() {
  checkException(.xts(1:3, c(1, 2, NaN)))
  checkException(.xts(1:3, c(NaN, 2, 3)))
  checkException(.xts(1:3, c(1, NaN, 3)))
}
test..xts_order.by_Inf_double <- function() {
  checkException(.xts(1:3, c(1, 2,  Inf)))
  checkException(.xts(1:3, c(-Inf, 2, 3)))
}
# xts()
test.xts_order.by_NA_integer <- function() {
  checkException(xts(1:3, as.Date(c(1L, 2L, NA), origin = "1970-01-01")))
  checkException(xts(1:3, as.Date(c(NA, 2L, 3L), origin = "1970-01-01")))
  checkException(xts(1:3, as.Date(c(1L, NA, 3L), origin = "1970-01-01")))
}
test.xts_order.by_NA_double <- function() {
  checkException(xts(1:3, .POSIXct(c(1, 2, NA))))
  checkException(xts(1:3, .POSIXct(c(NA, 2, 3))))
  checkException(xts(1:3, .POSIXct(c(1, NA, 3))))
}
test.xts_order.by_NaN_double <- function() {
  checkException(xts(1:3, .POSIXct(c(1, 2, NaN))))
  checkException(xts(1:3, .POSIXct(c(NaN, 2, 3))))
  checkException(xts(1:3, .POSIXct(c(1, NaN, 3))))
}
test.xts_order.by_Inf_double <- function() {
  checkException(xts(1:3, .POSIXct(c(1, 2,  Inf))))
  checkException(xts(1:3, .POSIXct(c(-Inf, 2, 3))))
}
### }}}

# Test that only first tzone element is stored
test.xts_only_use_first_tzone_element <- function() {
  tz <- "America/Chicago"
  i <- as.POSIXlt("2018-01-01", tz = tz)
  y <- xts(1, i)
  checkIdentical(tz, tzone(y))
}

test.xts_no_args_has_index_with_tzone_tclass <- function() {
  x <- xts()
  checkTrue(!is.null(attr(.index(x), "tclass")))
  checkTrue(!is.null(attr(.index(x), "tzone")))
}

# don't add index attributes to xts object
test.ctors_dont_add_tclass_indexCLASS_to_object <- function() {
  x <- xts(1, as.Date("2018-05-02"))
  checkIdentical(NULL, attr(x, "tclass"))
  checkIdentical(NULL, attr(x, ".indexCLASS"))
  y <- .xts(1, 1)
  checkIdentical(NULL, attr(y, "tclass"))
  checkIdentical(NULL, attr(y, ".indexCLASS"))
}

test.ctors_dont_add_tzone_indexTZ_to_object <- function() {
  x <- xts(1, as.Date("2018-05-02"))
  checkIdentical(NULL, attr(x, "tzone"))
  checkIdentical(NULL, attr(x, ".indexTZ"))
  y <- .xts(1, 1)
  checkIdentical(NULL, attr(y, "tzone"))
  checkIdentical(NULL, attr(y, ".indexTZ"))
}

test.ctors_dont_add_indexFORMAT_to_object <- function() {
  x <- xts(1, as.Date("2018-05-02"))
  checkIdentical(NULL, attr(x, ".indexFORMAT"))
  y <- .xts(1, 1)
  checkIdentical(NULL, attr(y, ".indexFORMAT"))
}

# warn if deprecated arguments passed to constructor
test.xts_ctor_warns_for_indexCLASS_arg <- function() {
  op <- options(warn = 2)
  on.exit(options(warn = op$warn))
  checkException(x <- xts(1, as.Date("2018-05-02"), .indexCLASS = "Date"))
  checkException(x <- .xts(1, as.Date("2018-05-02"), .indexCLASS = "Date"))
}

test.xts_ctor_warns_for_indexTZ_arg <- function() {
  op <- options(warn = 2)
  on.exit(options(warn = op$warn))
  checkException(x <- xts(1, as.Date("2018-05-02"), .indexTZ = "UTC"))
  checkException(x <- .xts(1, as.Date("2018-05-02"), .indexTZ = "UTC"))
}

test.xts_ctor_warns_for_indexFORMAT_arg <- function() {
  op <- options(warn = 2)
  on.exit(options(warn = op$warn))
  checkException(x <- xts(1, as.Date("2018-05-02"), .indexFORMAT = "%Y"))
  checkException(x <- .xts(1, as.Date("2018-05-02"), .indexFORMAT = "%Y"))
}

# .xts()
test..xts_dimnames_in_dots <- function() {
  x <- .xts(1:5, 1:5, dimnames = list(NULL, "x"))
  y <- xts(1:5, index(x), dimnames = list(NULL, "x"))
  checkEquals(x, y)
}

test..xts_ctor_does_not_return_rownames <- function() {
  m <- matrix(1, dimnames = list("a", "b"))
  x <- .xts(m, 1)
  checkEquals(rownames(x), NULL)
}

# test..xts_ctor_warns_if_index_tclass_not_NULL_or_POSIXct <- function() {
#   DEACTIVATED("Warning causes errors in dependencies")
#   op <- options(warn = 2)
#   on.exit(options(warn = op$warn))
#
#   idx <- 1:3
#   x <- .xts(1:3, idx)  # no error, NULL
#   idx <- .POSIXct(idx)
#   x <- .xts(1:3, idx)  # no error, POSIXct
#
#   idx <- structure(1:3, tclass = "Date", tzone = "UTC")
#   checkException(.xts(1:3, idx), msg = "tclass = Date")
#   idx <- structure(idx, tclass = "yearmon", tzone = "UTC")
#   checkException(.xts(1:3, idx), msg = "tclass = yearmon")
#   idx <- structure(idx, tclass = "timeDate", tzone = "UTC")
#   checkException(.xts(1:3, idx), msg = "tclass = timeDate")
# }

checkXtsFormat <- function(xts, format) {
  checkIdentical(tformat(xts), format)
  checkIdentical(attr(attr(xts, "index"), "tformat"), format)
}

### Check that index format attribute precedence is:
### .indexFORMAT argument > tformat argument > tformat index attribute
test..xts_index_format_precedence <- function() {
  fmt <- "%Y-%m-%d"
  checkXtsFormat(.xts(1, 1), NULL)
  checkXtsFormat(.xts(1, 1, tformat=fmt), fmt)
  checkXtsFormat(.xts(1, 1, .indexFORMAT=fmt), fmt)
  checkXtsFormat(.xts(1, 1, tformat="%Y", .indexFORMAT=fmt), fmt)

  ## check constructor arguments override existing index attribute
  idx <- structure(1, tzone="", tclass="yearmon", tformat="%Y-%b")
  fmt <- "%Y-%m"
  checkXtsFormat(.xts(1, idx), "%Y-%b")
  checkXtsFormat(.xts(1, idx, tformat=fmt), fmt)
  checkXtsFormat(.xts(1, idx, .indexFORMAT=fmt), fmt)
  checkXtsFormat(.xts(1, idx, tformat="%b%y", .indexFORMAT=fmt), fmt)
}

test..xts_user_attributes <- function() {
  x <- .xts(1, 1, tformat = "%Y", .indexCLASS = "Date", .indexTZ = "UTC",
            user = "attribute", hello = "world", dimnames = list(NULL, "x"))
  checkIdentical(NULL, attr(x, "tformat"))
  checkIdentical(NULL, attr(x, "tclass"))
  checkIdentical(NULL, attr(x, "tzone"))
  checkIdentical(NULL, attr(x, ".indexCLASS"))
  checkIdentical(NULL, attr(x, ".indexTZ"))
  checkIdentical("attribute", attr(x, "user"))
  checkIdentical("world", attr(x, "hello"))
  checkIdentical("x", colnames(x))
}

checkXtsClass <- function(xts, class) {
  checkEquals(tclass(xts), class)
  checkEquals(attr(attr(xts, "index"), "tclass"), class)
}

### Check that index class attribute precedence is:
### .indexCLASS argument > tclass argument > tclass index attribute
test..xts_index_class_precedence <- function() {
  checkXtsClass(.xts(1, 1), c("POSIXct", "POSIXt"))
  checkXtsClass(.xts(1, 1, tclass="timeDate"), "timeDate")
  checkXtsClass(.xts(1, 1, .indexCLASS="Date"), "Date")
  checkXtsClass(.xts(1, 1, tclass="timeDate", .indexCLASS="Date"), "Date")

  ## also check that tclass is ignored if specified as part of index
  idx <- structure(1, tzone="",tclass="yearmon")
  checkXtsClass(.xts(1, idx), c("POSIXct", "POSIXt"))
  checkXtsClass(.xts(1, idx, tclass="timeDate"), "timeDate")
  checkXtsClass(.xts(1, idx, .indexCLASS="Date"), "Date")
  checkXtsClass(.xts(1, idx, tclass="timeDate", .indexCLASS="Date"), "Date")
}

checkXtsTz <- function(xts, tzone) {
  checkEquals(tzone(xts), tzone)
  checkEquals(attr(attr(xts, "index"), "tzone"), tzone)
}

### Check that tzone is honoured and .indexTZ ignored
### Check that index timezone attribute precedence is:
### .indexTZ argument > tzone argument > tzone index attribute
### tzone argument > tzone argument > tzone index attribute
test..xts_index_tzone_precedence <- function() {
  sysTZ <- Sys.getenv("TZ")
  Sys.setenv(TZ = "UTC")
  on.exit(Sys.setenv(TZ = sysTZ), add = TRUE)

  checkXtsTz(.xts(1, 1), "UTC")
  checkXtsTz(.xts(1, 1, tzone="Europe/London"), "Europe/London")
  ## this case passes in 0.10-2 but looks wrong
  checkXtsTz(.xts(1, 1, .indexTZ="America/New_York"), "UTC")
  checkXtsTz(.xts(1, 1, tzone="Europe/London", .indexTZ="America/New_York"), "Europe/London")

  ## Cases where tzone is specified in the index
  idx <- structure(1, tzone="Asia/Tokyo",tclass="yearmon")
  checkXtsTz(.xts(1, idx), "Asia/Tokyo")
  checkXtsTz(.xts(1, idx, tzone="Europe/London"), "Europe/London")
  checkXtsTz(.xts(1, idx, .indexTZ="America/New_York"), "Asia/Tokyo")
  checkXtsTz(.xts(1, idx, tzone="Europe/London", .indexTZ="America/New_York"), "Europe/London")
}
