# Tests for xts constructors
#

### NA in order.by {{{

# .xts()
info_msg <- "test..xts_order.by_NA_integer"
expect_error(.xts(1:3, c(1L, 2L, NA)), info = info_msg)
expect_error(.xts(1:3, c(NA, 2L, 3L)), info = info_msg)
expect_error(.xts(1:3, c(1L, NA, 3L)), info = info_msg)

info_msg <- "test..xts_order.by_NA_double"
expect_error(.xts(1:3, c(1, 2, NA)), info = info_msg)
expect_error(.xts(1:3, c(NA, 2, 3)), info = info_msg)
expect_error(.xts(1:3, c(1, NA, 3)), info = info_msg)

info_msg <- "test..xts_order.by_NaN_double"
expect_error(.xts(1:3, c(1, 2, NaN)), info = info_msg)
expect_error(.xts(1:3, c(NaN, 2, 3)), info = info_msg)
expect_error(.xts(1:3, c(1, NaN, 3)), info = info_msg)

info_msg <- "test..xts_order.by_Inf_double"
expect_error(.xts(1:3, c(1, 2,  Inf)), info = info_msg)
expect_error(.xts(1:3, c(-Inf, 2, 3)), info = info_msg)

# xts()
info_msg <- "test.xts_order.by_NA_integer"
expect_error(xts(1:3, as.Date(c(1L, 2L, NA), origin = "1970-01-01")), info = info_msg)
expect_error(xts(1:3, as.Date(c(NA, 2L, 3L), origin = "1970-01-01")), info = info_msg)
expect_error(xts(1:3, as.Date(c(1L, NA, 3L), origin = "1970-01-01")), info = info_msg)

info_msg <- "test.xts_order.by_NA_double"
expect_error(xts(1:3, .POSIXct(c(1, 2, NA))), info = info_msg)
expect_error(xts(1:3, .POSIXct(c(NA, 2, 3))), info = info_msg)
expect_error(xts(1:3, .POSIXct(c(1, NA, 3))), info = info_msg)

info_msg <- "test.xts_order.by_NaN_double"
expect_error(xts(1:3, .POSIXct(c(1, 2, NaN))), info = info_msg)
expect_error(xts(1:3, .POSIXct(c(NaN, 2, 3))), info = info_msg)
expect_error(xts(1:3, .POSIXct(c(1, NaN, 3))), info = info_msg)

info_msg <- "test.xts_order.by_Inf_double"
expect_error(xts(1:3, .POSIXct(c(1, 2,  Inf))), info = info_msg)
expect_error(xts(1:3, .POSIXct(c(-Inf, 2, 3))), info = info_msg)
### }}}

# Test that only first tzone element is stored
info_msg <- "test.xts_only_use_first_tzone_element"
tz <- "America/Chicago"
i <- as.POSIXlt("2018-01-01", tz = tz)
y <- xts(1, i)
expect_identical(tz, tzone(y), info = info_msg)

info_msg <- "test.xts_no_args_has_index_with_tzone_tclass"
x <- xts()
expect_true(!is.null(attr(.index(x), "tclass")), info = info_msg)
expect_true(!is.null(attr(.index(x), "tzone")), info = info_msg)

# don't add index attributes to xts object
info_msg <- "test.ctors_dont_add_tclass_indexCLASS_to_object"
x <- xts(1, as.Date("2018-05-02"))
expect_identical(NULL, attr(x, "tclass"), info = info_msg)
expect_identical(NULL, attr(x, ".indexCLASS"), info = info_msg)
y <- .xts(1, 1)
expect_identical(NULL, attr(y, "tclass"), info = info_msg)
expect_identical(NULL, attr(y, ".indexCLASS"), info = info_msg)

info_msg <- "test.ctors_dont_add_tzone_indexTZ_to_object"
x <- xts(1, as.Date("2018-05-02"))
expect_identical(NULL, attr(x, "tzone"), info = info_msg)
expect_identical(NULL, attr(x, ".indexTZ"), info = info_msg)
y <- .xts(1, 1)
expect_identical(NULL, attr(y, "tzone"), info = info_msg)
expect_identical(NULL, attr(y, ".indexTZ"), info = info_msg)

info_msg <- "test.ctors_dont_add_indexFORMAT_to_object"
x <- xts(1, as.Date("2018-05-02"))
expect_identical(NULL, attr(x, ".indexFORMAT"), info = info_msg)
y <- .xts(1, 1)
expect_identical(NULL, attr(y, ".indexFORMAT"), info = info_msg)

# warn if deprecated arguments passed to constructor
info_msg <- "test.xts_ctor_warns_for_indexCLASS_arg"
expect_warning(x <- xts(1, as.Date("2018-05-02"), .indexCLASS = "Date"), info = info_msg)
expect_warning(x <- .xts(1, as.Date("2018-05-02"), .indexCLASS = "Date"), info = info_msg)

info_msg <- "test.xts_ctor_warns_for_indexTZ_arg"
expect_warning(x <- xts(1, as.Date("2018-05-02"), .indexTZ = "UTC"), info = info_msg)
expect_warning(x <- .xts(1, as.Date("2018-05-02"), .indexTZ = "UTC"), info = info_msg)

info_msg <- "test.xts_ctor_warns_for_indexFORMAT_arg"
expect_warning(x <- xts(1, as.Date("2018-05-02"), .indexFORMAT = "%Y"), info = info_msg)
expect_warning(x <- .xts(1, as.Date("2018-05-02"), .indexFORMAT = "%Y"), info = info_msg)

info_msg <- "test.xts_and.xts_ctors_add_tformat"
tf <- "%m/%d/%Y"
x <- xts(1:3, .Date(1:3), tformat = tf)
y <- .xts(1:3, .Date(1:3), tformat = tf)

expect_identical(tf, tformat(x), info = info_msg)
expect_identical(tf, tformat(y), info = info_msg)

# .xts()
info_msg <- "test..xts_dimnames_in_dots"
x <- .xts(1:5, 1:5, dimnames = list(NULL, "x"))
y <- xts(1:5, index(x), dimnames = list(NULL, "x"))
expect_equal(x, y, info = info_msg)

info_msg <- "test..xts_ctor_does_not_return_rownames"
m <- matrix(1, dimnames = list("a", "b"))
x <- .xts(m, 1)
expect_equal(rownames(x), NULL, info = info_msg)

# test..xts_ctor_warns_if_index_tclass_not_NULL_or_POSIXct <- function() {
#   DEACTIVATED("Warning causes errors in dependencies")
#
#   idx <- 1:3
#   x <- .xts(1:3, idx)  # no error, NULL
#   idx <- .POSIXct(idx)
#   x <- .xts(1:3, idx)  # no error, POSIXct
#
#   idx <- structure(1:3, tclass = "Date", tzone = "UTC")
#   expect_warning(.xts(1:3, idx), msg = "tclass = Date")
#   idx <- structure(idx, tclass = "yearmon", tzone = "UTC")
#   expect_warning(.xts(1:3, idx), msg = "tclass = yearmon")
#   idx <- structure(idx, tclass = "timeDate", tzone = "UTC")
#   expect_warning(.xts(1:3, idx), msg = "tclass = timeDate")
# }

checkXtsFormat <- function(xts, format) {
  expect_identical(tformat(xts), format, info = info_msg)
  expect_identical(attr(attr(xts, "index"), "tformat"), format, info = info_msg)
}

### Check that index format attribute precedence is:
### .indexFORMAT argument > tformat argument > tformat index attribute
info_msg <- "test..xts_index_format_precedence"
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

info_msg <- "test..xts_user_attributes"
x <- .xts(1, 1, tformat = "%Y", .indexCLASS = "Date", .indexTZ = "UTC",
          user = "attribute", hello = "world", dimnames = list(NULL, "x"))
expect_identical(NULL, attr(x, "tformat"), info = info_msg)
expect_identical(NULL, attr(x, "tclass"), info = info_msg)
expect_identical(NULL, attr(x, "tzone"), info = info_msg)
expect_identical(NULL, attr(x, ".indexCLASS"), info = info_msg)
expect_identical(NULL, attr(x, ".indexTZ"), info = info_msg)
expect_identical("attribute", attr(x, "user"), info = info_msg)
expect_identical("world", attr(x, "hello"), info = info_msg)
expect_identical("x", colnames(x), info = info_msg)

checkXtsClass <- function(xts, class, msg) {
  expect_equal(tclass(xts), class, info = msg)
  expect_equal(attr(attr(xts, "index"), "tclass"), class, info = msg)
}

### Check that index class attribute precedence is:
### .indexCLASS argument > tclass argument > tclass index attribute
info_msg <- ".xts() index class precedence"
checkXtsClass(.xts(1, 1), c("POSIXct", "POSIXt"), info_msg)
checkXtsClass(.xts(1, 1, tclass="timeDate"), "timeDate", info_msg)
checkXtsClass(.xts(1, 1, .indexCLASS="Date"), "Date", info_msg)
checkXtsClass(.xts(1, 1, tclass="timeDate", .indexCLASS="Date"), "Date", info_msg)

## also check that tclass is ignored if specified as part of index
info_msg <- ".xts() tclass is ignored if it's an index attribute"
idx <- structure(1, tzone="",tclass="yearmon")
checkXtsClass(.xts(1, idx), c("POSIXct", "POSIXt"), info_msg)
checkXtsClass(.xts(1, idx, tclass="timeDate"), "timeDate", info_msg)
checkXtsClass(.xts(1, idx, .indexCLASS="Date"), "Date", info_msg)
checkXtsClass(.xts(1, idx, tclass="timeDate", .indexCLASS="Date"), "Date", info_msg)

checkXtsTz <- function(xts, tzone, msg) {
  expect_equal(tzone(xts), tzone, info = msg)
  expect_equal(attr(attr(xts, "index"), "tzone"), tzone, info = msg)
}

### Check that tzone is honoured and .indexTZ ignored
### Check that index timezone attribute precedence is:
### .indexTZ argument > tzone argument > tzone index attribute
### tzone argument > tzone argument > tzone index attribute
info_msg <- ".xts() index tzone precedence"
sysTZ <- Sys.getenv("TZ")
Sys.setenv(TZ = "UTC")

checkXtsTz(.xts(1, 1), "UTC", info_msg)
checkXtsTz(.xts(1, 1, tzone="Europe/London"), "Europe/London", info_msg)
## this case passes in 0.10-2 but looks wrong
checkXtsTz(.xts(1, 1, .indexTZ="America/New_York"), "UTC", info_msg)
checkXtsTz(.xts(1, 1, tzone="Europe/London", .indexTZ="America/New_York"), "Europe/London", info_msg)

## Cases where tzone is specified in the index
info_msg <- ".xts() index tzone precedence - tzone is an index attribute"
idx <- structure(1, tzone="Asia/Tokyo",tclass="yearmon")
checkXtsTz(.xts(1, idx), "Asia/Tokyo", info_msg)
checkXtsTz(.xts(1, idx, tzone="Europe/London"), "Europe/London", info_msg)
checkXtsTz(.xts(1, idx, .indexTZ="America/New_York"), "Asia/Tokyo", info_msg)
checkXtsTz(.xts(1, idx, tzone="Europe/London", .indexTZ="America/New_York"), "Europe/London", info_msg)

Sys.setenv(TZ = sysTZ)
