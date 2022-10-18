library(xts)

# Tests for xts constructors

### NA in order.by {{{

# .xts()
expect_error(.xts(1:3, c(1L, 2L, NA)), info = ".xts() order.by ends with NA_integer_")
expect_error(.xts(1:3, c(NA, 2L, 3L)), info = ".xts() order.by starts with NA_integer_")
expect_error(.xts(1:3, c(1L, NA, 3L)), info = ".xts() order.by contains NA_integer_")

expect_error(.xts(1:3, c(1, 2, NA)), info = ".xts() order.by ends with NA_real_")
expect_error(.xts(1:3, c(NA, 2, 3)), info = ".xts() order.by starts with NA_real_")
expect_error(.xts(1:3, c(1, NA, 3)), info = ".xts() order.by contains NA_real_")

expect_error(.xts(1:3, c(1, 2, NaN)), info = ".xts() order.by ends with NaN")
expect_error(.xts(1:3, c(NaN, 2, 3)), info = ".xts() order.by starts with NaN")
expect_error(.xts(1:3, c(1, NaN, 3)), info = ".xts() order.by contains NaN")

expect_error(.xts(1:3, c(1, 2,  Inf)), info = ".xts() order.by ends with Inf")
expect_error(.xts(1:3, c(-Inf, 2, 3)), info = ".xts() order.by starts with -Inf")

# xts()
expect_error(xts(1:3, as.Date(c(1L, 2L, NA), origin = "1970-01-01")), info = "xts() order.by ends with NA_integer_")
expect_error(xts(1:3, as.Date(c(NA, 2L, 3L), origin = "1970-01-01")), info = "xts() order.by starts with NA_integer_")
expect_error(xts(1:3, as.Date(c(1L, NA, 3L), origin = "1970-01-01")), info = "xts() order.by contains NA_integer_")

expect_error(xts(1:3, .POSIXct(c(1, 2, NA))), info = "xts() order.by ends with NA_real_")
expect_error(xts(1:3, .POSIXct(c(NA, 2, 3))), info = "xts() order.by starts with NA_real_")
expect_error(xts(1:3, .POSIXct(c(1, NA, 3))), info = "xts() order.by contains NA_real_")

expect_error(xts(1:3, .POSIXct(c(1, 2, NaN))), info = "xts() order.by ends with NaN")
expect_error(xts(1:3, .POSIXct(c(NaN, 2, 3))), info = "xts() order.by starts with NaN")
expect_error(xts(1:3, .POSIXct(c(1, NaN, 3))), info = "xts() order.by contains NaN")

expect_error(xts(1:3, .POSIXct(c(1, 2,  Inf))), info = "xts() order.by ends with Inf")
expect_error(xts(1:3, .POSIXct(c(-Inf, 2, 3))), info = "xts() order.by starts with -Inf")
### }}}

# Test that only first tzone element is stored
tz <- "America/Chicago"
i <- as.POSIXlt("2018-01-01", tz = tz)
y <- xts(1, i)
expect_identical(tz, tzone(y), info = "xts() only uses the first element of tzone")

### constructors add tzone and tclass to the index by default
x <- xts()
expect_true(!is.null(attr(attr(x, "index"), "tclass")), info = "xts() with no args adds tclass to the index")
expect_true(!is.null(attr(attr(x, "index"), "tzone")), info = "xts() with no args adds tzone to the index")

x <- .xts(, .POSIXct(integer()))
expect_true(!is.null(attr(attr(x, "index"), "tclass")), info = ".xts() with no args adds tclass to the index")
expect_true(!is.null(attr(attr(x, "index"), "tzone")), info = ".xts() with no args adds tzone to the index")


### constructor defaults don't add index attributes to the xts object
x <- xts(1, as.Date("2018-05-02"))
expect_null(attr(x, "tclass"), info = "xts(<defaults>) doesn't add tclass to xts object")
expect_null(attr(x, ".indexCLASS"), info = "xts(<defaults>) doesn't add .indexCLASS to xts object")
y <- .xts(1, 1)
expect_null(attr(y, "tclass"), info = ".xts(<defaults>) doesn't add .indexCLASS to xts object")
expect_null(attr(y, ".indexCLASS"), info = ".xts(<defaults>) doesn't add .indexCLASS to xts object")

x <- xts(1, as.Date("2018-05-02"))
expect_null(attr(x, "tzone"), info = "xts(<defaults>) doesn't add tzone to xts object")
expect_null(attr(x, ".indexTZ"), info = "xts(<defaults>) doesn't add .indexTZ to xts object")
y <- .xts(1, 1)
expect_null(attr(y, "tzone"), info = ".xts(<defaults>) doesn't add tzone to xts object")
expect_null(attr(y, ".indexTZ"), info = ".xts(<defaults>) doesn't add .indexTZ to xts object")

x <- xts(1, as.Date("2018-05-02"))
expect_null(attr(x, "tformat"), info = "xts(<defaults>) doesn't add tformat to xts object")
expect_null(attr(x, ".indexFORMAT"), info = "xts(<defaults>) doesn't add .indexFORMAT to xts object")
y <- .xts(1, 1)
expect_null(attr(y, "tformat"), info = ".xts(<defaults>) doesn't add tformat to xts object")
expect_null(attr(y, ".indexFORMAT"), info = ".xts(<defaults>) doesn't add .indexFORMAT to xts object")


### warn if deprecated arguments passed to constructor
deprecated_warns <-
  list(iclass  = "'.indexCLASS' is deprecated.*use tclass instead",
       izone   = "'.indexTZ' is deprecated.*use tzone instead",
       iformat = "'.indexFORMAT' is deprecated.*use tformat instead")

expect_warning(x <- xts(1, as.Date("2018-05-02"), .indexCLASS = "Date"),
               pattern = deprecated_warns$iclass,
               info = "xts() warns when .indexCLASS argument is provided")

expect_warning(x <- .xts(1, as.Date("2018-05-02"), .indexCLASS = "Date"),
               pattern = deprecated_warns$iclass,
               info = ".xts() warns when .indexCLASS argument is provided")


expect_warning(x <- xts(1, as.Date("2018-05-02"), .indexTZ = "UTC"),
               pattern = deprecated_warns$izone,
               info = "xts() warns when .indexTZ argument is provided")

expect_warning(x <- .xts(1, as.Date("2018-05-02"), .indexTZ = "UTC"),
               pattern = deprecated_warns$izone,
               info = ".xts() warns when .indexTZ argument is provided")


expect_warning(x <- xts(1, as.Date("2018-05-02"), .indexFORMAT = "%Y"),
               pattern = deprecated_warns$iformat,
               info = "xts() warns when .indexFORMAT is provided")

expect_warning(x <- .xts(1, as.Date("2018-05-02"), .indexFORMAT = "%Y"),
               pattern = deprecated_warns$iformat,
               info = ".xts() warns when .indexFORMAT is provided")


info_msg <- "test.xts_and.xts_ctors_add_tformat"
tf <- "%m/%d/%Y"
x <- xts(1:3, .Date(1:3), tformat = tf)
y <- .xts(1:3, .Date(1:3), tformat = tf)

expect_identical(tf, attr(attr(x, "index"), "tformat"),
                 info = "xts(..., tformat = 'foo') adds tformat to index")
expect_identical(tf, attr(attr(y, "index"), "tformat"),
                 info = ".xts(..., tformat = 'foo') adds tformat to index")

# .xts()
info_msg <- "test..xts_dimnames_in_dots"
x <- .xts(1:5, 1:5, dimnames = list(NULL, "x"))
y <- xts(1:5, index(x), dimnames = list(NULL, "x"))
expect_equal(x, y, info = info_msg)
expect_null(rownames(x), info = "xts() and .xts() apply dimnames passed via '...'")

m <- matrix(1, dimnames = list("a", "b"))
x <- .xts(m, 1)
expect_null(rownames(x), info = ".xts() result does not have rownames")

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
# silence the warning due to .indexFORMAT It is tested independently.
checkXtsFormat(suppressWarnings(.xts(1, 1, .indexFORMAT=fmt)), fmt)  # TODO: expect_warnings
checkXtsFormat(suppressWarnings(.xts(1, 1, tformat="%Y", .indexFORMAT=fmt)), fmt)  # TODO: expect_warnings

## check constructor arguments override existing index attribute
idx <- structure(1, tzone="", tclass="yearmon", tformat="%Y-%b")
fmt <- "%Y-%m"
checkXtsFormat(.xts(1, idx), "%Y-%b")
checkXtsFormat(.xts(1, idx, tformat=fmt), fmt)
# silence the warning due to .indexFORMAT It is tested independently.
checkXtsFormat(suppressWarnings(.xts(1, idx, .indexFORMAT=fmt)), fmt)  # TODO: expect_warnings
checkXtsFormat(suppressWarnings(.xts(1, idx, tformat="%b%y", .indexFORMAT=fmt)), fmt)  # TODO: expect_warnings

info_msg <- "test..xts_user_attributes"
suppressWarnings({
  x <- .xts(1, 1, tformat = "%Y", .indexCLASS = "Date", .indexTZ = "UTC",
            user = "attribute", hello = "world", dimnames = list(NULL, "x"))
})
expect_null(attr(x, "tformat"), info = info_msg)
expect_null(attr(x, "tclass"), info = info_msg)
expect_null(attr(x, "tzone"), info = info_msg)
expect_null(attr(x, ".indexCLASS"), info = info_msg)
expect_null(attr(x, ".indexTZ"), info = info_msg)
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
# silence the warning due to .indexCLASS. It is tested independently.
suppressWarnings(x <-.xts(1, 1, .indexCLASS="Date"))  # TODO: expect_warnings
checkXtsClass(x, "Date", info_msg)
# silence the warning due to .indexCLASS. It is tested independently.
suppressWarnings(x <- .xts(1, 1, tclass="timeDate", .indexCLASS="Date"))  # TODO: expect_warnings
checkXtsClass(x, "Date", info_msg)

## also check that tclass is ignored if specified as part of index
info_msg <- ".xts() tclass is ignored if it's an index attribute"
idx <- structure(1, tzone="",tclass="yearmon")
checkXtsClass(.xts(1, idx), c("POSIXct", "POSIXt"), info_msg)
checkXtsClass(.xts(1, idx, tclass="timeDate"), "timeDate", info_msg)
# silence .indexCLASS warning because it's tested independently.
suppressWarnings(x <-.xts(1, 1, .indexCLASS="Date"))  # TODO: expect_warnings
checkXtsClass(x, "Date", info_msg)
# silence the warning due to .indexCLASS. It is tested independently.
suppressWarnings(x <- .xts(1, idx, tclass="timeDate", .indexCLASS="Date"))  # TODO: expect_warnings
checkXtsClass(x, "Date", info_msg)

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
suppressWarnings(x <- .xts(1, 1, .indexTZ="America/New_York"))  # TODO: expect_warning
checkXtsTz(x, "UTC", info_msg)
suppressWarnings(x <- .xts(1, 1, tzone="Europe/London", .indexTZ="America/New_York"))  # TODO: expect_warning
checkXtsTz(x, "Europe/London", info_msg)

## Cases where tzone is specified in the index
info_msg <- ".xts() index tzone precedence - tzone is an index attribute"
idx <- structure(1, tzone="Asia/Tokyo",tclass="yearmon")
checkXtsTz(.xts(1, idx), "Asia/Tokyo", info_msg)
checkXtsTz(.xts(1, idx, tzone="Europe/London"), "Europe/London", info_msg)
suppressWarnings(x <- .xts(1, idx, .indexTZ="America/New_York"))  # TODO: expect_warnings
checkXtsTz(x, "Asia/Tokyo", info_msg)
suppressWarnings(x <- .xts(1, idx, tzone="Europe/London", .indexTZ="America/New_York"))  # TODO: expect_warnings
checkXtsTz(x, "Europe/London", info_msg)

Sys.setenv(TZ = sysTZ)
