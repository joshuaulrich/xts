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

# Test that only first tzone element is stored for POSIXlt
tz <- "America/Chicago"
i <- as.POSIXlt("2018-01-01", tz = tz)
y <- xts(1, i)
expect_identical(tz, tzone(y), info = "xts() only uses the first element of tzone for POSIXlt order.by")

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

### constructor with index attributes specified doesn't add them to the xts object
create_msg <- function(func, attrib) {
  paste0(func, "(..., ",
         attrib, " = 'foo' doesn't add ",
         attrib, " to the xts object")
}
suppressWarnings({
    x <- xts(1, Sys.time(), .indexCLASS = "yearmon")
    y <- xts(1, Sys.time(), .indexFORMAT = "%Y")
    z <- xts(1, Sys.time(), .indexTZ = "UTC")
})
expect_null(attr(x, ".indexCLASS"), info = create_msg("xts", ".indexCLASS"))
expect_null(attr(y, ".indexFORMAT"), info = create_msg("xts", ".indexFORMAT"))
expect_null(attr(z, ".indexTZ"), info = create_msg("xts", ".indexTZ"))

suppressWarnings({
    x <- .xts(1, Sys.time(), .indexCLASS = "yearmon")
    y <- .xts(1, Sys.time(), .indexFORMAT = "%Y")
    z <- .xts(1, Sys.time(), .indexTZ = "UTC")
})
expect_null(attr(x, ".indexCLASS"), info = create_msg(".xts", ".indexCLASS"))
expect_null(attr(y, ".indexFORMAT"), info = create_msg(".xts", ".indexFORMAT"))
expect_null(attr(z, ".indexTZ"), info = create_msg(".xts", ".indexTZ"))

x <- xts(1, Sys.time(), tclass = "Date")
y <- xts(1, Sys.time(), tformat = "%Y-%m-%d %H:%M")
z <- xts(1, Sys.time(), tzone = "UTC")
expect_null(attr(x, "tclass"), info = create_msg("xts", "tclass"))
expect_null(attr(y, "tformat"), info = create_msg("xts", "tformat"))
expect_null(attr(z, "tzone"), info = create_msg("xts", "tzone"))

x <- .xts(1, Sys.time(), tclass = "Date")
y <- .xts(1, Sys.time(), tformat = "%Y-%m-%d %H:%M")
z <- .xts(1, Sys.time(), tzone = "UTC")
expect_null(attr(x, "tclass"), info = create_msg(".xts", "tclass"))
expect_null(attr(y, "tformat"), info = create_msg(".xts", "tformat"))
expect_null(attr(z, "tzone"), info = create_msg(".xts", "tzone"))


# These error due to `missing("tclass")` instead of `!hasArg("tclass")`
# missing() expects an argument symbol, not a character string. The error is
# not caught in expect_warning() as of tinytest_1.3.1
suppressWarnings(xts(1, as.Date("2018-05-02"), .indexCLASS = "Date"))
suppressWarnings(xts(1, as.Date("2018-05-02"), .indexFORMAT = "%Y"))
suppressWarnings(.xts(1, 1, .indexCLASS = "Date"))
suppressWarnings(.xts(1, 1, .indexFORMAT = "%Y"))


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


### constructors add tformat to the index when it's specified
tf <- "%m/%d/%Y"
x <- xts(1:3, .Date(1:3), tformat = tf)
y <- .xts(1:3, .Date(1:3), tformat = tf)

expect_identical(tf, attr(attr(x, "index"), "tformat"),
                 info = "xts(..., tformat = 'foo') adds tformat to index")
expect_identical(tf, attr(attr(y, "index"), "tformat"),
                 info = ".xts(..., tformat = 'foo') adds tformat to index")


### dimnames come through '...'
x <- xts(1:5, .Date(1:5), dimnames = list(NULL, "x"))
y <- .xts(1:5, 1:5, dimnames = list(NULL, "x"))
expect_equal(colnames(x), colnames(y), info = "xts() and .xts() apply dimnames passed via '...'")
x <- xts(1:5, .Date(1:5), dimnames = list(1:5, "x"))
y <- .xts(1:5, 1:5, dimnames = list(1:5, "x"))
expect_null(rownames(x), info = "xts() doesn't set rownames when dimnames passed via '...'")
expect_null(rownames(y), info = ".xts() doesn't set rownames when dimnames passed via '...'")

m <- matrix(1, dimnames = list("a", "b"))
x <- .xts(m, 1)
expect_null(rownames(x), info = ".xts() on a matrix with rownames does not have rownames")

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


### xts() index attribute precedence should be:
###   1. .index* value (e.g. .indexTZ)  # backward compatibility
###   2. t* value (e.g. tzone)          # current function to override index attribute
###   3. attribute on order.by          # overridden by either 2 above

target_index <- structure(Sys.time(), tzone = "UTC", tclass = "yearmon", tformat = "%Y-%m-%d")

suppressWarnings({
  x <- xts(1, target_index, .indexCLASS = "Date", tclass = "yearqtr")
  y <- xts(1, target_index, .indexFORMAT = "%Y-%b", tformat = "%Y-%m")
  z <- xts(1, target_index, .indexTZ = "Asia/Tokyo", tzone = "Europe/London")
})
expect_identical(tclass(x), "Date", info = "xts() .indexCLASS takes precedence over tclass")
expect_identical(tformat(y), "%Y-%b", info = "xts() .indexFORMAT takes precedence over tformat")
expect_identical(tzone(z), "Asia/Tokyo", info = "xts() .indexTZ takes precedence over tzone")

x <- xts(1, target_index, tclass = "yearqtr")
y <- xts(1, target_index, tformat = "%Y-%m")
z <- xts(1, target_index, tzone = "Europe/London")
expect_identical(tclass(x), "yearqtr", info = "xts() tclass takes precedence over index tclass")
expect_identical(tformat(y), "%Y-%m", info = "xts() tformat takes precedence over index tformat")
expect_identical(tzone(z), "Europe/London", info = "xts() tzone takes precedence over index tzone")

x <- xts(1, target_index)
y <- xts(1, target_index)
z <- xts(1, target_index)
expect_identical(tclass(x), attr(target_index, "tclass"), info = "xts() uses index tclass")
expect_identical(tformat(y), attr(target_index, "tformat"), info = "xts() uses index tformat")
expect_identical(tzone(z), attr(target_index, "tzone"), info = "xts() uses index tzone")

### .xts() index attribute precedence is similar. But we cannot override tclass
### because it's a formal argument with a specific default. Historically .xts()
### has always set the tclass to POSIXct by default, whether or not the 'index'
### argument already had a tclass attribute.
target_index <- structure(as.POSIXlt(Sys.time()), tzone = "UTC", tclass = "yearmon", tformat = "%Y-%m-%d")

suppressWarnings({
  x <- .xts(1, target_index, .indexCLASS = "Date", tclass = "yearqtr")
  y <- .xts(1, target_index, .indexFORMAT = "%Y-%b", tformat = "%Y-%m")
  z <- .xts(1, target_index, .indexTZ = "Asia/Tokyo", tzone = "Europe/London")
})
expect_identical(tclass(x), "Date", info = ".xts() .indexCLASS takes precedence over tclass")
expect_identical(tformat(y), "%Y-%b", info = ".xts() .indexFORMAT takes precedence over tformat")
expect_identical(tzone(z), "Asia/Tokyo", info = ".xts() .indexTZ takes precedence over tzone")

x <- .xts(1, target_index, tclass = "yearqtr")
y <- .xts(1, target_index, tformat = "%Y-%m")
z <- .xts(1, target_index, tzone = "Europe/London")
expect_identical(tclass(x), "yearqtr", info = ".xts() tclass takes precedence over index tclass")
expect_identical(tformat(y), "%Y-%m", info = ".xts() tformat takes precedence over index tformat")
expect_identical(tzone(z), "Europe/London", info = ".xts() tzone takes precedence over index tzone")

x <- .xts(1, target_index)
y <- .xts(1, target_index)
z <- .xts(1, target_index)
# NOTE: as of 0.10-0, .xts() sets tclass on the index to "POSIXct" by default.
#   It does not keep the index argument's tclass if it has one. So overriding
#   the default with the index's tclass attribute is a breaking change.
expect_identical(tclass(x), c("POSIXct", "POSIXt"), info = ".xts() *ignores* index tclass (unlike xts())")
# tformat and tzone are handled the same as in xts()
expect_identical(tformat(y), attr(target_index, "tformat"), info = ".xts() uses index tformat")
expect_identical(tzone(z), attr(target_index, "tzone"), info = ".xts() uses index tzone")

suppressWarnings({
  x <- xts(1, Sys.Date(), tformat = "%Y", .indexCLASS = "Date", .indexTZ = "UTC",
            user = "attribute", hello = "world", dimnames = list(NULL, "x"))
  y <- .xts(1, 1, tformat = "%Y", .indexCLASS = "Date", .indexTZ = "UTC",
            user = "attribute", hello = "world", dimnames = list(NULL, "x"))
})
info_msg <- "xts() adds user attributes"
expect_null(attr(x, "tformat"), info = info_msg)
expect_null(attr(x, "tclass"), info = info_msg)
expect_null(attr(x, "tzone"), info = info_msg)
expect_null(attr(x, ".indexCLASS"), info = info_msg)
expect_null(attr(x, ".indexTZ"), info = info_msg)
expect_identical("attribute", attr(x, "user"), info = info_msg)
expect_identical("world", attr(x, "hello"), info = info_msg)
expect_identical("x", colnames(x), info = info_msg)
info_msg <- ".xts() adds user attributes"
expect_null(attr(y, "tformat"), info = info_msg)
expect_null(attr(y, "tclass"), info = info_msg)
expect_null(attr(y, "tzone"), info = info_msg)
expect_null(attr(y, ".indexCLASS"), info = info_msg)
expect_null(attr(y, ".indexTZ"), info = info_msg)
expect_identical("attribute", attr(y, "user"), info = info_msg)
expect_identical("world", attr(y, "hello"), info = info_msg)
expect_identical("x", colnames(y), info = info_msg)

### constructors should not warn for Date, yearmon, yearqtr, chron::chron, chron::dates
### and should set tzone to UTC for any UTC-equivalent tzone
create_msg <- function(klass, tz, warns = TRUE) {
  warn_part <- if(warns) "warns" else "doesn't warn"
  sprintf("xts(1, %s(...), tzone = '%s') %s",
          klass, tz, warn_part)
}
create_msg. <- function(klass, tz, warns = TRUE) {
  paste0(".", create_msg(klass, tz, warns))
}
ym <- as.yearmon(Sys.Date())
yq <- as.yearqtr(Sys.Date())
for(tz in c("UTC", "GMT", "Etc/UTC", "Etc/GMT", "GMT-0", "GMT+0", "GMT0")) {
  # xts()
  x <- y <- z <- NULL
  expect_silent(x <- xts(1, .Date(1), tzone = tz), info = create_msg("Date()", tz, FALSE))
  expect_silent(y <- xts(1, ym, tzone = tz), info = create_msg("yearmon", tz, FALSE))
  expect_silent(z <- xts(1, yq, tzone = tz), info = create_msg("yearqtr", tz, FALSE))
  expect_identical(tzone(x), "UTC", info = "xts() UTC-equivalent tzone is set to UTC (Date)")
  expect_identical(tzone(y), "UTC", info = "xts() UTC-equivalent tzone is set to UTC (yearmon)")
  expect_identical(tzone(z), "UTC", info = "xts() UTC-equivalent tzone is set to UTC (yearqtr)")
  # .xts()
  x <- y <- z <- NULL
  expect_silent(x <- .xts(1, .Date(1), tzone = tz), info = create_msg.("Date", tz, FALSE))
  expect_silent(y <- .xts(1, ym, tzone = tz), info = create_msg.("yearmon", tz, FALSE))
  expect_silent(z <- .xts(1, yq, tzone = tz), info = create_msg.("yearqtr", tz, FALSE))
  expect_identical(tzone(x), "UTC", info = ".xts() UTC-equivalent tzone is set to UTC (Date)")
  expect_identical(tzone(y), "UTC", info = ".xts() UTC-equivalent tzone is set to UTC (yearmon)")
  expect_identical(tzone(z), "UTC", info = ".xts() UTC-equivalent tzone is set to UTC (yearqtr)")

  if(requireNamespace("chron", quietly = TRUE)) {
    x <- y <- NULL
    expect_silent(x <- xts(1, chron::chron(1, 1), tzone = tz), info = create_msg("chron", tz, FALSE))
    expect_silent(y <- xts(1, chron::dates(1), tzone = tz), info = create_msg("dates", tz, FALSE))
    expect_identical(tzone(x), "UTC", info = "xts() UTC-equivalent tzone is set to UTC (chron)")
    expect_identical(tzone(y), "UTC", info = ".xts() UTC-equivalent tzone is set to UTC (dates)")
    x <- y <- NULL
    expect_silent(x <- .xts(1, chron::chron(1, 1), tzone = tz), info = create_msg.("chron", tz, FALSE))
    expect_silent(y <- .xts(1, chron::dates(1), tzone = tz), info = create_msg.("dates", tz, FALSE))
    expect_identical(tzone(x), "UTC", info = "xts() UTC-equivalent tzone is set to UTC (chron)")
    expect_identical(tzone(y), "UTC", info = ".xts() UTC-equivalent tzone is set to UTC (dates)")
  }
}
### constructors warn and ignore non-UTC tzone for index/order.by classes without timezones
tz <- "America/Chicago"
warn_pattern <- "tzone.*setting ignored for.*indexes"

# xts()
x <- y <- z <- NULL
expect_warning(x <- xts(1, .Date(1), tzone = tz),
               pattern = warn_pattern, info = create_msg("Date", tz, TRUE))
expect_warning(y <- xts(1, ym, tzone = tz),
               pattern = warn_pattern, info = create_msg("yearmon", tz, TRUE))
expect_warning(z <- xts(1, yq, tzone = tz),
               pattern = warn_pattern, info = create_msg("yearqtr", tz, TRUE))
expect_identical(tzone(x), "UTC", info = "xts() non-UTC tzone is set to UTC (Date)")
expect_identical(tzone(y), "UTC", info = "xts() non-UTC tzone is set to UTC (yearmon)")
expect_identical(tzone(z), "UTC", info = "xts() non-UTC tzone is set to UTC (yearqtr)")
# .xts()
x <- y <- z <- NULL
expect_warning(x <- .xts(1, .Date(1), tzone = tz),
               pattern = warn_pattern, info = create_msg("yearqtr", tz, TRUE))
expect_warning(y <- .xts(1, ym, tzone = tz),
               pattern = warn_pattern, info = create_msg("Date", tz, TRUE))
expect_warning(z <- .xts(1, yq, tzone = tz),
               pattern = warn_pattern, info = create_msg("yearmon", tz, TRUE))
expect_identical(tzone(x), "UTC", info = ".xts() non-UTC tzone is set to UTC (Date)")
expect_identical(tzone(y), "UTC", info = ".xts() non-UTC tzone is set to UTC (yearmon)")
expect_identical(tzone(z), "UTC", info = ".xts() non-UTC tzone is set to UTC (yearqtr)")

if(requireNamespace("chron", quietly = TRUE)) {
  x <- y <- NULL
  expect_warning(x <- xts(1, chron::chron(1, 1), tzone = tz),
                 pattern = warn_pattern, info = create_msg("chron", tz, TRUE))
  expect_warning(y <- xts(1, chron::dates(1), tzone = tz),
                 pattern = warn_pattern, info = create_msg("dates", tz, TRUE))
  expect_identical(tzone(x), "UTC", info = "xts() non-UTC tzone is set to UTC (chron)")
  expect_identical(tzone(y), "UTC", info = "xts() non-UTC tzone is set to UTC (dates)")
  x <- y <- NULL
  expect_warning(x <- .xts(1, chron::chron(1, 1), tzone = tz),
                 pattern = warn_pattern, info = create_msg.("chron", tz, TRUE))
  expect_warning(y <- .xts(1, chron::dates(1), tzone = tz),
                 pattern = warn_pattern, info = create_msg.("dates", tz, TRUE))
  expect_identical(tzone(x), "UTC", info = ".xts() non-UTC tzone is set to UTC (chron)")
  expect_identical(tzone(y), "UTC", info = ".xts() non-UTC tzone is set to UTC (dates)")
}

### lists and zero-row data.frames
msg <- "cannot convert lists to xts objects"
expect_error(xts(list(1, 2), .Date(1:2)), msg, info = msg)
#expect_error(.xts(list(1, 2), 1:2), msg, info = msg)

zero_row_df <- data.frame(date = .Date(numeric(0)), x = numeric(0), y = numeric(0))
zero_row_xts <- xts(zero_row_df[, -1], zero_row_df[, 1])

expect_identical(names(zero_row_xts), names(zero_row_df)[-1],
                 info = "xts() keeps names for zero-row data.frame")
expect_equal(.Date(numeric(0)), index(zero_row_xts),
                 info = "xts() has zero-length Date index for zero-row data.frame with Date column")

zero_row_xts. <- .xts(zero_row_df[, -1], zero_row_df[, 1])
expect_identical(names(zero_row_xts.), names(zero_row_df)[-1],
                 info = ".xts() keeps names for zero-row data.frame")
expect_equal(.Date(numeric(0)), index(zero_row_xts.),
                 info = ".xts() has zero-length Date index for zero-row data.frame with Date column")
