# unit tests for the following 'xts' methods:
# rbind
# cbind
#
info_msg <- "test.rbind_zero_length_non_zero_length_POSIXct_errors"
xpz <- xts( , as.POSIXct("2017-01-01"))
xp1 <- xts(1, as.POSIXct("2017-01-02"))
zpz <- as.zoo(xpz)
zp1 <- as.zoo(xp1)
zpe <- tryCatch(rbind(zpz, zp1), error = identity)
xpe <- tryCatch(rbind(xpz, xp1), error = identity)
expect_identical(zpe$message, xpe$message, info = info_msg)

info_msg <- "test.rbind_zero_length_non_zero_length_Date_errors"
xpz <- xts( , as.Date("2017-01-01"))
xp1 <- xts(1, as.Date("2017-01-02"))
zpz <- as.zoo(xpz)
zp1 <- as.zoo(xp1)
zpe <- tryCatch(rbind(zpz, zp1), error = identity)
xpe <- tryCatch(rbind(xpz, xp1), error = identity)
expect_identical(zpe$message, xpe$message, info = info_msg)


info_msg <- "test.rbind_no_dim_does_not_error"
d <- rep(0.1, 2)
i <- rep(581910048, 2)
xts_no_dim <-
  structure(d[1], class = c("xts", "zoo"),
            index = structure(i[1], tzone = "UTC", tclass = "Date"))
xts_out <-
  structure(d, class = c("xts", "zoo"), .Dim = 2:1,
            index = structure(i, tzone = "UTC", tclass = "Date"))
xts_rbind <- rbind(xts_no_dim, xts_no_dim)
expect_identical(xts_out, xts_rbind, info = info_msg)


# Test that as.Date.numeric() works at the top level (via zoo::as.Date()),
# and for functions defined in the xts namespace even if xts::as.Date.numeric()
# is not formally registered as an S3 method.
info_msg <- "test.as.Date.numeric"
# Define function that calls as.Date.numeric() ...
f <- function(d) {
  as.Date(d)
}
# ... in xts' namespace
environment(f) <- as.environment("package:xts")

dd <- as.Date("2017-12-13")
dn <- unclass(dd)
expect_identical(dd, as.Date(dn), info = info_msg)  # via zoo::as.Date()
expect_identical(dd, f(dn), info = info_msg)

# .subset.xts
# window.xts
# .toPOSIXct (indirectly)

info_msg <- "test.window"
# window function for xts series, use basic logic for testing & debugging
# null start and end not supported
window_dbg <- function(x, index. = index(x), start, end)
{
  start <- xts:::.toPOSIXct(start, tzone(x))
  end <- xts:::.toPOSIXct(end, tzone(x))
  index. <- as.POSIXct(index., tz=tzone(x))
  all.indexes <- .index(x)
  in.index <- all.indexes %in% as.numeric(index.)
  matches <- (in.index & all.indexes >= start & all.indexes <= end)
  x[matches,]
}

DAY = 24*3600
base <- as.POSIXct("2000-12-31")
dts <- base + c(1:10, 12:15, 17:20)*DAY
x <- xts(1:length(dts), dts)

# Range over gap
start <- base + 11*DAY
end <- base + 16*DAY
bin <- window(x, start = start, end = end)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- range over gap"))

# Range over one day
start <- base + 12*DAY
end <- base + 12*DAY
bin <- window(x, start = start, end = end)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- range over one day"))

# Empty Range over one day
start <- base + 11*DAY
end <- base + 11*DAY
bin <- window(x, start = start, end = end)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- empty Range over one day"))

# Range containing all dates
start <- base
end <- base + 21*DAY
bin <- window(x, start = start, end = end)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- range containing all dates"))

# Range past end
start <- base + 16*DAY
end <- base + 30*DAY
bin <- window(x, start = start, end = end)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- range past end"))

# Range before begin
start <- base
end <- base + 3*DAY
bin <- window(x, start = start, end = end)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- range before begin"))

# Test just start, end = NULL
start <- base + 13 * DAY
end <- base + 30*DAY
bin <- window(x, start = start)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- just start, end = NULL"))

# Test just start, end = NULL, empty range
start <- base + 25 * DAY
end <- base + 30*DAY
bin <- window(x, start = start)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- just start, end = NULL, empty range"))

# Test just end, start = NULL
end <- base + 13 * DAY
start <- base
bin <- window(x, end = end)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- just end, start = NULL"))

# Test just end, start = NULL, empty range
end <- base
start <- base
bin <- window(x, end = end)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- just end, start = NULL, empty range"))

# Test end = NULL, start = NULL
start <- base
end <- base + 30*DAY
bin <- window(x)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- end = NULL, start = NULL"))

# Test just start, end = NA
start <- base + 13 * DAY
end <- base + 30*DAY
bin <- window(x, start = start, end = NA)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- just start, end = NA"))

# Test just start, end = NA, empty range
start <- base + 25 * DAY
end <- base + 30*DAY
bin <- window(x, start = start, end = NA)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- just start, end = NA, empty range"))

# Test just end, start = NA
end <- base + 13 * DAY
start <- base
bin <- window(x, start = NA, end = end)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- just end, start = NA"))

# Test just end, start = NA, empty range
end <- base
start <- base
bin <- window(x, start = NA, end = end)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- just end, start = NA, empty range"))

# Test end = NA, start = NA
start <- base
end <- base + 30*DAY
bin <- window(x, start = NA, end = NA)
reg <- window_dbg(x, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- end = NA, start = NA"))

#######################################
# Test for index. parameter
start <- base
end <- base + 30*DAY
idx = index(x)[c(2,4,6)]
bin <- window(x, index. = idx)
reg <- window_dbg(x, index. = idx, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- index. parameter provided"))

# Test index. outside range of dates in xts series
start <- base
end <- base + 30*DAY
idx = c(start, index(x)[c(2,4,6)], end)
bin <- window(x, index. = idx)
reg <- window_dbg(x, index. = idx, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- index. outside range of dates in xts series"))

# Test NA in index
start <- base
end <- base + 30*DAY
idx = c(start, index(x)[c(2,4,6)], end, NA)
bin <- window(x, index. = idx)
reg <- window_dbg(x, index. = idx, start = start, end = end)
expect_identical(bin, reg, info = paste(info_msg, "- NA in index "))

# Next 3 adapted from window.zoo example
# Test basic window.zoo example
x.date <- as.Date(paste(2003, rep(1:4, 4:1), seq(1,19,2), sep = "-"))
x <- xts(matrix(1:20, ncol = 2), x.date)
bin <- window(x, start = as.Date("2003-02-01"), end = as.Date("2003-03-01"))
reg <- window_dbg(x, start = as.Date("2003-02-01"), end = as.Date("2003-03-01"))
expect_identical(bin, reg, info = paste(info_msg, "- basic window.zoo example"))

# Test index + start
bin <- window(x, index. = x.date[1:6], start = as.Date("2003-02-01"))
reg <- window_dbg(x, index. = x.date[1:6], start = as.Date("2003-02-01"), end = as.Date("2004-01-01"))
expect_identical(bin, reg, info = paste(info_msg, "- index + start"))

# Test just index
bin <- window(x, index. = x.date[c(4, 8, 10)])
reg <- window_dbg(x, index. = x.date[c(4, 8, 10)], start = as.Date("2003-01-01"), end = as.Date("2004-01-01"))
expect_identical(bin, reg, info = paste(info_msg, "- just index"))

# Test decreasing index
bin <- window(x, index. = x.date[c(10, 8, 4)])
reg <- window_dbg(x, index. = x.date[c(10, 8, 4)], start = as.Date("2003-01-01"), end = as.Date("2004-01-01"))
expect_identical(bin, reg, info = paste(info_msg, "- decreasing index"))

# Test index parameter with repeated dates in xts series
idx <- sort(rep(1:5, 5))
x <- xts(1:length(idx), as.Date("1999-12-31")+idx)
bin <- window(x, index. = as.Date("1999-12-31")+c(1,3,5))
reg <- window_dbg(x, index. = as.Date("1999-12-31")+c(1,3,5), start = as.Date("2000-01-01"), end = as.Date("2000-01-05"))
expect_identical(bin, reg, info = paste(info_msg, "- index parameter with repeated dates in xts series"))
expect_true(nrow(bin) == 3*5, info = paste(info_msg, "- index parameter with repeated dates in xts series"))

# Test performance difference
DAY = 24*3600
base <- as.POSIXct("2000-12-31")
dts <- base + c(1:10, 12:15, 17:20)*DAY
x <- xts(1:length(dts), dts)
start <- base + 14*DAY
end <- base + 14*DAY
#cat("\n")
#print("performance:")
#print("binary search")
#print(system.time(replicate(1000, window(x, start = start, end = end)))) # Binary search is about 2x faster than regular
#print("regular search")
#print(system.time(replicate(1000, window_dbg(x, start = start, end = end))))

# test subset.xts for date subsetting by row
info_msg <- "test.subset_i_datetime_or_character"
base <- as.POSIXct("2000-12-31")
dts <- base + c(1:10, 12:15, 17:20) * 24L * 3600L
x <- xts(seq_along(dts), dts)

# Note that "2001-01-11" is not in the series. Skipped by convention.
d <- c("2001-01-10", "2001-01-11", "2001-01-12", "2001-01-13")

for (type in c("double", "integer")) {
  storage.mode(.index(x)) <- type

  # Test scalar
  msg <- paste0(info_msg, " scalar, ", type, " index")
  bin <- window(x, start = d[1], end = d[1])
  expect_identical(bin, x[d[1], ],             info = paste("character", msg))
  expect_identical(bin, x[I(d[1]), ],          info = paste("as-is character", msg))
  expect_identical(bin, x[as.POSIXct(d[1]), ], info = paste("POSIXct", msg))
  expect_identical(bin, x[as.Date(d[1]), ],    info = paste("Date", msg))

  # Test vector
  msg <- paste0(info_msg, " vector, ", type, " index")
  bin <- window(x, start = d[1], end = d[length(d)])
  expect_identical(bin, x[d, ],             info = paste("character", msg))
  expect_identical(bin, x[I(d), ],          info = paste("as-is character", msg))
  expect_identical(bin, x[as.POSIXct(d), ], info = paste("POSIXct", msg))
  expect_identical(bin, x[as.Date(d), ],    info = paste("Date", msg))

  # Test character dates, and single column selection
  y <- xts(rep(2, length(dts)), dts)
  z <- xts(rep(3, length(dts)), dts)
  x2 <- cbind(y, x, z)
  sub <- x2[d, 2]  # Note that "2001-01-11" is not in the series. Skipped by convention.
  bin <- window(x, start = d[1], end = d[length(d)])
  expect_equal(nrow(sub), nrow(bin),
               info = paste(info_msg, "- character dates, and single column selection"))
  expect_true(all(sub == bin),
              info = paste(info_msg, "- character dates, and single column selection"))
}

info_msg <- "test.subset_i_ISO8601"
x <- xts(1:1000, as.Date("2000-01-01")+1:1000)
for (type in c("double", "integer")) {
  storage.mode(.index(x)) <- type

  # Test Date Ranges
  sub <- x['200001'] # January 2000
  bin <- window(x, start = "2000-01-01", end = "2000-01-31")
  expect_identical(bin, sub, info = paste(info_msg, ", i = 2000-01"))

  # Test Date Ranges 2
  sub <- x['1999/2000'] # All of 2000 (note there is no need to use the exact start)
  bin <- window(x, start = "2000-01-01", end = "2000-12-31")
  expect_identical(bin, sub, info = paste(info_msg, ", i = 1999/2000"))

  # Test Date Ranges 3
  sub <- x['1999/200001'] # January 2000
  bin <- window(x, start = "2000-01-01", end = "2000-01-31")
  expect_identical(bin, sub, info = paste(info_msg, ", i= 1999/2000-01"))
}
