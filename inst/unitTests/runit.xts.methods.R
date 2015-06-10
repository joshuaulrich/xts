#
# RUnit tests for the following 'xts' methods:
# .subset.xts
# window.xts
# .toPOSIXct (indirectly)

test.window <- 
function()
{
  # window function for xts series, use basic logic for testing & debugging
  # null start and end not supported
  window_dbg <- function(x, index. = index(x), start, end)
  {   
    start <- xts:::.toPOSIXct(start, indexTZ(x))
    end <- xts:::.toPOSIXct(end, indexTZ(x))
    index. <- as.POSIXct(index., tz=indexTZ(x))
    all.indexes <- .index(x)
    in.index <- all.indexes %in% index.
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
  bin <- window.xts(x, start = start, end = end)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Range over gap")
  
  # Range over one day
  start <- base + 12*DAY
  end <- base + 12*DAY
  bin <- window.xts(x, start = start, end = end)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Range over one day")
  
  # Empty Range over one day
  start <- base + 11*DAY
  end <- base + 11*DAY
  bin <- window.xts(x, start = start, end = end)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Empty Range over one day")
  
  # Range containing all dates
  start <- base 
  end <- base + 21*DAY
  bin <- window.xts(x, start = start, end = end)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Range containing all dates")

  # Range past end
  start <- base + 16*DAY
  end <- base + 30*DAY
  bin <- window.xts(x, start = start, end = end)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Range past end")

  # Range before begin
  start <- base 
  end <- base + 3*DAY
  bin <- window.xts(x, start = start, end = end)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Range before begin")
  
  # Test just start, end = NULL
  start <- base + 13 * DAY
  end <- base + 30*DAY
  bin <- window.xts(x, start = start)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Test just start, end = NULL")
  
  # Test just start, end = NULL, empty range
  start <- base + 25 * DAY
  end <- base + 30*DAY
  bin <- window.xts(x, start = start)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Test just start, end = NULL, empty range")
  
  # Test just end, start = NULL
  end <- base + 13 * DAY
  start <- base
  bin <- window.xts(x, end = end)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Test just end, start = NULL")
  
  # Test just end, start = NULL, empty range
  end <- base 
  start <- base
  bin <- window.xts(x, end = end)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Test just end, start = NULL, empty range")
  
  # Test end = NULL, start = NULL
  start <- base  
  end <- base + 30*DAY
  bin <- window.xts(x)
  reg <- window_dbg(x, start = start, end = end)
  checkIdentical(bin, reg, "Test end = NULL, start = NULL")
  
  #######################################
  # Test for index. parameter
  start <- base  
  end <- base + 30*DAY
  idx = index(x)[c(2,4,6)]
  bin <- window.xts(x, index. = idx)
  reg <- window_dbg(x, index. = idx, start = start, end = end)
  checkIdentical(bin, reg, "Test for index. parameter")
  
  # Test index. outside range of dates in xts series
  start <- base  
  end <- base + 30*DAY
  idx = c(start, index(x)[c(2,4,6)], end)
  bin <- window.xts(x, index. = idx)
  reg <- window_dbg(x, index. = idx, start = start, end = end)
  checkIdentical(bin, reg, "Test index. outside range of dates in xts series")
  
  # Test NA in index 
  start <- base  
  end <- base + 30*DAY
  idx = c(start, index(x)[c(2,4,6)], end, NA)
  bin <- window.xts(x, index. = idx)
  reg <- window_dbg(x, index. = idx, start = start, end = end)
  checkIdentical(bin, reg, "Test NA in index ")
  
  # Next 3 adapted from window.zoo example
  # Test basic window.zoo example
  x.date <- as.Date(paste(2003, rep(1:4, 4:1), seq(1,19,2), sep = "-"))
  x <- xts(matrix(1:20, ncol = 2), x.date)
  bin <- window.xts(x, start = as.Date("2003-02-01"), end = as.Date("2003-03-01"))
  reg <- window_dbg(x, start = as.Date("2003-02-01"), end = as.Date("2003-03-01"))
  checkIdentical(bin, reg, "Test basic window.zoo example")
  
  # Test index + start
  bin <- window.xts(x, index = x.date[1:6], start = as.Date("2003-02-01"))
  reg <- window_dbg(x, index = x.date[1:6], start = as.Date("2003-02-01"), end = as.Date("2004-01-01"))
  checkIdentical(bin, reg, "Test index + start")
  
  # Test just index
  bin <- window.xts(x, index = x.date[c(4, 8, 10)])
  reg <- window_dbg(x, index = x.date[c(4, 8, 10)], start = as.Date("2003-01-01"), end = as.Date("2004-01-01"))
  checkIdentical(bin, reg, "Test just index")
  
  # Test performance difference
  start <- base + 14*DAY
  end <- base + 14*DAY
  cat("\n")
  print("performance:")
  print("binary search")
  print(system.time(replicate(1000, window.xts(x, start = start, end = end)))) # Binary search is about 2x faster than regular
  print("regular search")
  print(system.time(replicate(1000, window_dbg(x, start = start, end = end)))) 
}

# test subset.xts for date subsetting by row
test.subset.xts <- 
function()
{
  DAY = 24*3600
  base <- as.POSIXct("2000-12-31")
  dts <- base + c(1:10, 12:15, 17:20)*DAY
  x <- xts(1:length(dts), dts)
  
  # Test character
  sub <- .subset.xts(x, "2001-01-10")
  bin <- window.xts(x, start = "2001-01-10", end = "2001-01-10")
  checkIdentical(bin, sub, "Test character")
  
  # Test character vector
  sub <- .subset.xts(x, c("2001-01-10", "2001-01-11", "2001-01-12", "2001-01-13"))  # Note that "2001-01-11" is not in the series. Skipped by convention.
  bin <- window.xts(x, start = "2001-01-10", end = "2001-01-13")
  checkIdentical(bin, sub, "Test character vector")
  
  # Test POSIXct
  sub <- .subset.xts(x, as.POSIXct("2001-01-10"))
  bin <- window.xts(x, start = "2001-01-10", end = "2001-01-10")
  checkIdentical(bin, sub, "Test POSIXct")
  
  # Test POSIXct vector
  sub <- .subset.xts(x, as.POSIXct(c("2001-01-10", "2001-01-11", "2001-01-12", "2001-01-13")))  # Note that "2001-01-11" is not in the series. Skipped by convention.
  bin <- window.xts(x, start = "2001-01-10", end = "2001-01-13")
  checkIdentical(bin, sub, "Test POSIXct vector")
  
  # Test Date
  sub <- .subset.xts(x, as.Date(c("2001-01-10", "2001-01-11", "2001-01-12", "2001-01-13")))  # Note that "2001-01-11" is not in the series. Skipped by convention.
  bin <- window.xts(x, start = "2001-01-10", end = "2001-01-13")
  checkIdentical(bin, sub, "Test Date")
  
  # Test character dates, and single column selection
  x <- xts(1:length(dts), dts)
  y <- xts(rep(2, length(dts)), dts)
  z <- xts(rep(3, length(dts)), dts)
  x2 <- cbind(y, x, z)
  sub <- .subset.xts(x2, c("2001-01-10", "2001-01-11", "2001-01-12", "2001-01-13"), 2)  # Note that "2001-01-11" is not in the series. Skipped by convention.
  bin <- window.xts(x, start = "2001-01-10", end = "2001-01-13")
  checkTrue(nrow(sub) == nrow(bin), "Test character dates, and single column selection")
  checkTrue(all(sub == bin), "Test character dates, and single column selection")
  
  # Test Date Ranges
  x <- xts(1:1000, as.Date("2000-01-01")+1:1000)
  sub <- x['200001'] # January 2000
  bin <- window.xts(x, start = "2000-01-01", end = "2000-01-31")
  checkIdentical(bin, sub, "Test Date Ranges")
  
  # Test Date Ranges 2
  sub <- x['1999/2000'] # All of 2000 (note there is no need to use the exact start)
  bin <- window.xts(x, start = "2000-01-01", end = "2000-12-31")
  checkIdentical(bin, sub, "Test Date Ranges 2")
  
  # Test Date Ranges 3
  sub <- x['1999/200001'] # January 2000 
  bin <- window.xts(x, start = "2000-01-01", end = "2000-01-31")
  checkIdentical(bin, sub, "Test Date Ranges 3")
}

