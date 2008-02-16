data(sample_matrix)

convert_xts <- as.xts(sample_matrix) # indexClass defaults to POSIXct

# convert from 'POSIXct'
test.convert_POSIXct2Date <- function() {
  x <- convert_xts
  indexClass(x) <- 'Date'
  checkTrue(inherits(index(x),'Date'))
}
test.convert_POSIXct2chron <- function() {
  library(chron)
  x <- convert_xts
  indexClass(x) <- 'chron'
  checkTrue(inherits(index(x),'dates'))
  detach('package:chron')
}
test.convert_POSIXct2yearmon <- function() {
  x <- convert_xts
  indexClass(x) <- 'yearmon'
  checkTrue(inherits(index(x),'yearmon'))

}
test.convert_POSIXct2yearqtr <- function() {
  x <- convert_xts
  indexClass(x) <- 'yearqtr'
  checkTrue(inherits(index(x),'yearqtr'))

}
test.convert_POSIXct2timeDate <- function() {
  library(fSeries)
  x <- convert_xts
  indexClass(x) <- 'timeDate'
  checkTrue(inherits(index(x),'timeDate'))
}
test.convert_POSIXct2POSIXct <- function() {
  x <- convert_xts
  indexClass(x) <- 'POSIXct'
  checkTrue(inherits(index(x),'POSIXct'))
}

# Convert from 'Date'
indexClass(convert_xts) <- 'Date'

test.convert_Date2Date <- function() {
  x <- convert_xts
  indexClass(x) <- 'Date'
  checkTrue(inherits(index(x),'Date'))
}
test.convert_Date2chron <- function() {
  library(chron)
  x <- convert_xts
  indexClass(x) <- 'chron'
  checkTrue(inherits(index(x),'dates'))
}
test.convert_Date2yearmon <- function() {
  x <- convert_xts
  indexClass(x) <- 'yearmon'
  checkTrue(inherits(index(x),'yearmon'))
}
test.convert_Date2yearqtr <- function() {
  x <- convert_xts
  indexClass(x) <- 'yearqtr'
  checkTrue(inherits(index(x),'yearqtr'))
}
test.convert_Date2timeDate <- function() {
  library(fSeries)
  x <- convert_xts
  indexClass(x) <- 'timeDate'
  checkTrue(inherits(index(x),'timeDate'))
}
test.convert_Date2POSIXct <- function() {
  x <- convert_xts
  indexClass(x) <- 'POSIXct'
  checkTrue(inherits(index(x),'POSIXct'))
}
