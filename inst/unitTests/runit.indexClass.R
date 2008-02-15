data(sample_matrix)

convert_xts <- as.xts(sample_matrix) # indexClass defaults to POSIXct

# convert from 'POSIXct'
convert_POSIXct2Date <- function() {
  x <- convert_xts
  indexClass(x) <- 'Date'
  checkTrue(inherits(indexClass(x),'Date'))
}
convert_POSIXct2chron <- function() {
  library(chron)
  x <- convert_xts
  indexClass(x) <- 'chron'
  checkTrue(inherits(indexClass(x),'chron'))
  detach('package:chron')
}
convert_POSIXct2yearmon <- function() {
  x <- convert_xts
  indexClass(x) <- 'yearmon'
  checkTrue(inherits(indexClass(x),'yearmon'))

}
convert_POSIXct2yearqtr <- function() {
  x <- convert_xts
  indexClass(x) <- 'yearqtr'
  checkTrue(inherits(indexClass(x),'yearqtr'))

}
convert_POSIXct2timeDate <- function() {
  library(fSeries)
  x <- convert_xts
  indexClass(x) <- 'timeDate'
  checkTrue(inherits(indexClass(x),'timeDate'))
}
convert_POSIXct2POSIXct <- function() {
  x <- convert_xts
  indexClass(x) <- 'POSIXct'
  checkTrue(inherits(indexClass(x),'POSIXct'))
}

# Convert from 'Date'
indexClass(convert_xts) <- 'Date'

convert_Date2Date <- function() {
  x <- convert_xts
  indexClass(x) <- 'Date'
  checkTrue(inherits(indexClass(x),'Date'))
}
convert_Date2chron <- function() {
  library(chron)
  x <- convert_xts
  indexClass(x) <- 'chron'
  checkTrue(inherits(indexClass(x),'chron'))
}
convert_Date2yearmon <- function() {
  x <- convert_xts
  indexClass(x) <- 'yearmon'
  checkTrue(inherits(indexClass(x),'yearmon'))
}
convert_Date2yearqtr <- function() {
  x <- convert_xts
  indexClass(x) <- 'yearqtr'
  checkTrue(inherits(indexClass(x),'yearqtr'))
}
convert_Date2timeDate <- function() {
  library(fSeries)
  x <- convert_xts
  indexClass(x) <- 'timeDate'
  checkTrue(inherits(indexClass(x),'timeDate'))
}
convert_Date2POSIXct <- function() {
  x <- convert_xts
  indexClass(x) <- 'POSIXct'
  checkTrue(inherits(indexClass(x),'POSIXct'))
}
