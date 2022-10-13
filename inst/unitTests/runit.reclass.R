# ensure reclass() preserves index attributes from 'match.to'
test.reclass_preserves_match.to_tclass <- function() {
  x <- .xts(1:3, 1:3, tclass = "Date")
  y <- reclass(1:3, match.to = x)
  checkIdentical(tclass(y), "Date")
}

test.reclass_preserves_match.to_tzone <- function() {
  tz <- "Atlantic/Reykjavik"
  x <- .xts(1:3, 1:3, tzone = tz)
  y <- reclass(1:3, match.to = x)
  checkIdentical(tzone(y), tz)
}

test.reclass_preserves_match.to_tformat <- function() {
  tf <- "%m/%d/%Y %H:%M"
  x <- .xts(1:3, 1:3, tformat = tf)
  y <- reclass(1:3, match.to = x)
  checkIdentical(tformat(y), tf)
}

test.reclass_preserves_match.to_xtsAttributes <- function() {
  xts_attr <- list("hello" = "world")
  x <- .xts(1:3, 1:3)
  xtsAttributes(x) <- xts_attr

  z <- reclass(1:3, match.to = x)
  checkEquals(xts_attr, xtsAttributes(z))
}

# ensure reclass(xts_object, ...) preserves match.to attributes
test.reclass_xts_object_preserves_match.to_tclass <- function() {
  x <- y <- xts(1:3, .Date(1:3))
  tclass(x) <- c("POSIXct", "POSIXt")

  z <- reclass(x, y)
  checkIdentical(tclass(y), tclass(z))
}

test.reclass_xts_object_preserves_match.to_tzone <- function() {
  x <- y <- xts(1:3, .Date(1:3))
  tz <- "Atlantic/Reykjavik"
  tzone(x) <- tz

  z <- reclass(x, y)
  checkIdentical("UTC", tzone(z))
}

test.reclass_xts_object_preserves_match.to_tformat <- function() {
  tf <- "%m/%d/%Y"
  x <- y <- xts(1:3, .Date(1:3), tformat = tf)
  tformat(x) <- "%Y-%m-%d"

  z <- reclass(x, y)
  checkIdentical(tf, tformat(z))
}

test.reclass_xts_object_preserves_match.to_xtsAttributes <- function() {
  x <- y <- xts(1:3, .Date(1:3))
  xts_attr <- list("hello" = "world")
  xtsAttributes(y) <- xts_attr

  z <- reclass(x, y)
  checkEquals(xts_attr, xtsAttributes(z))
}
