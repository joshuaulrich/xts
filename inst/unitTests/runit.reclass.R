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
