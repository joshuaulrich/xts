# ensure reclass() preserves index attributes from 'match.to'
info_msg <- "test.reclass_preserves_match.to_tclass"
x <- .xts(1:3, 1:3, tclass = "Date")
y <- reclass(1:3, match.to = x)
expect_identical(tclass(y), "Date", info = info_msg)

info_msg <- "test.reclass_preserves_match.to_tzone"
tz <- "Atlantic/Reykjavik"
x <- .xts(1:3, 1:3, tzone = tz)
y <- reclass(1:3, match.to = x)
expect_identical(tzone(y), tz, info = info_msg)

info_msg <- "test.reclass_preserves_match.to_tformat"
tf <- "%m/%d/%Y %H:%M"
x <- .xts(1:3, 1:3, tformat = tf)
y <- reclass(1:3, match.to = x)
expect_identical(tformat(y), tf, info = info_msg)

info_msg <- "test.reclass_preserves_match.to_xtsAttributes"
xts_attr <- list("hello" = "world")
x <- .xts(1:3, 1:3)
xtsAttributes(x) <- xts_attr
z <- reclass(1:3, match.to = x)
expect_equal(xts_attr, xtsAttributes(z), info = info_msg)

# ensure reclass(xts_object, ...) preserves match.to attributes
info_msg <- "test.reclass_xts_object_preserves_match.to_tclass"
x <- y <- xts(1:3, .Date(1:3))
tclass(x) <- c("POSIXct", "POSIXt")
z <- reclass(x, y)
expect_identical(tclass(y), tclass(z), info = info_msg)

info_msg <- "test.reclass_xts_object_preserves_match.to_tzone"
x <- y <- xts(1:3, .Date(1:3))
tz <- "Atlantic/Reykjavik"
tzone(x) <- tz
z <- reclass(x, y)
expect_identical("UTC", tzone(z), info = info_msg)

info_msg <- "test.reclass_xts_object_preserves_match.to_tformat"
tf <- "%m/%d/%Y"
x <- y <- xts(1:3, .Date(1:3), tformat = tf)
tformat(x) <- "%Y-%m-%d"
z <- reclass(x, y)
expect_identical(tf, tformat(z), info = info_msg)

info_msg <- "test.reclass_xts_object_preserves_match.to_xtsAttributes"
x <- y <- xts(1:3, .Date(1:3))
xts_attr <- list("hello" = "world")
xtsAttributes(y) <- xts_attr
z <- reclass(x, y)
expect_equal(xts_attr, xtsAttributes(z), info = info_msg)
