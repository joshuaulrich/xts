### i = missing, j = NA, object has column names
### See #181
info_msg <- "test.i_missing_j_NA_has_colnames"
iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)
colnames(iina) <- colnames(idna) <-
colnames(dina) <- colnames(ddna) <- rep(NA_character_, 2)

# int data, int index
ii <- .xts(matrix(1:10, 5, 2), 1:5)
colnames(ii) <- c("a", "b")
expect_identical(ii[, NA], iina, info = paste(info_msg, "int data, int index"))
expect_identical(ii[, 1][, NA], iina[, 1], info = paste(info_msg, "int data, int index"))

# int data, dbl index
id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
colnames(id) <- c("a", "b")
expect_identical(id[, NA], idna, info = paste(info_msg, "int data, dbl index"))
expect_identical(id[, 1][, NA], idna[, 1], info = paste(info_msg, "int data, dbl index"))

# dbl data, int index
di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
colnames(di) <- c("a", "b")
expect_identical(di[, NA], dina, info = paste(info_msg, "dbl data, int index"))
expect_identical(di[, 1][, NA], dina[, 1], info = paste(info_msg, "dbl data, int index"))

# dbl data, dbl index
dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
colnames(dd) <- c("a", "b")
expect_identical(dd[, NA], ddna, info = paste(info_msg, "dbl data, dbl index"))
expect_identical(dd[, 1][, NA], ddna[, 1], info = paste(info_msg, "dbl data, dbl index"))


### i = missing, j = NA, object does not have column names
### See #97
info_msg <- "test.i_missing_j_NA_no_colnames"
iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)

# int data, int index
ii <- .xts(matrix(1:10, 5, 2), 1:5)
expect_identical(ii[, NA], iina, info = paste(info_msg, "int data, int index"))
expect_identical(ii[, 1][, NA], iina[, 1], info = paste(info_msg, "int data, int index"))

# int data, dbl index
id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
expect_identical(id[, NA], idna, info = paste(info_msg, "int data, dbl index"))
expect_identical(id[, 1][, NA], idna[, 1], info = paste(info_msg, "int data, dbl index"))

# dbl data, int index
di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
expect_identical(di[, NA], dina, info = paste(info_msg, "dbl data, int index"))
expect_identical(di[, 1][, NA], dina[, 1], info = paste(info_msg, "dbl data, int index"))

# dbl data, dbl index
dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
expect_identical(dd[, NA], ddna, info = paste(info_msg, "dbl data, dbl index"))
expect_identical(dd[, 1][, NA], ddna[, 1], info = paste(info_msg, "dbl data, dbl index"))


### i = integer, j = NA, object has column names
### See #97
info_msg <- "test.i_integer_j_NA_has_colnames"
iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)
colnames(iina) <- colnames(idna) <-
colnames(dina) <- colnames(ddna) <- rep(NA_character_, 2)

i <- 1:3

# int data, int index
ii <- .xts(matrix(1:10, 5, 2), 1:5)
colnames(ii) <- c("a", "b")
expect_identical(ii[i, NA], iina[i,], info = paste(info_msg, "int data, int index"))
expect_identical(ii[i, 1][, NA], iina[i, 1], info = paste(info_msg, "int data, int index"))

# int data, dbl index
id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
colnames(id) <- c("a", "b")
expect_identical(id[i, NA], idna[i,], info = paste(info_msg, "int data, dbl index"))
expect_identical(id[i, 1][, NA], idna[i, 1], info = paste(info_msg, "int data, dbl index"))

# dbl data, int index
di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
colnames(di) <- c("a", "b")
expect_identical(di[i, NA], dina[i,], info = paste(info_msg, "dbl data, int index"))
expect_identical(di[i, 1][, NA], dina[i, 1], info = paste(info_msg, "dbl data, int index"))

# dbl data, dbl index
dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
colnames(dd) <- c("a", "b")
expect_identical(dd[i, NA], ddna[i,], info = paste(info_msg, "dbl data, dbl index"))
expect_identical(dd[i, 1][, NA], ddna[i, 1], info = paste(info_msg, "dbl data, dbl index"))


### i = integer, j = NA, object does not have column names
### See #97
info_msg <- "test.i_integer_j_NA_no_colnames"
iina <- .xts(matrix(NA_integer_, 5, 2), 1:5)
idna <- .xts(matrix(NA_integer_, 5, 2), 1.0 * 1:5)
dina <- .xts(matrix(NA_real_, 5, 2), 1:5)
ddna <- .xts(matrix(NA_real_, 5, 2), 1.0 * 1:5)
i <- 1:3

# int data, int index
ii <- .xts(matrix(1:10, 5, 2), 1:5)
expect_identical(ii[i, NA], iina[i,], info = paste(info_msg, "int data, int index"))
expect_identical(ii[i, 1][, NA], iina[i, 1], info = paste(info_msg, "int data, int index"))

# int data, dbl index
id <- .xts(matrix(1:10, 5, 2), 1.0 * 1:5)
expect_identical(id[i, NA], idna[i,], info = paste(info_msg, "int data, dbl index"))
expect_identical(id[i, 1][, NA], idna[i, 1], info = paste(info_msg, "int data, dbl index"))

# dbl data, int index
di <- .xts(1.0 * matrix(1:10, 5, 2), 1:5)
expect_identical(di[i, NA], dina[i,], info = paste(info_msg, "dbl data, int index"))
expect_identical(di[i, 1][, NA], dina[i, 1], info = paste(info_msg, "dbl data, int index"))

# dbl data, dbl index
dd <- .xts(1.0 * matrix(1:10, 5, 2), 1.0 * 1:5)
expect_identical(dd[i, NA], ddna[i,], info = paste(info_msg, "dbl data, dbl index"))
expect_identical(dd[i, 1][, NA], ddna[i, 1], info = paste(info_msg, "dbl data, dbl index"))


info_msg <- "test.i_0"
x <- .xts(matrix(1:10, 5, 2), 1:5)
z <- as.zoo(x)
xz0 <- as.xts(z[0,])
expect_equal(x[0,], xz0, info = info_msg)

### Subset by non-numeric classes
X <- xts(1:5, as.Date("2018-04-21") - 5:1)

info_msg <- "test.i_character"
x <- X
for (r in c(1L, 3L, 5L)) {
  y <- x[r,]
  i <- as.character(index(y))
  expect_identical(y, x[i, ], info = paste(info_msg, "i =", r))
}

info_msg <- "test.i_asis_character"
x <- X
for (r in c(1L, 3L, 5L)) {
  y <- x[r,]
  i <- as.character(index(y))
  expect_identical(y, x[I(i), ], info = paste(info_msg, "r =", r))
}

info_msg <- "test.i_Date"
x <- X
for (r in c(1L, 3L, 5L)) {
  y <- x[r,]
  i <- index(y)
  expect_identical(y, x[i, ], info = paste(info_msg, "r =", r))
}

info_msg <- "test.i_POSIXct"
x <- X
index(x) <- as.POSIXct(index(x), tz = "UTC")
for (r in c(1L, 3L, 5L)) {
  y <- x[r,]
  i <- index(y)
  expect_identical(y, x[i, ], info = paste(info_msg, "r =", r))
}

info_msg <- "test.i_POSIXlt"
x <- X
index(x) <- as.POSIXlt(index(x), tz = "UTC")
for (r in c(1L, 3L, 5L)) {
  y <- x[r,]
  i <- index(y)
  expect_identical(y, x[i, ], info = paste(info_msg, "r =", r))
}

### invalid date/time
info_msg <- "test.i_invalid_date_string"
x <- xts(1:10, as.Date("2015-02-20")+0:9)
expect_warning(y <- x["2012-02-30"], pattern = "cannot determine first and last time")
expect_identical(y, x[NA,], info = info_msg)

info_msg <- "test.i_only_range_separator_or_empty_string"
x <- xts(1:10, as.Date("2015-02-20")+0:9)
y <- x["/",]
expect_identical(y, x, info = paste(info_msg, "sep = '/'"))
y <- x["::",]
expect_identical(y, x, info = paste(info_msg, "sep = '::'"))
y <- x["",]
expect_identical(y, x, info = paste(info_msg, "sep = ''"))

info_msg <- "test.i_date_range_open_end"
x <- xts(1:10, as.Date("2015-02-20")+0:9)
y <- x["2015-02-23/",]
expect_identical(y, x[4:10,], info = info_msg)

info_msg <- "test.i_date_range_open_start"
x <- xts(1:10, as.Date("2015-02-20")+0:9)
y <- x["/2015-02-26",]
expect_identical(y, x[1:7,], info = info_msg)

### subset empty xts
info_msg <- "empty xts subset by datetime matches zoo"
d0 <- as.Date(integer())
zl <- xts(, d0)
empty <- as.xts(as.zoo(zl)[i,])
i <- Sys.Date()
expect_identical(zl[i,], empty, info = paste(info_msg, "i = Date, [i,]"))
expect_identical(zl[i],  empty, info = paste(info_msg, "i = Date, [i]"))
i <- Sys.time()
expect_identical(zl[i,], empty, info = paste(info_msg, "i = POSIXct, [i,]"))
expect_identical(zl[i],  empty, info = paste(info_msg, "i = POSIXct, [i]"))

info_msg <- "empty xts subset by 0 matches zoo"
d0 <- as.Date(integer())
zl <- xts(, d0)
empty <- as.xts(as.zoo(zl)[0,])
expect_identical(zl[0,], empty, info = paste(info_msg, "[i,]"))
expect_identical(zl[0],  empty, info = paste(info_msg, "[i]"))

info_msg <- "empty xts subset by -1 matches zoo"
d0 <- as.Date(integer())
zl <- xts(, d0)
empty <- as.xts(as.zoo(zl)[i,])
expect_identical(zl[-1,], empty, info = paste(info_msg, "[-1,]"))
expect_identical(zl[-1],  empty, info = paste(info_msg, "[-1]"))

info_msg <- "empty xts subset by NA matches zoo"
d0 <- as.Date(integer())
zl <- xts(, d0)
empty <- as.xts(as.zoo(zl)[i,])
expect_identical(zl[NA,], empty, info = paste(info_msg, "[NA,]"))
expect_identical(zl[NA],  empty, info = paste(info_msg, "[NA]"))

info_msg <- "empty xts subset by NULL matches zoo"
d0 <- as.Date(integer())
zl <- xts(, d0)
empty <- as.xts(as.zoo(zl)[i,])
expect_identical(zl[NULL,], empty, info = paste(info_msg, "[NULL,]"))
expect_identical(zl[NULL],  empty, info = paste(info_msg, "[NULL]"))

info_msg <- "test.duplicate_index_duplicate_i"
dates <-
  structure(c(15770, 16257, 16282, 16291, 16296, 16296, 16298, 16301,
              16432, 16452), class = "Date")
x <- xts(c(1, 2, 2, 3, 3, 3, 3, 3, 4, 4), dates)
dupdates <-
  structure(c(15770, 16257, 16282, 16291, 16296, 16296, 16296, 16296,
              16298, 16301, 16432, 16452), class = "Date")
y <- xts(c(1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4), dupdates)
expect_identical(x[index(x),],  y, info = info_msg)

### Test dispatch to zoo for yearmon, yearqtr tclass
info_msg <- "test.window_yearmon_yearqtr_tclass_dispatches_to_zoo"
i1 <- seq(as.yearmon(2007), by = 1/12, length.out = 36)
x1 <- xts(1:36, i1)
i2 <- seq(as.yearqtr(2007), by = 1/4, length.out = 36)
x2 <- xts(1:36, i2)
r1 <- x1["2015"]
r2 <- x2["2015"]

########## results are empty objects ##########
### zoo supports numeric start for yearmon and yearqtr
w1 <- window(x1, start = 2015.01)  # to window.zoo()
w2 <- window(x2, start = 2015.1)   # to window.zoo()
expect_equal(r1, w1, info = paste(info_msg, "window, yearmon, numeric start, empty range"))
expect_equal(r2, w2, info = paste(info_msg, "window, yearqtr, numeric start, empty range"))

w1 <- window(x1, start = "2015-01-01")  # to window.xts()
w2 <- window(x2, start = "2015Q1")      # to window.zoo()
expect_equal(r1, w1, info = paste(info_msg, "window, yearmon, character start, empty range"))
expect_equal(r2, w2, info = paste(info_msg, "window, yearqtr, character start, empty range"))

w1 <- window(x1, start = "2015-01-01", end = NA)  # to window.xts()
expect_equal(r1, w1, info = paste(info_msg, "window, yearmon, character start with end = NA, empty range"))

########## results are *not* empty objects ##########
r1 <- x1["2011/"]
r2 <- x2["2011/"]

w1 <- window(x1, start = 2011.01)  # to window.zoo()
w2 <- window(x2, start = 2011.1)   # to window.zoo()
expect_equal(r1, w1, info = paste(info_msg, "window, yearmon, numeric start"))
expect_equal(r2, w2, info = paste(info_msg, "window, yearqtr, numeric start"))

w1 <- window(x1, start = "2011-01-01")  # to window.xts()
w2 <- window(x2, start = "2011Q1")      # to window.zoo()
expect_equal(r1, w1, info = paste(info_msg, "window, yearmon, character start"))
expect_equal(r2, w2, info = paste(info_msg, "window, yearqtr, character start"))

w1 <- window(x1, start = "2011-01-01", end = NA)  # to window.xts()
expect_equal(r1, w1, info = paste(info_msg, "window, yearmon, character start with end = NA"))

info_msg <- "test.zero_width_subset_does_not_drop_class"
target <- c("custom", "xts", "zoo")
x <- .xts(1:10, 1:10, class = target)
y <- x[,0]
expect_equal(target, class(y), info = info_msg)

info_msg <- "test.zero_width_subset_does_not_drop_user_attributes"
x <- .xts(1:10, 1:10, my_attr = "hello")
y <- x[,0]
expect_equal("hello", attr(y, "my_attr"), info = info_msg)

info_msg <- "test.zero_length_subset_xts_returns_same_tclass"
x <- .xts(matrix(1)[0,], integer(0), "Date")
expect_equal(tclass(x[0,]), "Date")
x <- .xts(matrix(1)[0,], integer(0), "POSIXct", "America/Chicago")
expect_equal(tclass(x[0,]), "POSIXct")
expect_equal(tzone(x[0,]), "America/Chicago")

info_msg <- "test.zero_length_subset_returns_same_storage_mode"
tf <- c(TRUE, FALSE)
# integer
sm <- "integer"
x <- .xts(matrix(integer(0), 0), integer(0))
expect_equal(storage.mode(x[0, ]),      sm, info = paste(info_msg, ": x[0,]"))
expect_equal(storage.mode(x[0, 0]),     sm, info = paste(info_msg, ": x[0, 0"))
expect_equal(storage.mode(x[0, FALSE]), sm, info = paste(info_msg, ": x[0, FALSE]"))

x <- .xts(matrix(integer(0), 0, 2), integer(0))
expect_equal(storage.mode(x[0,]),    sm, info = paste(info_msg, ": x[0,]"))
expect_equal(storage.mode(x[0, 1]),  sm, info = paste(info_msg, ": x[0, 1]"))
expect_equal(storage.mode(x[0, tf]), sm, nfo = paste(info_msg, ": x[0, c(TRUE, FALSE)]"))

# double
sm <- "double"
x <- .xts(matrix(numeric(0), 0), integer(0))
expect_equal(storage.mode(x[0, ]),      sm, info = paste(info_msg, ": x[0,]"))
expect_equal(storage.mode(x[0, 0]),     sm, info = paste(info_msg, ": x[0, 0]"))
expect_equal(storage.mode(x[0, FALSE]), sm, info = paste(info_msg, ": x[0, FALSE]"))

x <- .xts(matrix(numeric(0), 0, 2), integer(0))
expect_equal(storage.mode(x[0,]),    sm, info = paste(info_msg, ": x[0,]"))
expect_equal(storage.mode(x[0, 1]),  sm, info = paste(info_msg, ": x[0, 1]"))
expect_equal(storage.mode(x[0, tf]), sm, info = paste(info_msg, ": x[0, c(TRUE, FALSE)]"))

# non-integer subset
x <- .xts(matrix(1:20, 10, 2), 1:10)
# subset by non-integer-like 'i' warns
expect_warning(x[-1.5, ])
expect_warning(x[ 0.5, ])
expect_warning(x[ 1.5, ])
# subset by non-integer-like 'j' warns
expect_warning(x[, -1.5])
expect_warning(x[,  0.5])
expect_warning(x[,  1.5])
