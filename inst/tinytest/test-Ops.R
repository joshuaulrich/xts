all.modes <- c("double", "integer", "logical", "character")
ops.math <- c("+", "-", "*", "/", "^", "%%", "%/%")
ops.relation <- c(">", ">=", "==", "!=", "<=", "<")
ops.logic <- c("&", "|", ops.relation)
all.ops <- c(ops.math, ops.logic)

ops_numeric_tester <-
function(e1, e2, mode, op)
{
  storage.mode(e1) <- mode
  storage.mode(e2) <- mode
  eval(call(op, e1, e2))
}

make_msg <- function(info, op, type)
{
  sprintf("%s op: %s, type: %s", info, op, type)
}

### {{{ 2-column objects
info_msg <- "test.ops_xts2d_matrix2d_dimnames"
X1 <- .xts(cbind(1:3, 4:6), 1:3, dimnames = list(NULL, c("x", "y")))
M1 <- as.matrix(X1) * 5
M2 <- M1
colnames(M2) <- rev(colnames(M2))
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, M1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), M1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments should only change column names
    e <- ops_numeric_tester(M2, X1, m, o)
    E <- X1
    colnames(E) <- colnames(M2)
    E[] <- ops_numeric_tester(M2, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts2d_matrix2d_only_colnames"
X1 <- .xts(cbind(1:3, 4:6), 1:3, dimnames = list(NULL, c("x", "y")))
M1 <- coredata(X1) * 5
M2 <- M1
colnames(M2) <- rev(colnames(M2))
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, M1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), M1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments should only change column names
    e <- ops_numeric_tester(M2, X1, m, o)
    E <- X1
    colnames(E) <- colnames(M2)
    E[] <- ops_numeric_tester(M2, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts2d_matrix2d_only_rownames"

X1 <- .xts(cbind(1:3, 4:6), 1:3)
M1 <- coredata(X1) * 5
rownames(M1) <- format(.POSIXct(1:3))
M2 <- M1
colnames(M2) <- rev(colnames(M2))

for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, M1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), M1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments should only change column names
    e <- ops_numeric_tester(M2, X1, m, o)
    E <- X1
    colnames(E) <- colnames(M2)
    E[] <- ops_numeric_tester(M2, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts2d_matrix2d_no_dimnames"
X1 <- .xts(cbind(1:3, 1:3), 1:3)
M1 <- coredata(X1) * 5
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, M1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), M1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments shouldn't matter
    e <- ops_numeric_tester(M1, X1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(M1, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}
### }}} 2-column objects

### {{{ 1-column objects
info_msg <- "test.ops_xts1d_matrix1d_dimnames"
X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
M1 <- as.matrix(X1) * 5
M2 <- M1
colnames(M2) <- "y"
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, M1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), M1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments should only change column names
    e <- ops_numeric_tester(M2, X1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(M2, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    colnames(E) <- "y"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts1d_matrix1d_only_colnames"
X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
M1 <- coredata(X1) * 5
M2 <- M1
colnames(M2) <- "y"
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, M1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), M1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments should only change column names
    e <- ops_numeric_tester(M2, X1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(M2, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    colnames(E) <- "y"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts1d_matrix1d_only_rownames"
X1 <- .xts(1:3, 1:3)
M1 <- coredata(X1) * 5
rownames(M1) <- format(.POSIXct(1:3))
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, M1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), M1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments shouldn't matter
    e <- ops_numeric_tester(M1, X1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(M1, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts1d_matrix1d_no_dimnames"
X1 <- .xts(1:3, 1:3)
M1 <- coredata(X1) * 5
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, M1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), M1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments shouldn't matter
    e <- ops_numeric_tester(M1, X1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(M1, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts1d_xts1d"
X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next
    e <- ops_numeric_tester(X1, X1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(X1), coredata(X1), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts1d_xts1d_different_index"
X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
X2 <- .xts(2:4, 2:4, dimnames = list(NULL, "y"))
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, X2, m, o)
    E <- X1[2:3,]
    E[] <- ops_numeric_tester(coredata(E), coredata(X2[1:2,]), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments should only change column names
    e <- ops_numeric_tester(X2, X1, m, o)
    E <- X2[1:2,]
    E[] <- ops_numeric_tester(coredata(X1[2:3,]), coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}
### }}} 1-column objects

### {{{ xts with dim, vector
info_msg <- "test.ops_xts2d_vector_no_names"
X1 <- .xts(cbind(1:3, 4:6), 1:3, dimnames = list(NULL, c("x", "y")))
V1 <- as.vector(coredata(X1[,1L])) * 5
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, V1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), V1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments shouldn't matter
    e <- ops_numeric_tester(V1, X1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(V1, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts2d_vector_names"
X1 <- .xts(cbind(1:3, 4:6), 1:3, dimnames = list(NULL, c("x", "y")))
V1 <- setNames(as.vector(X1[,1L]), index(X1)) * 5
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, V1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), V1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments shouldn't matter
    e <- ops_numeric_tester(V1, X1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(V1, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts1d_vector_no_names"
X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
V1 <- as.vector(coredata(X1[,1L])) * 5
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, V1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), V1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments shouldn't matter
    e <- ops_numeric_tester(V1, X1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(V1, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts1d_vector_names"
X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
V1 <- setNames(as.vector(X1[,1L]), index(X1)) * 5
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(X1, V1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(E), V1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments shouldn't matter
    e <- ops_numeric_tester(V1, X1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(V1, coredata(E), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}
### }}} xts with dim, vector

### {{{ xts no dims, matrix/vector
info_msg <- "test.ops_xts_no_dim_matrix1d"
X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
Xv <- drop(X1)
M1 <- coredata(X1) * 5
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(Xv, M1, m, o)
    E <- X1
    E[] <- ops_numeric_tester(coredata(Xv), M1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments shouldn't matter
    e <- ops_numeric_tester(M1, Xv, m, o)
    E <- X1
    E[] <- ops_numeric_tester(M1, coredata(Xv), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts_no_dim_matrix2d"
X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
Xv <- drop(X1)
X2 <- merge(x = Xv * 2, y = Xv * 5)
M2 <- coredata(X2)
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(Xv, M2, m, o)
    E <- X2
    E[] <- ops_numeric_tester(coredata(Xv), M2, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    # results no identical because attributes change order
    expect_equal(e, E, info = make_msg(info_msg, o, m))

    # order of arguments shouldn't matter
    e <- ops_numeric_tester(M2, Xv, m, o)
    E <- X2
    E[] <- ops_numeric_tester(M2, coredata(Xv), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    # results no identical because attributes change order
    expect_equal(e, E, info = make_msg(info_msg, o, m))
  }
}

info_msg <- "test.ops_xts_no_dim_vector"
X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
Xv <- drop(X1)
V1 <- 4:6
for (o in all.ops) {
  for (m in all.modes) {
    if ("character" == m && !(o %in% ops.relation))
      next

    e <- ops_numeric_tester(Xv, V1, m, o)
    E <- Xv
    E[] <- ops_numeric_tester(coredata(Xv), V1, m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))

    # order of arguments shouldn't matter
    e <- ops_numeric_tester(V1, Xv, m, o)
    E <- Xv
    E[] <- ops_numeric_tester(V1, coredata(Xv), m, o)
    if (o %in% ops.logic) storage.mode(E) <- "logical"
    expect_identical(e, E, info = make_msg(info_msg, o, m))
  }
}
### }}} xts vector, matrix/vector

### These tests check that the time class of a time series on which
### a relational operator is applied is not changed.
ts1 <- xts(17, order.by = as.Date('2020-01-29'))

info_msg <- "test.get_tclass_ts1"
expect_identical(tclass(ts1), c("Date"), info = info_msg)

info_msg <- "test.tclass_after_rel_op"
expect_identical(tclass(ts1 < 0), c("Date"), info = paste(info_msg, "| <"))
expect_identical(tclass(ts1 > 0), c("Date"), info = paste(info_msg, "| >"))
expect_identical(tclass(ts1 <= 0), c("Date"), info = paste(info_msg, "| <="))
expect_identical(tclass(ts1 >= 0), c("Date"), info = paste(info_msg, "| >="))
expect_identical(tclass(ts1 == 0), c("Date"), info = paste(info_msg, "| =="))
expect_identical(tclass(ts1 != 0), c("Date"), info = paste(info_msg, "| !="))

tstz <- "Atlantic/Reykjavik"
ts2 <- xts(17, order.by = as.POSIXct("2020-01-29", tz = tstz))

info_msg <- "test.get_tclass_POSIXct_ts2"
expect_true("POSIXct" %in% tclass(ts2), info = info_msg)

info_msg <- "test.tclass_POSIXct_after_rel_op"
expect_true("POSIXct" %in% tclass(ts2 <  0), info = paste(info_msg, "| <"))
expect_true("POSIXct" %in% tclass(ts2 >  0), info = paste(info_msg, "| >"))
expect_true("POSIXct" %in% tclass(ts2 <= 0), info = paste(info_msg, "| <="))
expect_true("POSIXct" %in% tclass(ts2 >= 0), info = paste(info_msg, "| >="))
expect_true("POSIXct" %in% tclass(ts2 == 0), info = paste(info_msg, "| =="))
expect_true("POSIXct" %in% tclass(ts2 != 0), info = paste(info_msg, "| !="))

info_msg <- "test.get_tzone_ts2"
expect_identical(tzone(ts2), tstz, info = info_msg)

info_msg <- "test.tzone_after_rel_op"
expect_identical(tzone(ts2 <  0), tstz, info = paste(info_msg, "| <"))
expect_identical(tzone(ts2 >  0), tstz, info = paste(info_msg, "| >"))
expect_identical(tzone(ts2 <= 0), tstz, info = paste(info_msg, "| <="))
expect_identical(tzone(ts2 >= 0), tstz, info = paste(info_msg, "| >="))
expect_identical(tzone(ts2 == 0), tstz, info = paste(info_msg, "| =="))
expect_identical(tzone(ts2 != 0), tstz, info = paste(info_msg, "| !="))

### Ops.xts() doesn't change column names
x <- .xts(1:3, 1:3, dimnames = list(NULL, c("-1")))
z <- as.zoo(x)
expect_equal(names(x + x[-1,]), names(z + z[-1,]),
            "Ops.xts() doesn't change column names when merge() is called")
expect_equal(names(x + x), names(z + z),
            "Ops.xts() doesn't change column names when indexes are equal")

### Ops.xts returns derived class
st <- Sys.time()
x1 <- xts(1, st)
x2 <- xts(2, st)
x3 <- xts(3, st)  # regular xts object
# derived class objects
klass <- c("foo", "xts", "zoo")
class(x1) <- klass
class(x2) <- klass
expect_identical(klass, class(x1 + x2), "Ops.xts('foo', 'foo') returns derived class")
expect_identical(klass, class(x2 + x1), "Ops.xts('foo', 'foo') returns derived class")
expect_identical(klass, class(x1 + x3), "Ops.xts('foo', 'xts') returns derived class")
expect_identical(class(x3), class(x3 + x1), "Ops.xts('xts', 'foo') returns xts class")

info_msg <- "test.Ops.xts_unary"
xpos <- xts(1, .Date(1))
xneg <- xts(-1, .Date(1))
lt <- xts(TRUE, .Date(1))
lf <- xts(FALSE, .Date(1))
expect_identical(xpos, +xpos, info = paste(info_msg, "+ positive xts"))
expect_identical(xneg, +xneg, info = paste(info_msg, "+ negative xts"))
expect_identical(xneg, -xpos, info = paste(info_msg, "- positive xts"))
expect_identical(xpos, -xneg, info = paste(info_msg, "- negative xts"))

expect_identical(lf, !lt, info = paste(info_msg, "! TRUE"))
expect_identical(lt, !lf, info = paste(info_msg, "! FALSE"))
