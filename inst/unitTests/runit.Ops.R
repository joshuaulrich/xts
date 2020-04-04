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

### {{{ 2-column objects
test.ops_xts2d_matrix2d_dimnames <- function() {

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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments should only change column names
      e <- ops_numeric_tester(M2, X1, m, o)
      E <- X1
      colnames(E) <- colnames(M2)
      E[] <- ops_numeric_tester(M2, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts2d_matrix2d_only_colnames <- function() {

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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments should only change column names
      e <- ops_numeric_tester(M2, X1, m, o)
      E <- X1
      colnames(E) <- colnames(M2)
      E[] <- ops_numeric_tester(M2, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts2d_matrix2d_only_rownames <- function() {

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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments should only change column names
      e <- ops_numeric_tester(M2, X1, m, o)
      E <- X1
      colnames(E) <- colnames(M2)
      E[] <- ops_numeric_tester(M2, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts2d_matrix2d_no_dimnames <- function() {
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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(M1, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(M1, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}
### }}} 2-column objects

### {{{ 1-column objects
test.ops_xts1d_matrix1d_dimnames <- function() {
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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments should only change column names
      e <- ops_numeric_tester(M2, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(M2, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      colnames(E) <- "y"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts1d_matrix1d_only_colnames <- function() {
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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments should only change column names
      e <- ops_numeric_tester(M2, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(M2, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      colnames(E) <- "y"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts1d_matrix1d_only_rownames <- function() {

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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(M1, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(M1, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts1d_matrix1d_no_dimnames <- function() {

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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(M1, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(M1, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts1d_xts1d <- function() {
  X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))

  for (o in all.ops) {
    for (m in all.modes) {
      if ("character" == m && !(o %in% ops.relation))
        next
      e <- ops_numeric_tester(X1, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(coredata(X1), coredata(X1), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts1d_xts1d_different_index <- function() {
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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments should only change column names
      e <- ops_numeric_tester(X2, X1, m, o)
      E <- X2[1:2,]
      E[] <- ops_numeric_tester(coredata(X1[2:3,]), coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}
### }}} 1-column objects

### {{{ xts with dim, vector
test.ops_xts2d_vector_no_names <- function() {
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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(V1, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(V1, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts2d_vector_names <- function() {
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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(V1, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(V1, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts1d_vector_no_names <- function() {
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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(V1, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(V1, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts1d_vector_names <- function() {
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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(V1, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(V1, coredata(E), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}
### }}} xts with dim, vector

### {{{ xts no dims, matrix/vector
test.ops_xts_no_dim_matrix1d <- function() {
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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(M1, Xv, m, o)
      E <- X1
      E[] <- ops_numeric_tester(M1, coredata(Xv), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts_no_dim_matrix2d <- function() {
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
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(M2, Xv, m, o)
      E <- X2
      E[] <- ops_numeric_tester(M2, coredata(Xv), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      # results no identical because attributes change order
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.ops_xts_no_dim_vector <- function() {
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
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(V1, Xv, m, o)
      E <- Xv
      E[] <- ops_numeric_tester(V1, coredata(Xv), m, o)
      if (o %in% ops.logic) storage.mode(E) <- "logical"
      checkIdentical(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}
### }}} xts vector, matrix/vector

# These tests check that the time class of a time series on which
# a relational operator is applied is not changed.

ts1 <- xts(17, order.by = as.Date('2020-01-29'))

test.get_tclass_ts1  <- function() {
  checkIdentical(tclass(ts1), c("Date"))
}

test.tclass_after_rel_op <- function() {
  checkIdentical(tclass(ts1 < 0), c("Date"))
  checkIdentical(tclass(ts1 > 0), c("Date"))
  checkIdentical(tclass(ts1 <= 0), c("Date"))
  checkIdentical(tclass(ts1 >= 0), c("Date"))
  checkIdentical(tclass(ts1 == 0), c("Date"))
  checkIdentical(tclass(ts1 != 0), c("Date"))
}

tstz <- "Atlantic/Reykjavik"
ts2 <- xts(17, order.by = as.POSIXct("2020-01-29", tz = tstz))

test.get_tclass_POSIXct_ts2  <- function() {
  checkTrue("POSIXct" %in% tclass(ts2))
}

test.tclass_POSIXct_after_rel_op <- function() {
  checkTrue("POSIXct" %in% tclass(ts2 <  0))
  checkTrue("POSIXct" %in% tclass(ts2 >  0))
  checkTrue("POSIXct" %in% tclass(ts2 <= 0))
  checkTrue("POSIXct" %in% tclass(ts2 >= 0))
  checkTrue("POSIXct" %in% tclass(ts2 == 0))
  checkTrue("POSIXct" %in% tclass(ts2 != 0))
}

test.get_tzone_ts2  <- function() {
  checkIdentical(tzone(ts2), tstz)
}

test.tzone_after_rel_op <- function() {
  checkIdentical(tzone(ts2 <  0), tstz)
  checkIdentical(tzone(ts2 >  0), tstz)
  checkIdentical(tzone(ts2 <= 0), tstz)
  checkIdentical(tzone(ts2 >= 0), tstz)
  checkIdentical(tzone(ts2 == 0), tstz)
  checkIdentical(tzone(ts2 != 0), tstz)
}
