require(xts)
require(RUnit)

numeric.modes <- c("double", "integer", "logical")#, "complex")
ops.math <- c("+", "-", "*", "/", "^", "%%", "%/%")
ops.logic <- c("&", "|", ">", ">=", "==", "!=", "<=", "<")

ops_numeric_tester <-
function(e1, e2, mode, op)
{
  storage.mode(e1) <- mode
  storage.mode(e2) <- mode
  eval(call(op, e1, e2))
}

### One-column xts objects
test.math_xts1d_xts1d <- function() {
  X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
  for (o in ops.math) {
    for (m in numeric.modes) {
      e <- ops_numeric_tester(X1, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(coredata(X1), coredata(X1), m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.math_xts1d_xts1d_different_index <- function() {
  X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
  X2 <- .xts(2:4, 2:4, dimnames = list(NULL, "y"))
  for (o in ops.math) {
    for (m in numeric.modes) {

      e <- ops_numeric_tester(X1, X2, m, o)
      E <- X1[2:3,]
      E[] <- ops_numeric_tester(coredata(E), coredata(X2[1:2,]), m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments should only change column names
      e <- ops_numeric_tester(X2, X1, m, o)
      E <- X2[1:2,]
      E[] <- ops_numeric_tester(coredata(X1[2:3,]), coredata(E), m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

#test.math_xts_xts_no_common_index <- function() {
#  X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
#  X2 <- .xts(4:6, 4:6, dimnames = list(NULL, "y"))
#  for (o in ops.math) {
#    for (m in numeric.modes) {
#      e <- ops_numeric_tester(X1, X2, m, o)
#      # only coredata should change
#      E <- .xts(NULL, numeric())
#      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))
#    }
#  }
#}

test.math_xts1d_matrix1d <- function() {
  X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
  for (o in ops.math) {
    for (m in numeric.modes) {

      C1 <- coredata(X1)
      e <- ops_numeric_tester(X1, C1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(coredata(E), C1, m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments should only change column names
      C2 <- C1
      colnames(C2) <- "y"
      e <- ops_numeric_tester(C2, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(C2, coredata(E), m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.math_xts1d_vector <- function() {
  X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
  for (o in ops.math) {
    for (m in numeric.modes) {

      V1 <- as.vector(coredata(X1[,1L]))
      e <- ops_numeric_tester(X1, V1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(coredata(E), V1, m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(V1, X1, m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

### Two-column xts objects
test.math_xts2d_matrix2d_dimnames <- function() {
  X1 <- .xts(cbind(1:3, 1:3), 1:3, dimnames = list(NULL, c("x", "y")))
  for (o in ops.math) {
    for (m in numeric.modes) {

      C1 <- coredata(X1)
      e <- ops_numeric_tester(X1, C1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(coredata(E), C1, m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments should only change column names
      C2 <- C1
      colnames(C2) <- rev(colnames(C2))
      e <- ops_numeric_tester(C2, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(C2, coredata(E), m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.math_xts2d_matrix2d_no_dimnames <- function() {
  X1 <- .xts(cbind(1:3, 1:3), 1:3)
  for (o in ops.math) {
    for (m in numeric.modes) {

      C1 <- coredata(X1)
      e <- ops_numeric_tester(X1, C1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(coredata(E), C1, m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments should only change column names
      C2 <- C1
      colnames(C2) <- rev(colnames(C2))
      e <- ops_numeric_tester(C2, X1, m, o)
      E <- X1
      E[] <- ops_numeric_tester(C2, coredata(E), m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

### xts objects with no dims
test.math_xts.vector_matrix1d <- function() {
  X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
  for (o in ops.math) {
    for (m in numeric.modes) {

      C1 <- coredata(X1)
      e <- ops_numeric_tester(drop(X1), C1, m, o)
      E <- drop(X1)
      E[] <- ops_numeric_tester(coredata(E), C1, m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(C1, drop(X1), m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}

test.math_xts.vector_matrix2d <- function() {
  X1 <- .xts(1:3, 1:3, dimnames = list(NULL, "x"))
  C1 <- coredata(merge(x = drop(X1), y = drop(X1)))
  for (o in ops.math) {
    for (m in numeric.modes) {

      e <- ops_numeric_tester(drop(X1), C1, m, o)
      E <- drop(X1)
      E[] <- ops_numeric_tester(coredata(E), C1, m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))

      # order of arguments shouldn't matter
      e <- ops_numeric_tester(C1, drop(X1), m, o)
      checkEquals(e, E, sprintf("op: %s, type: %s", o, m))
    }
  }
}
